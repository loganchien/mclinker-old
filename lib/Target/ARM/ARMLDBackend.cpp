//===- ARMLDBackend.cpp ---------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "ARM.h"
#include "ARMGNUInfo.h"
#include "ARMELFAttributeData.h"
#include "ARMELFDynamic.h"
#include "ARMExData.h"
#include "ARMLDBackend.h"
#include "ARMNameToExDataMap.h"
#include "ARMRelocator.h"
#include "ARMToARMStub.h"
#include "ARMToTHMStub.h"
#include "THMToTHMStub.h"
#include "THMToARMStub.h"

#include <mcld/IRBuilder.h>
#include <mcld/LinkerConfig.h>
#include <mcld/Fragment/AlignFragment.h>
#include <mcld/Fragment/FillFragment.h>
#include <mcld/Fragment/NullFragment.h>
#include <mcld/Fragment/RegionFragment.h>
#include <mcld/Fragment/Stub.h>
#include <mcld/LD/BranchIslandFactory.h>
#include <mcld/LD/LDContext.h>
#include <mcld/LD/ELFFileFormat.h>
#include <mcld/LD/ELFSegmentFactory.h>
#include <mcld/LD/ELFSegment.h>
#include <mcld/LD/StubFactory.h>
#include <mcld/Object/ObjectBuilder.h>
#include <mcld/Support/MemoryArea.h>
#include <mcld/Support/MemoryRegion.h>
#include <mcld/Support/MsgHandling.h>
#include <mcld/Support/TargetRegistry.h>
#include <mcld/Target/ELFAttribute.h>
#include <mcld/Target/GNUInfo.h>

#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/Triple.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/ELF.h>
#include <llvm/Support/Format.h>
#include <llvm/Support/raw_ostream.h>

#include <algorithm>
#include <vector>

#include <cstring>

using namespace mcld;

//===----------------------------------------------------------------------===//
// ARMGNULDBackend
//===----------------------------------------------------------------------===//
ARMGNULDBackend::ARMGNULDBackend(const LinkerConfig& pConfig, GNUInfo* pInfo)
    : GNULDBackend(pConfig, pInfo),
      m_pRelocator(NULL),
      m_pGOT(NULL),
      m_pPLT(NULL),
      m_pRelDyn(NULL),
      m_pRelPLT(NULL),
      m_pAttrData(NULL),
      m_pDynamic(NULL),
      m_pGOTSymbol(NULL),
      m_pEXIDXStart(NULL),
      m_pEXIDXEnd(NULL),
      m_pEXIDX(NULL),
      m_pEXTAB(NULL),
      m_pAttributes(NULL) {
}

ARMGNULDBackend::~ARMGNULDBackend() {
  delete m_pRelocator;
  delete m_pGOT;
  delete m_pPLT;
  delete m_pRelDyn;
  delete m_pRelPLT;
  delete m_pDynamic;
  delete m_pAttrData;
  clearInputExDataMaps();
}

void ARMGNULDBackend::initTargetSections(Module& pModule,
                                         ObjectBuilder& pBuilder) {
  // FIXME: Currently we set exidx and extab to "Exception" and directly emit
  // them from input
  m_pEXIDX =
      pBuilder.CreateSection(".ARM.exidx",
                             LDFileFormat::Target,
                             llvm::ELF::SHT_ARM_EXIDX,
                             llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_LINK_ORDER,
                             config().targets().bitclass() / 8);
  m_pEXTAB = pBuilder.CreateSection(".ARM.extab",
                                    LDFileFormat::Target,
                                    llvm::ELF::SHT_PROGBITS,
                                    llvm::ELF::SHF_ALLOC,
                                    0x1);
  m_pAttributes = pBuilder.CreateSection(".ARM.attributes",
                                         LDFileFormat::Target,
                                         llvm::ELF::SHT_ARM_ATTRIBUTES,
                                         0x0,
                                         0x1);

  // initialize "aeabi" attributes subsection
  m_pAttrData = new ARMELFAttributeData();
  attribute().registerAttributeData(*m_pAttrData);

  if (LinkerConfig::Object != config().codeGenType()) {
    ELFFileFormat* file_format = getOutputFormat();

    // initialize .got
    LDSection& got = file_format->getGOT();
    m_pGOT = new ARMGOT(got);

    // initialize .plt
    LDSection& plt = file_format->getPLT();
    m_pPLT = new ARMPLT(plt, *m_pGOT);

    // initialize .rel.plt
    LDSection& relplt = file_format->getRelPlt();
    relplt.setLink(&plt);
    // create SectionData and ARMRelDynSection
    m_pRelPLT = new OutputRelocSection(pModule, relplt);

    // initialize .rel.dyn
    LDSection& reldyn = file_format->getRelDyn();
    m_pRelDyn = new OutputRelocSection(pModule, reldyn);
  }
}

void ARMGNULDBackend::initTargetSymbols(IRBuilder& pBuilder, Module& pModule) {
  // Define the symbol _GLOBAL_OFFSET_TABLE_ if there is a symbol with the
  // same name in input
  if (LinkerConfig::Object != config().codeGenType()) {
    m_pGOTSymbol =
        pBuilder.AddSymbol<IRBuilder::AsReferred, IRBuilder::Resolve>(
            "_GLOBAL_OFFSET_TABLE_",
            ResolveInfo::Object,
            ResolveInfo::Define,
            ResolveInfo::Local,
            0x0,  // size
            0x0,  // value
            FragmentRef::Null(),
            ResolveInfo::Hidden);
  }
  if (m_pEXIDX != NULL && m_pEXIDX->size() != 0x0) {
    FragmentRef* exidx_start =
        FragmentRef::Create(m_pEXIDX->getSectionData()->front(), 0x0);
    FragmentRef* exidx_end = FragmentRef::Create(
        m_pEXIDX->getSectionData()->front(), m_pEXIDX->size());
    m_pEXIDXStart =
        pBuilder.AddSymbol<IRBuilder::AsReferred, IRBuilder::Resolve>(
            "__exidx_start",
            ResolveInfo::Object,
            ResolveInfo::Define,
            ResolveInfo::Local,
            0x0,          // size
            0x0,          // value
            exidx_start,  // FragRef
            ResolveInfo::Default);

    m_pEXIDXEnd = pBuilder.AddSymbol<IRBuilder::AsReferred, IRBuilder::Resolve>(
        "__exidx_end",
        ResolveInfo::Object,
        ResolveInfo::Define,
        ResolveInfo::Local,
        0x0,        // size
        0x0,        // value
        exidx_end,  // FragRef
        ResolveInfo::Default);
    // change __exidx_start/_end to local dynamic category
    if (m_pEXIDXStart != NULL)
      pModule.getSymbolTable().changeToDynamic(*m_pEXIDXStart);
    if (m_pEXIDXEnd != NULL)
      pModule.getSymbolTable().changeToDynamic(*m_pEXIDXEnd);
  } else {
    m_pEXIDXStart =
        pBuilder.AddSymbol<IRBuilder::AsReferred, IRBuilder::Resolve>(
            "__exidx_start",
            ResolveInfo::NoType,
            ResolveInfo::Define,
            ResolveInfo::Absolute,
            0x0,  // size
            0x0,  // value
            FragmentRef::Null(),
            ResolveInfo::Default);

    m_pEXIDXEnd = pBuilder.AddSymbol<IRBuilder::AsReferred, IRBuilder::Resolve>(
        "__exidx_end",
        ResolveInfo::NoType,
        ResolveInfo::Define,
        ResolveInfo::Absolute,
        0x0,  // size
        0x0,  // value
        FragmentRef::Null(),
        ResolveInfo::Default);
  }
}

bool ARMGNULDBackend::initRelocator() {
  if (m_pRelocator == NULL) {
    m_pRelocator = new ARMRelocator(*this, config());
  }
  return true;
}

const Relocator* ARMGNULDBackend::getRelocator() const {
  assert(m_pRelocator != NULL);
  return m_pRelocator;
}

Relocator* ARMGNULDBackend::getRelocator() {
  assert(m_pRelocator != NULL);
  return m_pRelocator;
}

void ARMGNULDBackend::doPreLayout(IRBuilder& pBuilder) {
  // initialize .dynamic data
  if (!config().isCodeStatic() && m_pDynamic == NULL)
    m_pDynamic = new ARMELFDynamic(*this, config());

  // set attribute section size
  m_pAttributes->setSize(attribute().sizeOutput());

  // set .got size
  // when building shared object, the .got section is must
  if (LinkerConfig::Object != config().codeGenType()) {
    if (LinkerConfig::DynObj == config().codeGenType() || m_pGOT->hasGOT1() ||
        m_pGOTSymbol != NULL) {
      m_pGOT->finalizeSectionSize();
      defineGOTSymbol(pBuilder);
    }

    // set .plt size
    if (m_pPLT->hasPLT1())
      m_pPLT->finalizeSectionSize();

    ELFFileFormat* file_format = getOutputFormat();
    // set .rel.dyn size
    if (!m_pRelDyn->empty()) {
      assert(
          !config().isCodeStatic() &&
          "static linkage should not result in a dynamic relocation section");
      file_format->getRelDyn().setSize(m_pRelDyn->numOfRelocs() *
                                       getRelEntrySize());
    }

    // set .rel.plt size
    if (!m_pRelPLT->empty()) {
      assert(
          !config().isCodeStatic() &&
          "static linkage should not result in a dynamic relocation section");
      file_format->getRelPlt().setSize(m_pRelPLT->numOfRelocs() *
                                       getRelEntrySize());
    }
  }
}

void ARMGNULDBackend::doPostLayout(Module& pModule, IRBuilder& pBuilder) {
  const ELFFileFormat* file_format = getOutputFormat();

  // apply PLT
  if (file_format->hasPLT()) {
    // Since we already have the size of LDSection PLT, m_pPLT should not be
    // NULL.
    assert(m_pPLT != NULL);
    m_pPLT->applyPLT0();
    m_pPLT->applyPLT1();
  }

  // apply GOT
  if (file_format->hasGOT()) {
    // Since we already have the size of GOT, m_pGOT should not be NULL.
    assert(m_pGOT != NULL);
    if (LinkerConfig::DynObj == config().codeGenType())
      m_pGOT->applyGOT0(file_format->getDynamic().addr());
    else {
      // executable file and object file? should fill with zero.
      m_pGOT->applyGOT0(0);
    }
  }
}

/// dynamic - the dynamic section of the target machine.
/// Use co-variant return type to return its own dynamic section.
ARMELFDynamic& ARMGNULDBackend::dynamic() {
  assert(m_pDynamic != NULL);
  return *m_pDynamic;
}

/// dynamic - the dynamic section of the target machine.
/// Use co-variant return type to return its own dynamic section.
const ARMELFDynamic& ARMGNULDBackend::dynamic() const {
  assert(m_pDynamic != NULL);
  return *m_pDynamic;
}

void ARMGNULDBackend::defineGOTSymbol(IRBuilder& pBuilder) {
  // define symbol _GLOBAL_OFFSET_TABLE_ when .got create
  if (m_pGOTSymbol != NULL) {
    pBuilder.AddSymbol<IRBuilder::Force, IRBuilder::Unresolve>(
        "_GLOBAL_OFFSET_TABLE_",
        ResolveInfo::Object,
        ResolveInfo::Define,
        ResolveInfo::Local,
        0x0,  // size
        0x0,  // value
        FragmentRef::Create(*(m_pGOT->begin()), 0x0),
        ResolveInfo::Hidden);
  } else {
    m_pGOTSymbol = pBuilder.AddSymbol<IRBuilder::Force, IRBuilder::Resolve>(
        "_GLOBAL_OFFSET_TABLE_",
        ResolveInfo::Object,
        ResolveInfo::Define,
        ResolveInfo::Local,
        0x0,  // size
        0x0,  // value
        FragmentRef::Create(*(m_pGOT->begin()), 0x0),
        ResolveInfo::Hidden);
  }
}

uint64_t ARMGNULDBackend::emitSectionData(const LDSection& pSection,
                                          MemoryRegion& pRegion) const {
  assert(pRegion.size() && "Size of MemoryRegion is zero!");

  const ELFFileFormat* file_format = getOutputFormat();

  if (file_format->hasPLT() && (&pSection == &(file_format->getPLT()))) {
    uint64_t result = m_pPLT->emit(pRegion);
    return result;
  }

  if (file_format->hasGOT() && (&pSection == &(file_format->getGOT()))) {
    uint64_t result = m_pGOT->emit(pRegion);
    return result;
  }

  if (&pSection == m_pAttributes) {
    return attribute().emit(pRegion);
  }

  // FIXME: Currently Emitting .ARM.attributes, .ARM.exidx, and .ARM.extab
  // directly from the input file.
  const SectionData* sect_data = pSection.getSectionData();
  SectionData::const_iterator frag_iter, frag_end = sect_data->end();
  uint8_t* out_offset = pRegion.begin();
  for (frag_iter = sect_data->begin(); frag_iter != frag_end; ++frag_iter) {
    size_t size = frag_iter->size();
    switch (frag_iter->getKind()) {
      case Fragment::Fillment: {
        const FillFragment& fill_frag = llvm::cast<FillFragment>(*frag_iter);
        if (fill_frag.getValueSize() == 0) {
          // virtual fillment, ignore it.
          break;
        }

        memset(out_offset, fill_frag.getValue(), fill_frag.size());
        break;
      }
      case Fragment::Region: {
        const RegionFragment& region_frag =
            llvm::cast<RegionFragment>(*frag_iter);
        const char* start = region_frag.getRegion().begin();
        memcpy(out_offset, start, size);
        break;
      }
      case Fragment::Alignment: {
        const AlignFragment& align_frag = llvm::cast<AlignFragment>(*frag_iter);
        uint64_t count = size / align_frag.getValueSize();
        switch (align_frag.getValueSize()) {
          case 1u:
            std::memset(out_offset, align_frag.getValue(), count);
            break;
          default:
            llvm::report_fatal_error(
                "unsupported value size for align fragment emission yet.\n");
            break;
        }  // end switch
        break;
      }
      case Fragment::Null: {
        assert(0x0 == size);
        break;
      }
      default:
        llvm::report_fatal_error("unsupported fragment type.\n");
        break;
    }  // end switch
    out_offset += size;
  }  // end for
  return pRegion.size();
}

/// finalizeSymbol - finalize the symbol value
bool ARMGNULDBackend::finalizeTargetSymbols() {
  return true;
}

void ARMGNULDBackend::preMergeSections(Module& pModule)
{
  buildInputExDataMaps(pModule);
}

bool ARMGNULDBackend::mergeSection(Module& pModule,
                                   const Input& pInput,
                                   LDSection& pSection) {
  switch (pSection.type()) {
    case llvm::ELF::SHT_ARM_ATTRIBUTES: {
      return attribute().merge(pInput, pSection);
    }
    case llvm::ELF::SHT_ARM_EXIDX: {
      assert(pSection.getLink() != NULL);
      if ((pSection.getLink()->kind() == LDFileFormat::Ignore) ||
          (pSection.getLink()->kind() == LDFileFormat::Folded)) {
        // if the target section of the .ARM.exidx is Ignore, then it should be
        // ignored as well
        pSection.setKind(LDFileFormat::Ignore);
        return true;
      }
      InputToExDataMapMap::iterator exDataMapIt =
          m_InputToExDataMapMap.find(const_cast<Input*>(&pInput));
      if (exDataMapIt != m_InputToExDataMapMap.end()) {
        ARMNameToExDataMap* exDataMap = exDataMapIt->second;
        if (ARMExData* exData = exDataMap->getByExSection(pSection.name())) {
          if (exData->exTab()) {
            ObjectBuilder builder(pModule);
            builder.MergeSection(pInput, *exData->exTab());
          }
        }
      }
    }
    /** fall through **/
    default: {
      ObjectBuilder builder(pModule);
      builder.MergeSection(pInput, pSection);
      return true;
    }
  }  // end of switch
  return true;
}

void ARMGNULDBackend::setUpReachedSectionsForGC(
    const Module& pModule,
    GarbageCollection::SectionReachedListMap& pSectReachedListMap) const {
  // traverse all the input relocations to find the relocation sections applying
  // .ARM.exidx sections
  Module::const_obj_iterator input, inEnd = pModule.obj_end();
  for (input = pModule.obj_begin(); input != inEnd; ++input) {
    LDContext::const_sect_iterator rs,
        rsEnd = (*input)->context()->relocSectEnd();
    for (rs = (*input)->context()->relocSectBegin(); rs != rsEnd; ++rs) {
      // bypass the discarded relocation section
      // 1. its section kind is changed to Ignore. (The target section is a
      // discarded group section.)
      // 2. it has no reloc data. (All symbols in the input relocs are in the
      // discarded group sections)
      LDSection* reloc_sect = *rs;
      LDSection* apply_sect = reloc_sect->getLink();
      if ((LDFileFormat::Ignore == reloc_sect->kind()) ||
          (!reloc_sect->hasRelocData()))
        continue;

      if (llvm::ELF::SHT_ARM_EXIDX == apply_sect->type()) {
        // 1. set up the reference according to relocations
        bool add_first = false;
        GarbageCollection::SectionListTy* reached_sects = NULL;
        RelocData::iterator reloc_it, rEnd = reloc_sect->getRelocData()->end();
        for (reloc_it = reloc_sect->getRelocData()->begin(); reloc_it != rEnd;
             ++reloc_it) {
          Relocation* reloc = llvm::cast<Relocation>(reloc_it);
          ResolveInfo* sym = reloc->symInfo();
          // only the target symbols defined in the input fragments can make the
          // reference
          if (sym == NULL)
            continue;
          if (!sym->isDefine() || !sym->outSymbol()->hasFragRef())
            continue;

          // only the target symbols defined in the concerned sections can make
          // the reference
          const LDSection* target_sect =
              &sym->outSymbol()->fragRef()->frag()->getParent()->getSection();
          if (target_sect->kind() != LDFileFormat::TEXT &&
              target_sect->kind() != LDFileFormat::DATA &&
              target_sect->kind() != LDFileFormat::BSS)
            continue;

          // setup the reached list, if we first add the element to reached list
          // of this section, create an entry in ReachedSections map
          if (!add_first) {
            reached_sects = &pSectReachedListMap.getReachedList(*apply_sect);
            add_first = true;
          }
          reached_sects->insert(target_sect);
        }
        reached_sects = NULL;
        add_first = false;
        // 2. set up the reference from XXX to .ARM.exidx.XXX
        assert(apply_sect->getLink() != NULL);
        pSectReachedListMap.addReference(*apply_sect->getLink(), *apply_sect);
      }
    }
  }
}

bool ARMGNULDBackend::readSection(Input& pInput, SectionData& pSD) {
  Fragment* frag = NULL;
  uint32_t offset = pInput.fileOffset() + pSD.getSection().offset();
  uint32_t size = pSD.getSection().size();

  llvm::StringRef region = pInput.memArea()->request(offset, size);
  if (region.size() == 0) {
    // If the input section's size is zero, we got a NULL region.
    // use a virtual fill fragment
    frag = new FillFragment(0x0, 0, 0);
  } else {
    frag = new RegionFragment(region);
  }

  ObjectBuilder::AppendFragment(*frag, pSD);
  return true;
}

ARMGOT& ARMGNULDBackend::getGOT() {
  assert(m_pGOT != NULL && "GOT section not exist");
  return *m_pGOT;
}

const ARMGOT& ARMGNULDBackend::getGOT() const {
  assert(m_pGOT != NULL && "GOT section not exist");
  return *m_pGOT;
}

ARMPLT& ARMGNULDBackend::getPLT() {
  assert(m_pPLT != NULL && "PLT section not exist");
  return *m_pPLT;
}

const ARMPLT& ARMGNULDBackend::getPLT() const {
  assert(m_pPLT != NULL && "PLT section not exist");
  return *m_pPLT;
}

OutputRelocSection& ARMGNULDBackend::getRelDyn() {
  assert(m_pRelDyn != NULL && ".rel.dyn section not exist");
  return *m_pRelDyn;
}

const OutputRelocSection& ARMGNULDBackend::getRelDyn() const {
  assert(m_pRelDyn != NULL && ".rel.dyn section not exist");
  return *m_pRelDyn;
}

OutputRelocSection& ARMGNULDBackend::getRelPLT() {
  assert(m_pRelPLT != NULL && ".rel.plt section not exist");
  return *m_pRelPLT;
}

const OutputRelocSection& ARMGNULDBackend::getRelPLT() const {
  assert(m_pRelPLT != NULL && ".rel.plt section not exist");
  return *m_pRelPLT;
}

ARMELFAttributeData& ARMGNULDBackend::getAttributeData() {
  assert(m_pAttrData != NULL && ".ARM.attributes section not exist");
  return *m_pAttrData;
}

const ARMELFAttributeData& ARMGNULDBackend::getAttributeData() const {
  assert(m_pAttrData != NULL && ".ARM.attributes section not exist");
  return *m_pAttrData;
}

unsigned int ARMGNULDBackend::getTargetSectionOrder(
    const LDSection& pSectHdr) const {
  const ELFFileFormat* file_format = getOutputFormat();

  if (file_format->hasGOT() && (&pSectHdr == &file_format->getGOT())) {
    if (config().options().hasNow())
      return SHO_RELRO_LAST;
    return SHO_DATA;
  }

  if (file_format->hasPLT() && (&pSectHdr == &file_format->getPLT()))
    return SHO_PLT;

  if (&pSectHdr == m_pEXIDX || &pSectHdr == m_pEXTAB) {
    // put ARM.exidx and ARM.extab in the same order of .eh_frame
    return SHO_EXCEPTION;
  }

  return SHO_UNDEFINED;
}

/// doRelax
bool ARMGNULDBackend::doRelax(Module& pModule,
                              IRBuilder& pBuilder,
                              bool& pFinished) {
  assert(getStubFactory() != NULL && getBRIslandFactory() != NULL);

  bool isRelaxed = false;
  ELFFileFormat* file_format = getOutputFormat();
  // check branch relocs and create the related stubs if needed
  Module::obj_iterator input, inEnd = pModule.obj_end();
  for (input = pModule.obj_begin(); input != inEnd; ++input) {
    LDContext::sect_iterator rs, rsEnd = (*input)->context()->relocSectEnd();
    for (rs = (*input)->context()->relocSectBegin(); rs != rsEnd; ++rs) {
      if (LDFileFormat::Ignore == (*rs)->kind() || !(*rs)->hasRelocData())
        continue;
      RelocData::iterator reloc, rEnd = (*rs)->getRelocData()->end();
      for (reloc = (*rs)->getRelocData()->begin(); reloc != rEnd; ++reloc) {
        Relocation* relocation = llvm::cast<Relocation>(reloc);

        switch (relocation->type()) {
          case llvm::ELF::R_ARM_PC24:
          case llvm::ELF::R_ARM_CALL:
          case llvm::ELF::R_ARM_JUMP24:
          case llvm::ELF::R_ARM_PLT32:
          case llvm::ELF::R_ARM_THM_CALL:
          case llvm::ELF::R_ARM_THM_XPC22:
          case llvm::ELF::R_ARM_THM_JUMP24:
          case llvm::ELF::R_ARM_THM_JUMP19: {
            // calculate the possible symbol value
            uint64_t sym_value = 0x0;
            LDSymbol* symbol = relocation->symInfo()->outSymbol();
            if (symbol->hasFragRef()) {
              uint64_t value = symbol->fragRef()->getOutputOffset();
              uint64_t addr =
                  symbol->fragRef()->frag()->getParent()->getSection().addr();
              sym_value = addr + value;
            }
            if ((relocation->symInfo()->reserved() &
                 ARMRelocator::ReservePLT) != 0x0) {
              // FIXME: we need to find out the address of the specific plt
              // entry
              assert(file_format->hasPLT());
              sym_value = file_format->getPLT().addr();
            }
            Stub* stub = getStubFactory()->create(*relocation,  // relocation
                                                  sym_value,    // symbol value
                                                  pBuilder,
                                                  *getBRIslandFactory());
            if (stub != NULL) {
              switch (config().options().getStripSymbolMode()) {
                case GeneralOptions::StripAllSymbols:
                case GeneralOptions::StripLocals:
                  break;
                default: {
                  // a stub symbol should be local
                  assert(stub->symInfo() != NULL && stub->symInfo()->isLocal());
                  LDSection& symtab = file_format->getSymTab();
                  LDSection& strtab = file_format->getStrTab();

                  // increase the size of .symtab and .strtab if needed
                  if (config().targets().is32Bits())
                    symtab.setSize(symtab.size() +
                                   sizeof(llvm::ELF::Elf32_Sym));
                  else
                    symtab.setSize(symtab.size() +
                                   sizeof(llvm::ELF::Elf64_Sym));
                  symtab.setInfo(symtab.getInfo() + 1);
                  strtab.setSize(strtab.size() + stub->symInfo()->nameSize() +
                                 1);
                }
              }  // end of switch
              isRelaxed = true;
            }
            break;
          }
          case llvm::ELF::R_ARM_V4BX:
            /* FIXME: bypass R_ARM_V4BX relocation now */
            break;
          default:
            break;
        }  // end of switch

      }  // for all relocations
    }    // for all relocation section
  }      // for all inputs

  // find the first fragment w/ invalid offset due to stub insertion
  Fragment* invalid = NULL;
  pFinished = true;
  for (BranchIslandFactory::iterator island = getBRIslandFactory()->begin(),
                                     island_end = getBRIslandFactory()->end();
       island != island_end;
       ++island) {
    if ((*island).end() == file_format->getText().getSectionData()->end())
      break;

    Fragment* exit = (*island).end();
    if (((*island).offset() + (*island).size()) > exit->getOffset()) {
      invalid = exit;
      pFinished = false;
      break;
    }
  }

  // reset the offset of invalid fragments
  while (invalid != NULL) {
    invalid->setOffset(invalid->getPrevNode()->getOffset() +
                       invalid->getPrevNode()->size());
    invalid = invalid->getNextNode();
  }

  // reset the size of .text
  if (isRelaxed) {
    file_format->getText().setSize(
        file_format->getText().getSectionData()->back().getOffset() +
        file_format->getText().getSectionData()->back().size());
  }
  return isRelaxed;
}

/// initTargetStubs
bool ARMGNULDBackend::initTargetStubs() {
  if (getStubFactory() != NULL) {
    getStubFactory()->addPrototype(new ARMToARMStub(config().isCodeIndep()));
    getStubFactory()->addPrototype(new ARMToTHMStub(config().isCodeIndep()));
    getStubFactory()->addPrototype(
        new THMToTHMStub(config().isCodeIndep(), m_pAttrData->usingThumb2()));
    getStubFactory()->addPrototype(
        new THMToARMStub(config().isCodeIndep(), m_pAttrData->usingThumb2()));
    return true;
  }
  return false;
}

/// maxFwdBranchOffset
int64_t ARMGNULDBackend::maxFwdBranchOffset() {
  if (m_pAttrData->usingThumb2()) {
    return THM2_MAX_FWD_BRANCH_OFFSET;
  } else {
    return THM_MAX_FWD_BRANCH_OFFSET;
  }
}

/// maxBwdBranchOffset
int64_t ARMGNULDBackend::maxBwdBranchOffset() {
  if (m_pAttrData->usingThumb2()) {
    return THM2_MAX_BWD_BRANCH_OFFSET;
  } else {
    return THM_MAX_BWD_BRANCH_OFFSET;
  }
}

/// doCreateProgramHdrs - backend can implement this function to create the
/// target-dependent segments
void ARMGNULDBackend::doCreateProgramHdrs(Module& pModule) {
  if (m_pEXIDX != NULL && m_pEXIDX->size() != 0x0) {
    // make PT_ARM_EXIDX
    ELFSegment* exidx_seg =
        elfSegmentTable().produce(llvm::ELF::PT_ARM_EXIDX, llvm::ELF::PF_R);
    exidx_seg->append(m_pEXIDX);
  }
}

/// mayHaveUnsafeFunctionPointerAccess - check if the section may have unsafe
/// function pointer access
bool ARMGNULDBackend::mayHaveUnsafeFunctionPointerAccess(
    const LDSection& pSection) const {
  llvm::StringRef name(pSection.name());
  return !name.startswith(".ARM.exidx") && !name.startswith(".ARM.extab") &&
         GNULDBackend::mayHaveUnsafeFunctionPointerAccess(pSection);
}

/// clearInputExDataMaps - delete the exception section maps for each input
/// object files.
void ARMGNULDBackend::clearInputExDataMaps()
{
  // Delete the ARMNameToExDataMap of each input object file.
  for (InputToExDataMapMap::iterator it = m_InputToExDataMapMap.begin(),
                                     end = m_InputToExDataMapMap.end();
       it != end; ++it) {
    delete it->second;
  }

  // Clear the map from Input to ARMNameToExDataMap.
  m_InputToExDataMapMap.clear();
}

/// getOrCreateInputExDataMap - get the ARMNameToExDataMap entry for a input
/// object file.
ARMNameToExDataMap* ARMGNULDBackend::getOrCreateInputExDataMap(Input& pInput)
{
  ARMNameToExDataMap*& result = m_InputToExDataMapMap[&pInput];
  if (NULL == result) {
    result = new ARMNameToExDataMap();
  }
  return result;
}

/// buildInputExDataMaps - build the exception section maps for each input
/// object files.
void ARMGNULDBackend::buildInputExDataMaps(Module& pModule)
{
  for (Module::const_obj_iterator inputIt = pModule.obj_begin(),
                                  inputEnd = pModule.obj_end();
       inputIt != inputEnd; ++inputIt) {
    Input* input = *inputIt;
    ARMNameToExDataMap* exDataMap = getOrCreateInputExDataMap(*input);

    // Build the ARMNameToExDataMap for this input object file.
    buildInputExDataMap(*input, *exDataMap);

    // Check whether we can rewrite the ARMExData in this ARMNameToExDataMap.
    checkExDataRewritable(*input, *exDataMap);

    // Build the ARMExEntry for all rewritable ARMExData.
    buildExEntries(*exDataMap);

    // Ignore the .ARM.extab section if rewritable.  We will re-create regions
    // later in object merging stage.
    for (ARMNameToExDataMap::iterator it = exDataMap->begin(),
                                      end = exDataMap->end(); it != end; ++it) {
      ARMExData* exData = it->second;
      if (exData->isRewritable() && NULL != exData->exTab()) {
        exData->exTab()->setKind(LDFileFormat::Ignore);
      }
    }

#if defined(DEBUG_EX_OPT)
    // Print the eligible exception handling sections.
    for (ARMNameToExDataMap::iterator it = exDataMap->begin(),
                                      end = exDataMap->end(); it != end; ++it) {
      ARMExData* exData = it->second;
      if (exData->isRewritable()) {
        llvm::errs() << "- name: \"" << it->first << "\" ELIGIBLE\n";
      } else {
        llvm::errs() << "- name: \"" << it->first << "\" NOT REWRITABLE\n";
      }

      llvm::errs() << "  - .ARM.exidx: " << exData->exIdx() << "\n";
      llvm::errs() << "  - .ARM.extab: " << exData->exTab() << "\n";
      llvm::errs() << "  - .rel.ARM.exidx: " << exData->relExIdx() << "\n";
      llvm::errs() << "  - .rel.ARM.extab: " << exData->relExTab() << "\n";

      if (exData->begin() != exData->end()) {
        llvm::errs() << "  - ENTRIES:\n";
        for (ARMExData::iterator it = exData->begin(), end = exData->end();
             it != end; ++it) {
          const llvm::StringRef region = it->getRegion();
          size_t size = region.size();
          const char* data = region.data();

          llvm::errs() << "    - entry: [";
          llvm::errs() << " begin: " << it->getInputOffset();
          llvm::errs() << " size: " << size;
          llvm::errs() << " data: " << (const void*)data;
          llvm::errs() << " ]\n";

          size_t pos = 0;
          for (size_t i = 0; i < (size + 15) / 16; ++i) {
            llvm::errs() << "     ";
            for (size_t j = 0; j < 16 && pos < size; ++j, ++pos) {
              llvm::errs() << " " << llvm::format("%02X", (uint8_t)data[pos]);
            }
            llvm::errs() << "\n";
          }
        }
      }
    }
#endif
  }
}

/// buildInputExDataMap - build the exception section maps for a particular
/// input object file.
void ARMGNULDBackend::buildInputExDataMap(Input& pInput,
                                          ARMNameToExDataMap& pExDataMap)
{
  LDContext* inputCtx = pInput.context();
  for (LDContext::const_sect_iterator sectIt = inputCtx->sectBegin(),
                                      sectEnd = inputCtx->sectEnd();
       sectIt != sectEnd; ++sectIt) {
    LDSection* sect = *sectIt;
    llvm::StringRef sectName(sect->name());

    if (sectName.startswith(".ARM.exidx")) {
      ARMExData* exData = pExDataMap.getOrCreateByExSection(sectName);
      exData->setExIdx(sect);
    } else if (sectName.startswith(".ARM.extab")) {
      ARMExData* exData = pExDataMap.getOrCreateByExSection(sectName);
      exData->setExTab(sect);
    } else if (sectName.startswith(".rel.ARM.exidx")) {
      ARMExData* exData = pExDataMap.getOrCreateByRelExSection(sectName);
      exData->setRelExIdx(sect);
    } else if (sectName.startswith(".rel.ARM.extab")) {
      ARMExData* exData = pExDataMap.getOrCreateByRelExSection(sectName);
      exData->setRelExTab(sect);
    }
  }
}

static const Fragment* checkAndFindRegionFragment(LDSection& pSection)
{
  const Fragment* regionFrag = NULL;

  const SectionData* sectData = pSection.getSectionData();
  for (SectionData::const_iterator it = sectData->begin(),
                                   end = sectData->end(); it != end; ++it) {
    switch (it->getKind()) {
    case Fragment::Alignment:
    case Fragment::Null:
      // Expected.  Check next fragment.
      break;

    case Fragment::Fillment:
    case Fragment::Target:
    case Fragment::Stub:
      return NULL;

    case Fragment::Region:
      if (regionFrag == NULL) {
        regionFrag = it;
      } else {
        return NULL;
      }
      break;
    }
  }
  return regionFrag;
}

/// checkExDataRewritable - check whether the ARMExData in the
/// ARMNameToExDataMap is rewritable or not.
void ARMGNULDBackend::checkExDataRewritable(Input& pInput,
                                            ARMNameToExDataMap& pExDataMap) {
  // To reduce the possible incorrect optimization, we are only rewriting the
  // exception handling sections if the constraints are satisfied:
  //
  //   1. The ARMExData has both .ARM.exidx and .rel.ARM.exidx sections.
  //
  //   2. The linked section of .ARM.exidx is not ignored.
  //
  //   3. The .ARM.exidx section should not have non-local symbol, and the
  //      local symbols of .ARM.exidx can only appears at offset 0.
  //
  //   4. The .ARM.extab section should not have non-local symbol, and the
  //      local symbols of .ARM.extab can only appears at offset 0.
  //
  //   5. The .ARM.exidx is only referencing its corresponding .ARM.extab in
  //      its even words.
  //
  //   6. The .ARM.extab is only referenced by the even words of the
  //      corresponding .ARM.exidx section.
  //
  // AFAIK, the existing assemblers and compilers will satisfy these
  // constraints without any problem.

  LDContext* inputCtx = pInput.context();

  // Check the section constraints.
  for (ARMNameToExDataMap::iterator it = pExDataMap.begin(),
                                    end = pExDataMap.end(); it != end; ++it) {
    ARMExData* exData = it->second;

    if (NULL == exData->exIdx() || NULL == exData->relExIdx()) {
      // It is quite strange that .ARM.exidx or .rel.ARM.exidx is missing,
      // since these are essential to the ARM EHABI.  Just ignore this case,
      // and proceed.
      exData->setIsRewritable(false);
      continue;
    }

    if (LDFileFormat::Ignore == exData->exIdx()->getLink()->kind()) {
      // The associated text section is GC'ed, thus we don't have to process
      // the exception sections.
      exData->setIsRewritable(false);
      continue;
    }

    // Check the fragments in the sections.
    const Fragment* exIdxFrag = checkAndFindRegionFragment(*exData->exIdx());
    if (NULL == exIdxFrag) {
      // Can't find the RegionFragment in .ARM.exidx section.
      exData->setIsRewritable(false);
      continue;
    }

    if ((exIdxFrag->size() & 7) != 0) {
      // According to ARM EHABI, the size of .ARM.exidx should always be
      // multiple of 8-bytes.
      exData->setIsRewritable(false);
      continue;
    }

    if (NULL != exData->exTab() &&
        NULL == checkAndFindRegionFragment(*exData->exTab())) {
      // Can't find the RegionFragment in .ARM.extab section.
      exData->setIsRewritable(false);
      continue;
    }
  }

  // Check the symbol constraint for each symbols.
  for (LDContext::const_sym_iterator symIt = inputCtx->symTabBegin(),
                                     symEnd = inputCtx->symTabEnd();
       symIt != symEnd; ++symIt) {
    LDSymbol* sym = *symIt;
    if (sym->isNull() || !sym->hasFragRef()) {
      continue;
    }

    const FragmentRef* fragRef = sym->fragRef();
    if (fragRef->isNull()) {
      continue;
    }

    if (ResolveInfo::Local != sym->binding() || 0 != fragRef->offset()) {
      const Fragment* frag = fragRef->frag();
      const LDSection& section = frag->getParent()->getSection();
      llvm::StringRef sectName(section.name());
      if (sectName.startswith(".ARM.exidx") ||
          sectName.startswith(".ARM.extab")) {
        ARMExData* exData = pExDataMap.getByExSection(sectName);
        exData->setIsRewritable(false);
#if defined(DEBUG_EX_OPT)
        llvm::errs() << "NON-REWRITABLE:"
                     << " .ARM.exidx" << exData->name()
                     << " .ARM.extab" << exData->name()
                     << " -- because symbol "
                     << sym->name() << " is at " << sectName << "+"
                     << fragRef->offset() << "\n";
#endif
      }
    }
  }

  // Check the relocations.
  for (LDContext::const_sect_iterator rsIt = inputCtx->relocSectBegin(),
                                      rsEnd = inputCtx->relocSectEnd();
       rsIt != rsEnd; ++rsIt) {
    LDSection* sect = *rsIt;
    llvm::StringRef sectName(sect->name());

    assert(sect->hasRelocData() && "Reloc section should have RelocData");
    RelocData* relocData = sect->getRelocData();

    if (sectName.startswith(".rel.ARM.exidx")) {
      scanRelExIdxToCheckRewritable(sectName, *relocData, pExDataMap);
    } else {
      scanRelToCheckRewritable(sectName, *relocData, pExDataMap);
    }
  }
}

void ARMGNULDBackend::scanRelToCheckRewritable(llvm::StringRef pRelocSectName,
                                               RelocData& pRelocData,
                                               ARMNameToExDataMap& pExDataMap)
{
  // No reference to .ARM.extab are allowed.  If there is one RelocData
  // referencing .ARM.extab, then mark that section as non-writable.
  for (RelocData::iterator rel = pRelocData.begin(),
                           end = pRelocData.end(); rel != end; ++rel) {
    LDSymbol* destSym = rel->symInfo()->outSymbol();
    if (destSym->isNull() || !destSym->hasFragRef()) {
      continue;
    }
    const FragmentRef* destFragRef = destSym->fragRef();
    if (destFragRef->isNull()) {
      continue;
    }
    const Fragment* destFrag = destFragRef->frag();
    const LDSection* destSect= &destFrag->getParent()->getSection();
    llvm::StringRef destSectName(destSect->name());
    if (destSectName.startswith(".ARM.extab")) {
      ARMExData* exData = pExDataMap.getByExSection(destSectName);
      exData->setIsRewritable(false);
#if defined(DEBUG_EX_OPT)
      llvm::errs() << "NON-REWRITABLE:"
                   << " .ARM.exidx" << exData->name()
                   << " .ARM.extab" << exData->name()
                   << " -- unexpected reference from "
                   << pRelocSectName << "\n";
#endif
    }
  }
}

void ARMGNULDBackend::
scanRelExIdxToCheckRewritable(llvm::StringRef pRelocSectName,
                              RelocData& pRelocData,
                              ARMNameToExDataMap& pExDataMap)
{
  ARMExData* relocExData = pExDataMap.getByRelExSection(pRelocSectName);

  for (RelocData::iterator rel = pRelocData.begin(),
                           end = pRelocData.end(); rel != end; ++rel) {
    const FragmentRef& fixupFragRef = rel->targetRef();

    LDSymbol* destSym = rel->symInfo()->outSymbol();
    if (destSym->isNull() || !destSym->hasFragRef()) {
      continue;
    }

    const FragmentRef* destFragRef = destSym->fragRef();
    if (destFragRef->isNull()) {
      continue;
    }

    const Fragment* destFrag = destFragRef->frag();
    const LDSection* destSect= &destFrag->getParent()->getSection();

    // If this is the odd word, then this should not reference any .ARM.extab
    // sections, including the corresponding one.  If this is the even word,
    // then this should only access the corresponding .ARM.extab.
    bool isEvenWord = ((fixupFragRef.offset() & 0x7) == 4);

    if (isEvenWord && destSect == relocExData->exTab()) {
      // If this fixup is at the even words and the destination is the
      // corresponding .ARM.extab section, then this is a good relocation.
      continue;
    }

    llvm::StringRef destSectName(destSect->name());
    if (destSectName.startswith(".ARM.extab")) {
      // Relocation is either not in the even words, or this is an unexpected
      // relocation to .ARM.extab sections which is not associated with
      // this .ARM.exidx section.
      ARMExData* exData = pExDataMap.getByExSection(destSectName);
      exData->setIsRewritable(false);
#if defined(DEBUG_EX_OPT)
      llvm::errs() << "NON-REWRITABLE:"
                   << " .ARM.exidx" << exData->name()
                   << " .ARM.extab" << exData->name()
                   << " -- unexpected reference from "
                   << pRelocSectName << "\n";
#endif
    }
  }
}

/// compareRelocOffset - compare the relocations by their offsets
static bool compareRelocOffset(const Relocation* a, const Relocation* b) {
  return ((a->targetRef().offset()) < (b->targetRef().offset()));
}

/// isRelocDataOffsetAscending - check whether RelocData has been sorted by
/// their offsets
static bool isRelocDataOffsetAscending(const RelocData& pRelocData) {
  FragmentRef::Offset lastOffset = 0;
  for (RelocData::const_iterator rel = pRelocData.begin(),
                                 end = pRelocData.end(); rel != end; ++rel) {
    const FragmentRef& fixupFragRef = rel->targetRef();
    FragmentRef::Offset fixupOffset = fixupFragRef.offset();
    if (fixupOffset < lastOffset) {
      return false;
    }
    lastOffset = fixupOffset;
  }
  return true;
}

/// sortRelocData - Sort the Relocations in RelocData by their fixup offsets.
static void sortRelocData(LDSection& pRelocSect)
{
  if (RelocData* relocData = pRelocSect.getRelocData()) {
    if (!isRelocDataOffsetAscending(*relocData)) {
      relocData->sort(compareRelocOffset);
    }
  }
}

/// buildExEntries - build the ARMExEntry for all rewritable ARMExData
void ARMGNULDBackend::buildExEntries(ARMNameToExDataMap& pExDataMap)
{
  for (ARMNameToExDataMap::iterator it = pExDataMap.begin(),
                                    end = pExDataMap.end(); it != end; ++it) {
    ARMExData* exData = it->second;
    if (exData->isRewritable()) {
      buildExEntries(*exData);
    }
  }
}

/// buildExEntries - build ARMExEntry for ARMExData
void ARMGNULDBackend::buildExEntries(ARMExData& pExData)
{
  assert(pExData.isRewritable());

  // Note: The availability of .ARM.exidx and .rel.ARM.exidx are guaranteed
  // by the checkExDataRewritable().
  LDSection& exIdx = *pExData.exIdx();
  LDSection& relExIdx = *pExData.relExIdx();

  LDSection* exTab = pExData.exTab();
  LDSection* relExTab = pExData.relExTab();

  // Sort the relocations.
  sortRelocData(relExIdx);

  if (relExTab) {
    sortRelocData(*relExTab);
  }

  // Collect the split offsets.
  std::vector<FragmentRef::Offset> offsets;

  const RelocData* relExIdxData = relExIdx.getRelocData();
  RelocData::const_iterator relExIdxIt = relExIdxData->begin();
  RelocData::const_iterator relExIdxEnd = relExIdxData->end();

  const RegionFragment* exIdxFrag =
      (const RegionFragment*)checkAndFindRegionFragment(exIdx);

  const llvm::StringRef exIdxRegion = exIdxFrag->getRegion();

  // Find the relocations at the even words and obtain the .ARM.extab starting
  // offsets.
  for (FragmentRef::Offset offset = 4, end = exIdxRegion.size();
       offset < end; offset += 8) {
    // Forward the reloc iterator until the even word is reached.
    while (relExIdxIt != relExIdxEnd &&
           relExIdxIt->targetRef().offset() < offset) {
      ++relExIdxIt;
    }

    // The relocation with smallest offset is not in this .ARM.exidx entry,
    // ignore this entry.
    if (relExIdxIt == relExIdxEnd ||
        relExIdxIt->targetRef().offset() > offset) {
      continue;
    }

    // The starting offset of .ARM.extab entry for this .ARM.exidx entry.
    FragmentRef::Offset exTabOffset =
        relExIdxIt->symInfo()->outSymbol()->fragRef()->offset() +
        relExIdxIt->target();

    offsets.push_back(exTabOffset);
  }

  if (offsets.empty()) {
    // If no .ARM.extab entry is found, then return now.
    return;
  }

  // Append the size of .ARM.extab.
  const RegionFragment* exTabFrag =
      (const RegionFragment*)checkAndFindRegionFragment(*exTab);

  const llvm::StringRef exTabRegion = exTabFrag->getRegion();

  offsets.push_back(exTabRegion.size());

  std::sort(offsets.begin(), offsets.end());

  // Initialize the .rel.ARM.extab iterator.
  RelocData* relExTabData;
  RelocData::iterator relExTabIt, relExTabEnd;
  if (NULL == relExTab) {
    relExTabData = NULL;
    relExTabIt = NULL;
    relExTabEnd = NULL;
  } else {
    relExTabData = relExTab->getRelocData();
    relExTabIt = relExTabData->begin();
    relExTabEnd = relExTabData->end();
  }

  // Compute the .ARM.extab entries.
  const char* exTabRegionData = exTabRegion.data();
  for (size_t i = 0, n = offsets.size() - 1; i < n; ++i) {
    FragmentRef::Offset entryBegin = offsets[i];
    FragmentRef::Offset entryEnd = offsets[i + 1];
    llvm::StringRef entryRegion(exTabRegionData + entryBegin,
                                entryEnd - entryBegin);

    // Forward the reloc iterator until the entry begins.
    while (relExTabIt != relExTabEnd &&
           relExTabIt->targetRef().offset() < entryBegin) {
      ++relExTabIt;
    }

    RelocData::iterator entryRelocBegin = relExTabIt;

    // Forward the reloc iterator until the entry ends.
    while (relExTabIt != relExTabEnd &&
           relExTabIt->targetRef().offset() < entryEnd) {
      ++relExTabIt;
    }

    RelocData::iterator entryRelocEnd = relExTabIt;

    pExData.appendEntry(ARMExEntry(pExData, entryBegin, entryRegion,
                                   entryRelocBegin, entryRelocEnd));
  }
}

namespace mcld {

//===----------------------------------------------------------------------===//
/// createARMLDBackend - the help funtion to create corresponding ARMLDBackend
///
TargetLDBackend* createARMLDBackend(const LinkerConfig& pConfig) {
  if (pConfig.targets().triple().isOSDarwin()) {
    assert(0 && "MachO linker is not supported yet");
    /**
    return new ARMMachOLDBackend(createARMMachOArchiveReader,
                               createARMMachOObjectReader,
                               createARMMachOObjectWriter);
    **/
  }
  if (pConfig.targets().triple().isOSWindows()) {
    assert(0 && "COFF linker is not supported yet");
    /**
    return new ARMCOFFLDBackend(createARMCOFFArchiveReader,
                               createARMCOFFObjectReader,
                               createARMCOFFObjectWriter);
    **/
  }
  return new ARMGNULDBackend(pConfig,
                             new ARMGNUInfo(pConfig.targets().triple()));
}

}  // namespace mcld

//===----------------------------------------------------------------------===//
// Force static initialization.
//===----------------------------------------------------------------------===//
extern "C" void MCLDInitializeARMLDBackend() {
  // Register the linker backend
  mcld::TargetRegistry::RegisterTargetLDBackend(TheARMTarget,
                                                createARMLDBackend);
  mcld::TargetRegistry::RegisterTargetLDBackend(TheThumbTarget,
                                                createARMLDBackend);
}
