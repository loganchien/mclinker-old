//===- ARMException.cpp ---------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "ARMException.h"

#include "ARMLDBackend.h"

#include "mcld/ADT/ilist_sort.h"
#include "mcld/Fragment/RegionFragment.h"
#include "mcld/LD/ELFFileFormat.h"
#include "mcld/LD/LDContext.h"
#include "mcld/Support/MsgHandling.h"

#include <memory>

using namespace mcld;

void ARMExData::addInputMap(Input* pInput,
                            std::unique_ptr<ARMInputExMap>&& pExMap) {
  assert(m_Inputs.find(pInput) == m_Inputs.end() &&
         "multiple maps for an input");

  ARMInputExMap* exMap = pExMap.get();

  // Add mapping to the input-to-exdata map.
  m_Inputs.insert(std::make_pair(pInput, std::move(pExMap)));

  // Add mapping to the fragment-to-exdata map.
  for (ARMInputExMap::iterator it = exMap->begin(), end = exMap->end();
       it != end; ++it) {
    ARMExSectionTuple* exTuple = it->second.get();
    m_ExIdxToTuple[exTuple->getExIdxFragment()] = exTuple;
  }
}

void ARMGNULDBackend::scanInputExceptionSections(Module& pModule) {
  for (Module::obj_iterator it = pModule.obj_begin(),
                            end = pModule.obj_end(); it != end; ++it) {
    Input* input = *it;
    scanInputExceptionSections(pModule, *input);
  }
}

static RegionFragment* findRegionFragment(LDSection& pSection) {
  SectionData* sectData = pSection.getSectionData();
  for (SectionData::iterator it = sectData->begin(),
                             end = sectData->end(); it != end; ++it) {
    if (it->getKind() == Fragment::Region) {
      return static_cast<RegionFragment*>(&*it);
    }
  }
  return NULL;
}

void ARMGNULDBackend::scanInputExceptionSections(Module& pModule,
                                                 Input& pInput) {
  std::unique_ptr<ARMInputExMap> exMap(new ARMInputExMap());

  // Scan the input and collect all related sections.
  LDContext* ctx = pInput.context();
  for (LDContext::sect_iterator it = ctx->sectBegin(),
                                end = ctx->sectEnd(); it != end; ++it) {
    LDSection* sect = *it;
    llvm::StringRef name(sect->name());

    if (name.startswith(".ARM.exidx")) {
      ARMExSectionTuple* exTuple = exMap->getOrCreateByExSection(name);
      exTuple->setExIdxSection(sect);
      exTuple->setTextSection(sect->getLink());
    } else if (name.startswith(".ARM.extab")) {
      ARMExSectionTuple* exTuple = exMap->getOrCreateByExSection(name);
      exTuple->setExTabSection(sect);
    } else if (name.startswith(".rel.ARM.exidx")) {
      ARMExSectionTuple* exTuple = exMap->getOrCreateByRelExSection(name);
      exTuple->setRelExIdxSection(sect);
    } else if (name.startswith(".rel.ARM.extab")) {
      ARMExSectionTuple* exTuple = exMap->getOrCreateByRelExSection(name);
      exTuple->setRelExIdxSection(sect);
    }
  }

  // Remove the invalid exception tuples and convert LDSection to RegionFragment
  // or RelocData.
  ARMInputExMap::iterator it = exMap->begin();
  ARMInputExMap::iterator end = exMap->end();
  while (it != end) {
    ARMExSectionTuple* exTuple = it->second.get();
    LDSection* const text = exTuple->getTextSection();
    LDSection* const exIdx = exTuple->getExIdxSection();
    LDSection* const exTab = exTuple->getExTabSection();
    LDSection* const relExIdx = exTuple->getRelExIdxSection();
    LDSection* const relExTab = exTuple->getRelExTabSection();

    // Check the .ARM.exidx section.
    if (!exIdx) {
      if (exTab) {
        fatal(diag::eh_missing_exidx_section) << exTab->name() << pInput.name();
      } else if (relExIdx) {
        fatal(diag::eh_missing_exidx_section) << relExIdx->name()
                                              << pInput.name();
      } else if (relExTab) {
        fatal(diag::eh_missing_exidx_section) << relExTab->name()
                                              << pInput.name();
      } else {
        llvm_unreachable("unexpected bad exception tuple");
      }
    }

    // Check the text section.
    if (!text) {
      fatal(diag::eh_missing_text_section) << exIdx->name() << pInput.name();
    }

    // Ignore the exception section if the text section is ignored.
    if ((text->kind() == LDFileFormat::Ignore) ||
        (text->kind() == LDFileFormat::Folded)) {
      // Set the related exception sections as LDFileFormat::Ignore.
      exIdx->setKind(LDFileFormat::Ignore);
      if (exTab) {
        exTab->setKind(LDFileFormat::Ignore);
      }
      // Remove this tuple from the input exception map.
      exMap->erase(it++);
      continue;
    }

    // Get RegionFragment from ".text", ".ARM.exidx", and ".ARM.extab" sections.
    RegionFragment* textFrag = findRegionFragment(*text);
    RegionFragment* exIdxFrag = findRegionFragment(*exIdx);
    RegionFragment* exTabFrag = exTab ? findRegionFragment(*exTab) : NULL;

    exTuple->setTextFragment(textFrag);
    exTuple->setExIdxFragment(exIdxFrag);
    exTuple->setExTabFragment(exTabFrag);

    // Get the RelocData from ".rel.ARM.exidx" and ".rel.ARM.extab" sections.
    RelocData* exIdxRD = relExIdx ? relExIdx->getRelocData() : NULL;
    RelocData* exTabRD = relExTab ? relExTab->getRelocData() : NULL;

    exTuple->setExIdxRelocData(exIdxRD);
    exTuple->setExTabRelocData(exTabRD);

    // If there is no region fragment in the .ARM.extab section, then we can
    // skip this tuple.
    if (!exIdxFrag) {
      exMap->erase(it++);
      continue;
    }

    // TODO: Sort the RelocData w.r.t. the fixup offset.

    // Check next tuple
    ++it;
  }

  // Add input map
  m_ExData.addInputMap(&pInput, std::move(exMap));
}

class ExIdxFragmentComparator {
private:
  const ARMExData& m_ExData;

public:
  ExIdxFragmentComparator(const ARMExData& pExData)
      : m_ExData(pExData) {
  }

  bool operator()(const Fragment& a, const Fragment& b) {
    ARMExSectionTuple* tupleA = m_ExData.getTupleByExIdx(&a);
    ARMExSectionTuple* tupleB = m_ExData.getTupleByExIdx(&b);

    Fragment* textFragA = tupleA->getTextFragment();
    Fragment* textFragB = tupleB->getTextFragment();

    uint64_t addrA = textFragA->getParent()->getSection().addr() +
                     textFragA->getOffset();
    uint64_t addrB = textFragB->getParent()->getSection().addr() +
                     textFragB->getOffset();
    return (addrA < addrB);
  }
};

void ARMGNULDBackend::rewriteARMExIdxSection(Module& pModule) {
  if (!m_pEXIDX->hasSectionData()) {
    // Return if this is empty section.
    return;
  }

  SectionData* sectData = m_pEXIDX->getSectionData();
  SectionData::FragmentListType& list = sectData->getFragmentList();

  // Move the first and last fragment to temporary list.
  SectionData::FragmentListType tmp;
  {
    SectionData::iterator first = sectData->begin();
    SectionData::iterator last = sectData->end();
    --last;

    assert(first->getKind() == Fragment::Alignment);
    assert(last->getKind() == Fragment::Null);

    tmp.splice(tmp.end(), list, first);
    tmp.splice(tmp.end(), list, last);
  }

  // Sort the region fragments in the .ARM.exidx output section.
  sort(list, ExIdxFragmentComparator(m_ExData));

  // Add the first and last fragment back.
  list.splice(list.begin(), tmp, tmp.begin());
  list.splice(list.end(), tmp, tmp.begin());

  // Update the fragment offsets.
  uint64_t offset = 0;
  for (SectionData::iterator it = sectData->begin(), end = sectData->end();
       it != end; ++it) {
    it->setOffset(offset);
    offset += it->size();
  }

  // Rebuild the section header.
  setOutputSectionAddress(pModule);
}
