//===- Linker.cpp ---------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "alone/Linker.h"
#include "alone/Support/LinkerConfig.h"
#include "alone/Support/MemoryFactory.h"
#include "alone/Support/Log.h"

#include <llvm/Support/ELF.h>

#include <mcld/Object/ObjectLinker.h>
#include <mcld/MC/InputTree.h>
#include <mcld/Fragment/FragmentLinker.h>
#include <mcld/MC/InputTree.h>
#include <mcld/LD/LDSection.h>
#include <mcld/LD/LDContext.h>
#include <mcld/Target/TargetLDBackend.h>
#include <mcld/Support/Path.h>
#include <mcld/Support/MemoryArea.h>
#include <mcld/Support/FileHandle.h>
#include <mcld/Support/MemoryAreaFactory.h>
#include <mcld/Support/TargetRegistry.h>

using namespace alone;

const char* Linker::GetErrorString(enum Linker::ErrorCode pErrCode) {
  static const char* ErrorString[] = {
    /* kSuccess */
    "Successfully compiled.",
    /* kDoubleConfig */
    "Configure Linker twice.",
    /* kCreateBackend */
    "Cannot create backend.",
    /* kDelegateLDInfo */
    "Cannot get linker information",
    /* kFindNameSpec */
    "Cannot find -lnamespec",
    /* kOpenNameSpec */
    "Cannot open -lnamespec",
    /* kOpenObjectFile */
    "Cannot open object file",
    /* kNotConfig */
    "Linker::config() is not called",
    /* kNotSetUpOutput */
    "Linker::setOutput() is not called before add input files",
    /* kOpenOutput */
    "Cannot open output file",
    /* kReadSections */
    "Cannot read sections",
    /* kReadSymbols */
    "Cannot read symbols",
    /* kAddAdditionalSymbols */
    "Cannot add standard and target symbols",
    /* kMaxErrorCode */
    "(Unknown error code)"
  };

  if (pErrCode > kMaxErrorCode) {
    pErrCode = kMaxErrorCode;
  }

  return ErrorString[ static_cast<size_t>(pErrCode) ];
}

//===----------------------------------------------------------------------===//
// Linker
//===----------------------------------------------------------------------===//
Linker::Linker()
  : mBackend(NULL), mObjLinker(NULL), mMemAreaFactory(NULL), mLDConfig(NULL),
    mRoot(NULL), mShared(false) {
}

Linker::Linker(const LinkerConfig& pConfig)
  : mBackend(NULL), mObjLinker(NULL), mMemAreaFactory(NULL), mLDConfig(NULL),
    mRoot(NULL), mShared(false) {

  const std::string &triple = pConfig.getTriple();

  enum ErrorCode err = config(pConfig);
  if (kSuccess != err) {
    ALOGE("%s (%s)", GetErrorString(err), triple.c_str());
    return;
  }

  return;
}

Linker::~Linker() {
  delete mObjLinker;
  delete mBackend;
  delete mMemAreaFactory;
  delete mRoot;
}

enum Linker::ErrorCode Linker::extractFiles(const LinkerConfig& pConfig) {
  mLDConfig = const_cast<mcld::LinkerConfig*>(pConfig.getLDConfig());
  if (mLDConfig == NULL) {
    return kDelegateLDInfo;
  }

  mRoot = new mcld::InputTree::iterator(mLDConfig->inputs().root());
  mShared = pConfig.isShared();
  mSOName = pConfig.getSOName();

  return kSuccess;
}

enum Linker::ErrorCode Linker::config(const LinkerConfig& pConfig) {
  if (mLDConfig != NULL) {
    return kDoubleConfig;
  }

  extractFiles(pConfig);

  mBackend = pConfig.getTarget()->createLDBackend(pConfig.getTriple());
  if (mBackend == NULL) {
    return kCreateBackend;
  }

  mMemAreaFactory = new MemoryFactory();

  mObjLinker = new mcld::ObjectLinker(*mLDConfig, *mBackend, *mMemAreaFactory);

  mObjLinker->initFragmentLinker();

  return kSuccess;
}

void Linker::advanceRoot() {
  if (mRoot->isRoot()) {
    mRoot->move<mcld::TreeIteratorBase::Leftward>();
  } else {
    mRoot->move<mcld::TreeIteratorBase::Rightward>();
  }
  return;
}

enum Linker::ErrorCode Linker::openFile(const mcld::sys::fs::Path& pPath,
                                        enum Linker::ErrorCode pCode,
                                        mcld::Input& pInput) {
  mcld::MemoryArea *input_memory = mMemAreaFactory->produce(pPath,
                                                    mcld::FileHandle::ReadOnly);

  if (input_memory->handler()->isGood()) {
    pInput.setMemArea(input_memory);
  } else {
    return pCode;
  }

  mcld::LDContext *input_context = mLDConfig->contextFactory().produce(pPath);
  pInput.setContext(input_context);
  return kSuccess;
}

enum Linker::ErrorCode Linker::addNameSpec(const std::string &pNameSpec) {
  mcld::sys::fs::Path* path = NULL;
  // find out the real path of the namespec.
  if (mLDConfig->attrFactory().constraint().isSharedSystem()) {
    // In the system with shared object support, we can find both archive
    // and shared object.

    if (mLDConfig->attrFactory().last().isStatic()) {
      // with --static, we must search an archive.
      path = mLDConfig->options().directories().find(pNameSpec,
                                                   mcld::Input::Archive);
    }
    else {
      // otherwise, with --Bdynamic, we can find either an archive or a
      // shared object.
      path = mLDConfig->options().directories().find(pNameSpec,
                                                   mcld::Input::DynObj);
    }
  }
  else {
    // In the system without shared object support, we only look for an
    // archive.
    path = mLDConfig->options().directories().find(pNameSpec,
                                                 mcld::Input::Archive);
  }

  if (NULL == path)
    return kFindNameSpec;

  mcld::Input* input = mLDConfig->inputFactory().produce(pNameSpec, *path,
                                                       mcld::Input::Unknown);
  mLDConfig->inputs().insert<mcld::InputTree::Positional>(*mRoot, *input);

  advanceRoot();

  return openFile(*path, kOpenNameSpec, *input);
}

/// addObject - Add a object file by the filename.
enum Linker::ErrorCode Linker::addObject(const std::string &pObjectPath) {
  mcld::Input* input = mLDConfig->inputFactory().produce(pObjectPath,
                                                       pObjectPath,
                                                       mcld::Input::Unknown);

  mLDConfig->inputs().insert<mcld::InputTree::Positional>(*mRoot, *input);

  advanceRoot();

  return openFile(pObjectPath, kOpenObjectFile, *input);
}

/// addObject - Add a piece of memory. The memory is of ELF format.
enum Linker::ErrorCode Linker::addObject(void* pMemory, size_t pSize) {

  mcld::Input* input = mLDConfig->inputFactory().produce("memory object",
                                                       "NAN",
                                                       mcld::Input::Unknown);

  mLDConfig->inputs().insert<mcld::InputTree::Positional>(*mRoot, *input);

  advanceRoot();

  mcld::MemoryArea *input_memory = mMemAreaFactory->produce(pMemory, pSize);
  input->setMemArea(input_memory);

  mcld::LDContext *input_context = mLDConfig->contextFactory().produce();
  input->setContext(input_context);

  return kSuccess;
}

enum Linker::ErrorCode Linker::addCode(void* pMemory, size_t pSize) {
  mcld::Input* input = mLDConfig->inputFactory().produce("code object",
                                                       "NAN",
                                                       mcld::Input::External);

  mLDConfig->inputs().insert<mcld::InputTree::Positional>(*mRoot, *input);

  advanceRoot();

  mcld::MemoryArea *input_memory = mMemAreaFactory->produce(pMemory, pSize);
  input->setMemArea(input_memory);

  mcld::LDContext *input_context = mLDConfig->contextFactory().produce();
  input->setContext(input_context);

  // FIXME: So far, FragmentLinker must set up output before add input files.
  // set up LDContext
  if (mObjLinker->hasInitLinker()) {
    return kNotConfig;
  }

  if (!mLDConfig->output().hasContext()) {
    return kNotSetUpOutput;
  }

  // create NULL section
  mcld::LDSection& null =
      mObjLinker->getLinker()->createSectHdr("",
                                          mcld::LDFileFormat::Null,
                                          llvm::ELF::SHT_NULL,
                                          0);

  null.setSize(0);
  null.setOffset(0);
  null.setIndex(0);
  null.setInfo(0);
  null.setAlign(0);

  input_context->getSectionTable().push_back(&null);

  // create .text section
  mcld::LDSection& text = mObjLinker->getLinker()->createSectHdr(".text",
                              mcld::LDFileFormat::Regular,
                              llvm::ELF::SHT_PROGBITS,
                              llvm::ELF::SHF_ALLOC | llvm::ELF::SHF_EXECINSTR);

  text.setSize(pSize);
  text.setOffset(0x0);
  text.setIndex(1);
  text.setInfo(0);
  text.setAlign(1);

  input_context->getSectionTable().push_back(&text);

  return kSuccess;
}

enum Linker::ErrorCode Linker::setOutput(const std::string &pPath) {
  if (mLDConfig->output().hasContext()) {
    return kDoubleConfig;
  }

  // -----  initialize output file  ----- //

  mcld::FileHandle::Permission perm = 0755;

  mcld::MemoryArea* out_area = mMemAreaFactory->produce(
                      pPath,
                      mcld::FileHandle::ReadWrite |
                        mcld::FileHandle::Truncate |
                        mcld::FileHandle::Create,
                      perm);

  if (!out_area->handler()->isGood()) {
    return kOpenOutput;
  }

  if (mShared) {
    mLDConfig->output().setType(mcld::Output::DynObj);
  } else {
    mLDConfig->output().setType(mcld::Output::Exec);
  }

  mLDConfig->output().setSOName(mSOName);
  mLDConfig->output().setMemArea(out_area);
  mLDConfig->output().setContext(mLDConfig->contextFactory().produce(pPath));

  // FIXME: We must initialize FragmentLinker before setOutput, and initialize
  // standard sections here. This is because we have to build the section
  // map before input files using it.
  if (!mObjLinker->hasInitLinker()) {
    return kNotConfig;
  }

  mObjLinker->initStdSections();

  return kSuccess;
}

enum Linker::ErrorCode Linker::setOutput(int pFileHandler) {
  if (mLDConfig->output().hasContext()) {
    return kDoubleConfig;
  }

  // -----  initialize output file  ----- //
  mcld::MemoryArea* out_area = mMemAreaFactory->produce(pFileHandler);

  mLDConfig->output().setType(mcld::Output::DynObj);
  mLDConfig->output().setMemArea(out_area);
  mLDConfig->output().setContext(mLDConfig->contextFactory().produce());

  // FIXME: We must initialize FragmentLinker before setOutput, and initialize
  // standard sections here. This is because we have to build the section
  // map before input files using it.
  if (!mObjLinker->hasInitLinker()) {
    return kNotConfig;
  }

  mObjLinker->initStdSections();

  return kSuccess;
}

enum Linker::ErrorCode Linker::link() {
  mObjLinker->normalize();

  if (!mObjLinker->mergeSections()) {
    return kReadSections;
  }

  if (!mObjLinker->addStandardSymbols() || !mObjLinker->addTargetSymbols()) {
    return kAddAdditionalSymbols;
  }

  mObjLinker->readRelocations();
  mObjLinker->prelayout();
  mObjLinker->layout();
  mObjLinker->postlayout();
  mObjLinker->finalizeSymbolValue();
  mObjLinker->relocation();
  mObjLinker->emitOutput();
  mObjLinker->postProcessing();

  return kSuccess;
}

