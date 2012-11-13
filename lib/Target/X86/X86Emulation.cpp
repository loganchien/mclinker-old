//===- X86Emulation.cpp ---------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "X86.h"
#include <mcld/LinkerConfig.h>
#include <mcld/Target/ELFEmulation.h>
#include <mcld/Support/TargetRegistry.h>

namespace mcld {

static bool MCLDEmulateX86ELF(LinkerConfig& pConfig)
{
  if (!MCLDEmulateELF(pConfig))
    return false;

  // set up target-dependent constraints of attributes
  pConfig.attribute().constraint().enableWholeArchive();
  pConfig.attribute().constraint().enableAsNeeded();
  pConfig.attribute().constraint().setSharedSystem();

  // set up the predefined attributes
  pConfig.attribute().predefined().unsetWholeArchive();
  pConfig.attribute().predefined().unsetAsNeeded();
  pConfig.attribute().predefined().setDynamic();
  return true;
}

//===----------------------------------------------------------------------===//
// emulateX86LD - the help function to emulate X86 ld
//===----------------------------------------------------------------------===//
bool emulateX86LD(const std::string& pTriple, LinkerConfig& pConfig)
{
  llvm::Triple theTriple(pTriple);
  if (theTriple.isOSDarwin()) {
    assert(0 && "MachO linker has not supported yet");
    return NULL;
  }
  if (theTriple.isOSWindows()) {
    assert(0 && "COFF linker has not supported yet");
    return NULL;
  }

  return MCLDEmulateX86ELF(pConfig);
}

} // namespace of mcld

//===----------------------------------------------------------------------===//
// X86Emulation
//===----------------------------------------------------------------------===//
extern "C" void MCLDInitializeX86Emulation() {
  // Register the emulation
  mcld::TargetRegistry::RegisterEmulation(mcld::TheX86Target, mcld::emulateX86LD);
}

