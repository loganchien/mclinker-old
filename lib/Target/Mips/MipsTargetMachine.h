//===- MipsTargetMachine.h ------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MIPS_TARGET_MACHINE_H
#define MIPS_TARGET_MACHINE_H
#include "mcld/Target/TargetMachine.h"
#include "Mips.h"

namespace mcld
{

class MipsBaseTargetMachine : public LLVMTargetMachine
{
protected:
  LinkerConfig *m_pConfig;

public:
  MipsBaseTargetMachine(llvm::TargetMachine &pTM,
                        const mcld::Target &pTarget,
                        const std::string &pTriple);

  virtual ~MipsBaseTargetMachine();

  mcld::LinkerConfig& getConfig()
  { return *m_pConfig; }

  const mcld::LinkerConfig& getConfig() const
  { return *m_pConfig; }
};

} // namespace of mcld

#endif
