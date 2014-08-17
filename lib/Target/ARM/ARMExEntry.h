//===- ARMExEntry.h -------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef TARGET_ARM_ARMEXENTRY_H
#define TARGET_ARM_ARMEXENTRY_H

#include "mcld/Fragment/FragmentRef.h"
#include "mcld/LD/RelocData.h"

#include <llvm/ADT/StringRef.h>

namespace mcld {

class ARMExData;

class ARMExEntry {
private:
  FragmentRef::Offset m_Offset;
  const llvm::StringRef m_pInputRegion;
  ARMExData* m_pParent;
  Relocation* m_pRelocBegin;
  Relocation* m_pRelocEnd;

public:
  ARMExEntry(ARMExData& pParent, FragmentRef::Offset pOffset,
             const llvm::StringRef pInputRegion, Relocation* pRelocBegin,
             Relocation* pRelocEnd)
    : m_Offset(pOffset),
      m_pInputRegion(pInputRegion),
      m_pParent(&pParent),
      m_pRelocBegin(pRelocBegin),
      m_pRelocEnd(pRelocEnd)
  { }

  ARMExData* getParent() const
  { return m_pParent; }

  FragmentRef::Offset getInputOffset() const
  { return m_Offset; }

  const llvm::StringRef getRegion() const
  { return m_pInputRegion; }

  Relocation* begin() const
  { return m_pRelocBegin; }

  Relocation* end() const
  { return m_pRelocEnd; }
};

} // namespace of mcld

#endif
