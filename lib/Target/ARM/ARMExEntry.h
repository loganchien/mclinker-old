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
#include "mcld/Fragment/RegionFragment.h"
#include "mcld/IRBuilder.h"
#include "mcld/LD/RelocData.h"

#include <llvm/ADT/StringRef.h>

namespace mcld {

class ARMExData;

class ARMExEntry {
private:
  FragmentRef::Offset m_Offset;
  const llvm::StringRef m_pInputRegion;
  RegionFragment* m_pSplittedRegionFragment;
  LDSymbol* m_pSplittedSymbol;
  ARMExData* m_pParent;
  Relocation* m_pRelocBegin;
  Relocation* m_pRelocEnd;

public:
  ARMExEntry(ARMExData& pParent, FragmentRef::Offset pOffset,
             const llvm::StringRef pInputRegion, Relocation* pRelocBegin,
             Relocation* pRelocEnd)
    : m_Offset(pOffset),
      m_pInputRegion(pInputRegion),
      m_pSplittedRegionFragment(NULL),
      m_pSplittedSymbol(NULL),
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

  RegionFragment* getRegionFragment() const
  { return m_pSplittedRegionFragment; }

  void setRegionFragment(RegionFragment* pFrag)
  { m_pSplittedRegionFragment = pFrag; }

  LDSymbol* getSymbol() const
  { return m_pSplittedSymbol; }

  void setSymbol(LDSymbol* pSym)
  { m_pSplittedSymbol = pSym; }

  Relocation* begin() const
  { return m_pRelocBegin; }

  Relocation* end() const
  { return m_pRelocEnd; }

  bool operator==(const ARMExEntry& rhs) const
  {
    // FIXME: This should return true when the content is equivalent, i.e.
    // having the same input region data and same relocations.
    return false;
  }

  bool operator!=(const ARMExEntry& rhs) const
  { return !(*this == rhs); }
};

} // namespace of mcld

#endif
