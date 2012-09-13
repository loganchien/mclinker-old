//===- ARMGOT.h -----------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MCLD_ARM_GOT_H
#define MCLD_ARM_GOT_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif

#include <llvm/ADT/DenseMap.h>

#include <mcld/Target/GOT.h>

namespace mcld {

class LDSection;
class MemoryRegion;

/** \class ARMGOT
 *  \brief ARM Global Offset Table.
 *
 *  ARM GOT integrates traditional .got.plt and .got sections into one.
 *  Traditional .got.plt is placed in the front part of GOT (PLTGOT), and
 *  traditional .got is placed in the rear part of GOT (GOT).
 *
 *  ARM .got
 *            +--------------+
 *            |    PLTGOT    |
 *            +--------------+
 *            |     GOT      |
 *            +--------------+
 *
 */
class ARMGOT : public GOT
{
public:
  ARMGOT(LDSection &pSection, SectionData& pSectionData);

  ~ARMGOT();

  void reserveGOTPLTEntry();

  GOT::Entry* getOrConsumeGOTPLTEntry(const ResolveInfo& pInfo, bool& pExist);

  uint64_t emit(MemoryRegion& pRegion);

  void applyGOT0(uint64_t pAddress);

  void applyAllGOTPLT(uint64_t pPLTBase);

  // ----- obsererse ----- //
  iterator getGOTPLTBegin();

  const iterator getGOTPLTEnd();

  bool hasGOT1() const;

private:
  typedef llvm::DenseMap<const ResolveInfo*, GOT::Entry*> SymbolIndexMapType;

private:
  // For GOTPLT entries
  iterator m_GOTPLTIterator;
  SymbolIndexMapType m_GOTPLTMap;

  iterator m_GOTPLTBegin;
  iterator m_GOTPLTEnd;
};

} // namespace of mcld

#endif

