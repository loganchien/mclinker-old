//===- ARMExEntry.cpp -----------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "ARMExEntry.h"

using namespace mcld;

bool ARMExEntry::operator==(const ARMExEntry& rhs) const
{
  // Return false if the content (without relocation) is not equal.
  if (m_pInputRegion != rhs.getRegion()) {
    return false;
  }

  // Check the relocations.
  Relocation* lhsIt = begin();
  Relocation* lhsEnd = end();
  Relocation* rhsIt = rhs.begin();
  Relocation* rhsEnd = rhs.end();

  while (lhsIt != lhsEnd && rhsIt != rhsEnd) {
    // Return false if the word to be filled to fixup is not equal.
    if (lhsIt->target() != rhsIt->target()) {
      return false;
    }

    // Return false if the fixups offset is not equal.
    if (lhsIt->targetRef().offset() - getInputOffset() !=
        rhsIt->targetRef().offset() - rhs.getInputOffset()) {
      return false;
    }

    // Return false if the target destination is not equal.
    if (lhsIt->symInfo() != rhsIt->symInfo()) {
      return false;
    }

    lhsIt = lhsIt->getNextNode();
    rhsIt = rhsIt->getNextNode();
  }

  if (lhsIt != lhsEnd || rhsIt != rhsEnd) {
    // Return false because the number of relocations is not equal.
    return false;
  }

  return true;
}
