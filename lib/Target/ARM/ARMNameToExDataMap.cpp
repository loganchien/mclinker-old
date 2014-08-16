//===- ARMNameToExDataMap.cpp ---------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "ARMNameToExDataMap.h"

#include "ARMExData.h"

#include <llvm/Support/raw_ostream.h>

using namespace mcld;

ARMNameToExDataMap::~ARMNameToExDataMap()
{
  for (MapTy::iterator it = m_ExDataMap.begin(), end = m_ExDataMap.end();
       it != end; ++it) {
    delete it->second;
  }
}
