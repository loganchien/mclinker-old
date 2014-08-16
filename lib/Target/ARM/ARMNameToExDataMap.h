//===- ARMNameToExDataMap.h -----------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef TARGET_ARM_ARMNAMETOEXDATAMAP_H
#define TARGET_ARM_ARMNAMETOEXDATAMAP_H

#include "ARMExData.h"

#include <llvm/ADT/StringRef.h>

#include <map>
#include <string>

#include <cassert>

namespace mcld {

class ARMExData;

class ARMNameToExDataMap {
public:
  typedef std::map<std::string, ARMExData*> MapTy;
  typedef MapTy::iterator iterator;
  typedef MapTy::const_iterator const_iterator;

public:
  ARMNameToExDataMap() { }

  ~ARMNameToExDataMap();

  /// get - Get the ARMExData by the corresponding text section name.
  /// As an exception, to get the ARMExData for .text section, use ""
  /// as the section name instead.
  ARMExData* get(const char* pName) const
  {
    MapTy::const_iterator it = m_ExDataMap.find(pName);
    if (it == m_ExDataMap.end()) {
      return NULL;
    }
    return it->second;
  }

  ARMExData* getByExSection(llvm::StringRef pName) const
  {
    assert((pName.startswith(".ARM.exidx") ||
            pName.startswith(".ARM.extab")) &&
           "Not a .ARM.exidx section name");
    return get(pName.data() + sizeof(".ARM.ex***") - 1);
  }

  ARMExData* getByRelExSection(llvm::StringRef pName) const
  {
    assert((pName.startswith(".rel.ARM.exidx") ||
            pName.startswith(".rel.ARM.extab")) &&
           "Not a .rel.ARM.exidx section name");
    return get(pName.data() + sizeof(".rel.ARM.ex***") - 1);
  }

  /// getOrCreate - Get an existing or create a new ARMExData which is
  /// associated with the text section name.  As an exception, use "" as the
  /// section name for .text section.
  ARMExData* getOrCreate(const char* pName)
  {
    ARMExData*& result = m_ExDataMap[pName];
    if (NULL == result) {
      result = new ARMExData(pName);
    }
    return result;
  }

  ARMExData* getOrCreateByExSection(llvm::StringRef pName)
  {
    assert((pName.startswith(".ARM.exidx") ||
            pName.startswith(".ARM.extab")) &&
           "Not a .ARM.exidx section name");
    return getOrCreate(pName.data() + sizeof(".ARM.ex***") - 1);
  }

  ARMExData* getOrCreateByRelExSection(llvm::StringRef pName)
  {
    assert((pName.startswith(".rel.ARM.exidx") ||
            pName.startswith(".rel.ARM.extab")) &&
           "Not a .rel.ARM.exidx section name");
    return getOrCreate(pName.data() + sizeof(".rel.ARM.ex***") - 1);
  }

  /// begin - return the iterator to the begin of the map
  iterator       begin()       { return m_ExDataMap.begin(); }
  const_iterator begin() const { return m_ExDataMap.begin(); }

  /// end - return the iterator to the end of the map
  iterator       end()       { return m_ExDataMap.end(); }
  const_iterator end() const { return m_ExDataMap.end(); }

private:
  MapTy m_ExDataMap;
};

} // namespace of mcld

#endif
