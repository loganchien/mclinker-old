//===- ARMExData.h --------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef TARGET_ARM_ARMEXDATA_H
#define TARGET_ARM_ARMEXDATA_H

#include "ARMExEntry.h"

#include <string>

#include <cstddef>

namespace mcld {

class LDSection;

class ARMExData {
public:
  ARMExData(const std::string& pName)
    : m_Name(pName),
      m_pExIdx(NULL),
      m_pExTab(NULL),
      m_pRelExIdx(NULL),
      m_pRelExTab(NULL),
      m_IsRewritable(true)
  { }

  const std::string& name() const
  { return m_Name; }

  LDSection* exIdx() const
  { return m_pExIdx; }

  LDSection* exTab() const
  { return m_pExTab; }

  LDSection* relExIdx() const
  { return m_pRelExIdx; }

  LDSection* relExTab() const
  { return m_pRelExTab; }

  void setExIdx(LDSection* s)
  { m_pExIdx = s; }

  void setExTab(LDSection* s)
  { m_pExTab = s; }

  void setRelExIdx(LDSection* s)
  { m_pRelExIdx = s; }

  void setRelExTab(LDSection* s)
  { m_pRelExTab = s; }

  /// isRewritable - Whether we can rewrite these exception sections.
  bool isRewritable() const
  { return m_IsRewritable; }

  void setIsRewritable(bool rewritable)
  { m_IsRewritable = rewritable; }

  void appendEntry(const ARMExEntry& pEnt)
  { m_ExTabEntries.push_back(pEnt); }

  typedef std::vector<ARMExEntry>::iterator iterator;
  typedef std::vector<ARMExEntry>::const_iterator const_iterator;

  iterator       begin()       { return m_ExTabEntries.begin(); }
  const_iterator begin() const { return m_ExTabEntries.begin(); }

  iterator       end()       { return m_ExTabEntries.end(); }
  const_iterator end() const { return m_ExTabEntries.end(); }

private:
  /// m_Name - text section name associated with these exception sections.
  std::string m_Name;

  /// m_pExIdx - .ARM.exidx section
  LDSection* m_pExIdx;

  /// m_pExTab - .ARM.extab section
  LDSection* m_pExTab;

  /// m_pRelExIdx - .rel.ARM.exidx section
  LDSection* m_pRelExIdx;

  /// m_pRelExTab - .rel.ARM.extab section
  LDSection* m_pRelExTab;

  /// m_IsRewritable - Whether we can rewrite these exception sections.
  bool m_IsRewritable;

  /// m_ExTabEntries - .ARM.extab entries.
  std::vector<ARMExEntry> m_ExTabEntries;
};

} // namespace of mcld

#endif
