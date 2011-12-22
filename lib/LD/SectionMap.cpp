//===- SectionMap.cpp -----------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <cassert>
#include <cstring>
#include <mcld/LD/SectionMap.h>

using namespace mcld;

//==========================
// SectionMap

SectionMap::SectionMap()
{
}

SectionMap::~SectionMap()
{
}

const std::string& SectionMap::getOutputSectName(const std::string& pInput)
{
  iterator it;
  for (it = begin(); it != end(); ++it) {
    if (0 == strncmp(pInput.c_str(),
                     (*it).inputSubStr.c_str(),
                     (*it).inputSubStr.length()))
      break;
    // wildcard to a user-defined output section.
    else if (0 == strcmp("*", (*it).inputSubStr.c_str()))
      break;
  }
  // if still no matching, just let a output seciton has the same input name
  if (it == end())
    return pInput;
  
  return (*it).outputStr;
}

bool SectionMap::push_back(const std::string& pInput,
                           const std::string& pOutput,
                           const uint64_t pOffset)
{
  // Now only check if the mapping exists in the map already
  // TODO: handle the cases such as overriding the exist mapping and drawing
  //       exception from the given SECTIONS command
  iterator it;
  for (it = begin(); it != end(); ++it) {
    if (pInput == (*it).inputSubStr)
      return false;
  }
  struct Mapping mapping = {
    pInput,
    pOutput,
    pOffset,
  };
  m_SectMap.push_back(mapping);
  return true;
}

SectionMap::iterator SectionMap::find(const std::string& pInput)
{
  iterator it;
  for (it = begin(); it != end(); ++it) {
    if(pInput == (*it).inputSubStr)
      break;
  }
  return it;
}

SectionMap::Mapping* SectionMap::at(const std::string& pInput)
{
  iterator it;
  for (it = begin(); it != end(); ++it) {
    if(pInput == (*it).inputSubStr)
      break;
  }
  if (end() == it)
    return NULL;
  return &(*it);
}

// Common mappings of ELF and other formants. Now only ELF specific mappings are added
const SectionMap::SectionNameMapping SectionMap::m_StdSectionMap[] =
{
  {".text", ".text"},
  {".rodata", ".rodata"},
  {".data.rel.ro.local", ".data.rel.ro.local"},
  {".data.rel.ro", ".data.rel.ro"},
  {".data", ".data"},
  {".bss", ".bss"},
  {".tdata", ".tdata"},
  {".tbss", ".tbss"},
  {".init_array", ".init_array"},
  {".fini_array", ".fini_array"},
  {".sdata", ".sdata"},
  {".sbss", ".sbss"},
  // FIXME: In the GNU linker, .sbss2 and .sdata2 are handled
  // differently depending on whether it is creating a shared library.
  {".sdata2", ".sdata"},
  {".sbss2", ".sbss"},
  {".lrodata", ".lrodata"},
  {".ldata", ".ldata"},
  {".lbss", ".lbss"},
  {".gcc_except_table", ".gcc_except_table"},
  {".gnu.linkonce.d.rel.ro.local", ".data.rel.ro.local"},
  {".gnu.linkonce.d.rel.ro", ".data.rel.ro"},
  {".gnu.linkonce.t", ".text"},
  {".gnu.linkonce.r", ".rodata"},
  {".gnu.linkonce.d", ".data"},
  {".gnu.linkonce.b", ".bss"},
  {".gnu.linkonce.s", ".sdata"},
  {".gnu.linkonce.sb", ".sbss"},
  {".gnu.linkonce.s2", ".sdata"},
  {".gnu.linkonce.sb2", ".sbss"},
  {".gnu.linkonce.wi", ".debug_info"},
  {".gnu.linkonce.td", ".tdata"},
  {".gnu.linkonce.tb", ".tbss"},
  {".gnu.linkonce.lr", ".lrodata"},
  {".gnu.linkonce.l", ".ldata"},
  {".gnu.linkonce.lb", ".lbss"},
};

const int SectionMap::m_StdSectionMapSize =
  (sizeof(SectionMap::m_StdSectionMap) / sizeof(SectionMap::m_StdSectionMap[0]));

bool SectionMap::addStdSectionMap()
{
  for (int i = 0; i < m_StdSectionMapSize; ++i) {
    if (!push_back(m_StdSectionMap[i].from, m_StdSectionMap[i].to))
      return false;
  }
  return true;
}
