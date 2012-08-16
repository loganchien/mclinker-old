//===- LinkerConfig.cpp ---------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include <mcld/LinkerConfig.h>
#include <mcld/Config/Config.h>
#include <mcld/MC/InputFactory.h>
#include <mcld/MC/AttributeFactory.h>
#include <mcld/MC/ContextFactory.h>
#include <mcld/LD/NamePool.h>
#include <mcld/LD/StaticResolver.h>
#include <mcld/Support/FileSystem.h>
#include <string>

using namespace mcld;

//===----------------------------------------------------------------------===//
// LinkerConfig
//===----------------------------------------------------------------------===//
LinkerConfig::LinkerConfig(const std::string& pTripleString,
                           size_t pAttrNum,
                           size_t pInputNum)
  : m_Options(),
    m_Scripts(),
    m_pBitcode(NULL),
    m_Triple(pTripleString)
{
  m_pAttrFactory = new AttributeFactory(pAttrNum);
  m_pCntxtFactory = new ContextFactory(pInputNum);
  m_pInputFactory = new InputFactory(pInputNum, *m_pAttrFactory);
  m_pInputTree = new InputTree(*m_pInputFactory);
  m_pOutput = new mcld::Output();
  m_pResolver = new StaticResolver();
  m_pNamePool = new NamePool(*m_pResolver, 1024);
}

LinkerConfig::~LinkerConfig()
{
  delete m_pOutput;
  delete m_pAttrFactory;
  delete m_pCntxtFactory;
  delete m_pInputFactory;
  delete m_pInputTree;
  delete m_pResolver;
  delete m_pNamePool;
}

void LinkerConfig::setBitcode(const Input& pInput)
{
  m_pBitcode = const_cast<Input*>(&pInput);
}

Input& LinkerConfig::bitcode()
{
  assert((0 != m_pBitcode) && "default bitcode is not set");
  return *m_pBitcode;
}

const Input& LinkerConfig::bitcode() const
{
  assert((0 != m_pBitcode) && "default bitcode is not set");
  return *m_pBitcode;
}

const char* LinkerConfig::version()
{
  return MCLD_VERSION;
}
