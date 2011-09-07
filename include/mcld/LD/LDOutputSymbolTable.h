/*****************************************************************************
 *   The MCLinker Project, Copyright (C), 2011 -                             *
 *   Embedded and Web Computing Lab, National Taiwan University              *
 *   MediaTek, Inc.                                                          *
 *                                                                           *
 *   TDYa127 <a127a127@gmail.com>                                            *
 ****************************************************************************/
#ifndef LDOUTPUTSYMBOLTABLE_H
#define LDOUTPUTSYMBOLTABLE_H
#include <llvm/ADT/StringRef.h>
#include <mcld/LD/LDIOSymbolTableIF.h>
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif

namespace mcld
{

class LDSymbol;


/** \class LDOutputSymbolTable
 *  \brief Output symbol table, for MCLDOutput.
 *
 *  \see
 *  \author TDYa127 <a127a127@gmail.com>
 */
class LDOutputSymbolTable : public LDIOSymbolTableIF
{
  /* draft. */
friend class LDSymbolTableFactory;
private:
  LDOutputSymbolTable(LDSymbolTableStorage *symtab):LDIOSymbolTableIF(symtab){}
public:
  virtual void insertSymbol(llvm::StringRef) {}
  virtual bool merge(const LDSymbolTableIF *) {}
}

} // namespace of mcld

#endif

