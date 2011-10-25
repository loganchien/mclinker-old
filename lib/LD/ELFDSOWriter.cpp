//===- ELFDSOWriter.cpp ---------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the ELFDSOWriter,
// which writes the result of linking into a ELF .so file.
//
//===----------------------------------------------------------------------===//

#include "mcld/LD/ELFDSOWriter.h"

using namespace mcld;

void ELFDSOWriter::writeHeader(){
  RegionWriter Writer(
    m_pArea->request(0, sizeof(llvm::ELF::Elf32_Ehdr)), false);

  Writer.Write8(0x7f); // e_ident[EI_MAG0]
  Writer.Write8('E');  // e_ident[EI_MAG1]
  Writer.Write8('L');  // e_ident[EI_MAG2]
  Writer.Write8('F');  // e_ident[EI_MAG3]
}


void ELFDSOWriter::writeFile() {

  writeHeader();

}
