/*****************************************************************************
 *   The MCLinker Project, Copyright (C), 2011 -                             *
 *   Embedded and Web Computing Lab, National Taiwan University              *
 *   MediaTek, Inc.                                                          *
 *                                                                           *
 *   Luba Tang <lubatang@mediatek.com>                                       *
 *   Nowar Gu <nowar100@gmail.com>                                           *
 ****************************************************************************/
#ifndef ARMELFOBJECTREADER_H
#define ARMELFOBJECTREADER_H
#ifdef ENABLE_UNITTEST
#include <gtest.h>
#endif
#include <mcld/MC/MCELFObjectTargetReader.h>

namespace mcld
{

/** \class ARMELFObjectReader
 *  \brief ARMELFObjectReader is a target-dependent ELF object reader.
 *
 *  \see
 *  \author Luba Tang <lubatang@mediatek.com>
 */
class ARMELFObjectReader : public MCELFObjectTargetReader
{
public:
  virtual bool hasRelocationAddend();
  virtual unsigned getRelocType(const MCValue&,
                                const MCFixup&,
                                bool IsPCRel,
                                bool IsRelocWithSymbol,
                                int64_t);
  virtual const MCSymbol* explicitRelSym(const MCAssembler&,
                                         const MCValue&,
                                         const MCFragment&,
                                         const MCFixup&,
                                         bool) const;
};

} // namespace of mcld

#endif

