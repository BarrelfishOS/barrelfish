//===------------------------- AddressSpace.hpp ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is dual licensed under the MIT and the University of Illinois Open
// Source Licenses. See LICENSE.TXT for details.
//
//
// Abstracts accessing local vs remote address spaces.
//
//===----------------------------------------------------------------------===//

extern "C" {
#include <unwind.h>
}
#include "config.h"
#include "AddressSpace.hpp"

#include "DwarfParser.hpp"

using namespace libunwind;

bool LocalAddressSpace::findUnwindSections(pint_t targetAddr,
                                           UnwindInfoSections &info)
{
#ifdef BARRELFISH
    bool result = 0;

    uint64_t eh_frame = 0;
    uint64_t eh_frame_size = 0;
    bf_unwind_get_eh(&eh_frame, &eh_frame_size);

    if (eh_frame != 0 && eh_frame_size != 0) {
#if _LIBUNWIND_SUPPORT_DWARF_UNWIND
        info.dwarf_section = eh_frame;
        info.dwarf_section_length = eh_frame_size;
        result = 1;
#endif
#if _LIBUNWIND_SUPPORT_COMPACT_UNWIND
        info.compact_unwind_section = eh_frame_ptr;
        info.compact_unwind_section_length = eh_frame_size;
#endif
    }

    uint64_t eh_frame_hdr;
    uint64_t eh_frame_hdr_size;
    bf_unwind_get_eh_hdr(&eh_frame_hdr, &eh_frame_hdr_size);
    if (eh_frame_hdr != 0 && eh_frame_hdr_size != 0) {
#if _LIBUNWIND_SUPPORT_DWARF_INDEX
        info.dwarf_index_section = eh_frame_hdr;
        info.dwarf_index_section_length = eh_frame_hdr_size;
#endif
#if _LIBUNWIND_SUPPORT_DWARF_UNWIND
        if (eh_frame == 0 || eh_frame_size == 0) {
            uint8_t *hdr = (uint8_t *) eh_frame;
            pint_t addr = (pint_t) (hdr + 4);
            pint_t eh_frame_ptr = LocalAddressSpace::getEncodedP(addr, addr + 16, hdr[1]);
            info.dwarf_section = eh_frame_ptr;
            info.dwarf_section_length = 0xffffffff;
            result = 1;
        }
#endif
    }

    return result;
#endif
    return false;
}
