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

#ifdef BARRELFISH
extern "C" {
extern void bf_unwind_get_eh(uint64_t *eh_frame, uint64_t *eh_frame_size);
extern void bf_unwind_get_eh_hdr(uint64_t *eh_frame_hdr, uint64_t *eh_frame_hdr_size);
}
#endif

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

#if 0


inline bool LocalAddressSpace::findUnwindSections(pint_t targetAddr,
                                                  UnwindInfoSections &info) {
#if defined(_LIBUNWIND_SUPPORT_DWARF_UNWIND) && defined(_LIBUNWIND_IS_BAREMETAL)
  // Bare metal is statically linked, so no need to ask the dynamic loader
  info.dwarf_section_length = (uintptr_t)(&__eh_frame_end - &__eh_frame_start);
  info.dwarf_section =        (uintptr_t)(&__eh_frame_start);
  _LIBUNWIND_TRACE_UNWINDING("findUnwindSections: section %p length %p",
                             (void *)info.dwarf_section, (void *)info.dwarf_section_length);
#if defined(_LIBUNWIND_SUPPORT_DWARF_INDEX)
  info.dwarf_index_section =        (uintptr_t)(&__eh_frame_hdr_start);
  info.dwarf_index_section_length = (uintptr_t)(&__eh_frame_hdr_end - &__eh_frame_hdr_start);
  _LIBUNWIND_TRACE_UNWINDING("findUnwindSections: index section %p length %p",
                             (void *)info.dwarf_index_section, (void *)info.dwarf_index_section_length);
#endif
  if (info.dwarf_section_length)
    return true;
#elif defined(_LIBUNWIND_ARM_EHABI) && defined(_LIBUNWIND_IS_BAREMETAL)
  // Bare metal is statically linked, so no need to ask the dynamic loader
  info.arm_section =        (uintptr_t)(&__exidx_start);
  info.arm_section_length = (uintptr_t)(&__exidx_end - &__exidx_start);
  _LIBUNWIND_TRACE_UNWINDING("findUnwindSections: section %p length %p",
                             (void *)info.arm_section, (void *)info.arm_section_length);
  if (info.arm_section && info.arm_section_length)
    return true;
#elif defined(_LIBUNWIND_SUPPORT_DWARF_UNWIND) && defined(_WIN32)
  HMODULE mods[1024];
  HANDLE process = GetCurrentProcess();
  DWORD needed;

  if (!EnumProcessModules(process, mods, sizeof(mods), &needed))
    return false;

  for (unsigned i = 0; i < (needed / sizeof(HMODULE)); i++) {
    PIMAGE_DOS_HEADER pidh = (PIMAGE_DOS_HEADER)mods[i];
    PIMAGE_NT_HEADERS pinh = (PIMAGE_NT_HEADERS)((BYTE *)pidh + pidh->e_lfanew);
    PIMAGE_FILE_HEADER pifh = (PIMAGE_FILE_HEADER)&pinh->FileHeader;
    PIMAGE_SECTION_HEADER pish = IMAGE_FIRST_SECTION(pinh);
    bool found_obj = false;
    bool found_hdr = false;

    info.dso_base = (uintptr_t)mods[i];
    for (unsigned j = 0; j < pifh->NumberOfSections; j++, pish++) {
      uintptr_t begin = pish->VirtualAddress + (uintptr_t)mods[i];
      uintptr_t end = begin + pish->Misc.VirtualSize;
      if (!strncmp((const char *)pish->Name, ".text",
                   IMAGE_SIZEOF_SHORT_NAME)) {
        if (targetAddr >= begin && targetAddr < end)
          found_obj = true;
      } else if (!strncmp((const char *)pish->Name, ".eh_frame",
                          IMAGE_SIZEOF_SHORT_NAME)) {
        info.dwarf_section = begin;
        info.dwarf_section_length = pish->Misc.VirtualSize;
        found_hdr = true;
      }
      if (found_obj && found_hdr)
        return true;
    }
  }
  return false;
#elif defined(_LIBUNWIND_SUPPORT_SEH_UNWIND) && defined(_WIN32)
  // Don't even bother, since Windows has functions that do all this stuff
  // for us.
  (void)targetAddr;
  (void)info;
  return true;
#elif defined(_LIBUNWIND_ARM_EHABI) && defined(__BIONIC__) &&                  \
    (__ANDROID_API__ < 21)
  int length = 0;
  info.arm_section =
      (uintptr_t)dl_unwind_find_exidx((_Unwind_Ptr)targetAddr, &length);
  info.arm_section_length = (uintptr_t)length;
  if (info.arm_section && info.arm_section_length)
    return true;
#elif defined(_LIBUNWIND_ARM_EHABI) || defined(_LIBUNWIND_SUPPORT_DWARF_UNWIND)
  struct dl_iterate_cb_data {
    LocalAddressSpace *addressSpace;
    UnwindInfoSections *sects;
    uintptr_t targetAddr;
  };

  dl_iterate_cb_data cb_data = {this, &info, targetAddr};
  int found = dl_iterate_phdr(
      [](struct dl_phdr_info *pinfo, size_t, void *data) -> int {
        auto cbdata = static_cast<dl_iterate_cb_data *>(data);
        bool found_obj = false;
        bool found_hdr = false;

        assert(cbdata);
        assert(cbdata->sects);

        if (cbdata->targetAddr < pinfo->dlpi_addr) {
          return false;
        }

#if !defined(Elf_Half)
        typedef ElfW(Half) Elf_Half;
#endif
#if !defined(Elf_Phdr)
        typedef ElfW(Phdr) Elf_Phdr;
#endif
#if !defined(Elf_Addr) && defined(__ANDROID__)
        typedef ElfW(Addr) Elf_Addr;
#endif

 #if defined(_LIBUNWIND_SUPPORT_DWARF_UNWIND)
  #if !defined(_LIBUNWIND_SUPPORT_DWARF_INDEX)
   #error "_LIBUNWIND_SUPPORT_DWARF_UNWIND requires _LIBUNWIND_SUPPORT_DWARF_INDEX on this platform."
  #endif
        size_t object_length;
        for (Elf_Half i = 0; i < pinfo->dlpi_phnum; i++) {
          const Elf_Phdr *phdr = &pinfo->dlpi_phdr[i];
          if (phdr->p_type == PT_LOAD) {
            uintptr_t begin = pinfo->dlpi_addr + phdr->p_vaddr;
            uintptr_t end = begin + phdr->p_memsz;
            if (cbdata->targetAddr >= begin && cbdata->targetAddr < end) {
              cbdata->sects->dso_base = begin;
              object_length = phdr->p_memsz;
              found_obj = true;
            }
          } else if (phdr->p_type == PT_GNU_EH_FRAME) {
            EHHeaderParser<LocalAddressSpace>::EHHeaderInfo hdrInfo;
            uintptr_t eh_frame_hdr_start = pinfo->dlpi_addr + phdr->p_vaddr;
            cbdata->sects->dwarf_index_section = eh_frame_hdr_start;
            cbdata->sects->dwarf_index_section_length = phdr->p_memsz;
            found_hdr = EHHeaderParser<LocalAddressSpace>::decodeEHHdr(
                *cbdata->addressSpace, eh_frame_hdr_start, phdr->p_memsz,
                hdrInfo);
            if (found_hdr)
              cbdata->sects->dwarf_section = hdrInfo.eh_frame_ptr;
          }
        }

        if (found_obj && found_hdr) {
          cbdata->sects->dwarf_section_length = object_length;
          return true;
        } else {
          return false;
        }
 #else // defined(_LIBUNWIND_ARM_EHABI)
        for (Elf_Half i = 0; i < pinfo->dlpi_phnum; i++) {
          const Elf_Phdr *phdr = &pinfo->dlpi_phdr[i];
          if (phdr->p_type == PT_LOAD) {
            uintptr_t begin = pinfo->dlpi_addr + phdr->p_vaddr;
            uintptr_t end = begin + phdr->p_memsz;
            if (cbdata->targetAddr >= begin && cbdata->targetAddr < end)
              found_obj = true;
          } else if (phdr->p_type == PT_ARM_EXIDX) {
            uintptr_t exidx_start = pinfo->dlpi_addr + phdr->p_vaddr;
            cbdata->sects->arm_section = exidx_start;
            cbdata->sects->arm_section_length = phdr->p_memsz;
            found_hdr = true;
          }
        }
        return found_obj && found_hdr;
 #endif
      },
      &cb_data);
  return static_cast<bool>(found);
#endif

  return false;
}

#endif