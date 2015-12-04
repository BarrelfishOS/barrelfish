/* wincap.cc -- figure out on which OS we're running. Set the
		capability class to the appropriate values.

   Copyright 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011,
   2012, 2013, 2014, 2015 Red Hat, Inc.

This file is part of Cygwin.

This software is a copyrighted work licensed under the terms of the
Cygwin license.  Please consult the file "CYGWIN_LICENSE" for
details. */

#include "winsup.h"
#include "security.h"
#include "ntdll.h"

/* CV, 2008-10-23: All wincapc's have to be in the .cygwin_dll_common section,
   same as wincap itself.  Otherwise the capability changes made in
   wincapc::init() are not propagated to any subsequently started process
   in the same session.  I'm only writing this longish comment because I'm
   puzzled that this has never been noticed before... */

wincaps wincap_xpsp2 __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:1,
  max_sys_priv:SE_CREATE_GLOBAL_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:false,
  needs_count_in_si_lpres2:false,
  has_recycle_dot_bin:false,
  has_gaa_on_link_prefix:false,
  has_gaa_largeaddress_bug:false,
  supports_all_posix_ai_flags:false,
  has_restricted_stack_args:false,
  has_transactions:false,
  has_sendmsg:false,
  has_broken_udf:true,
  has_broken_alloc_console:false,
  has_always_all_codepages:false,
  has_localenames:false,
  has_fast_cwd:false,
  has_restricted_raw_disk_access:false,
  use_dont_resolve_hack:true,
  has_console_logon_sid:false,
  wow64_has_secondary_stack:false,
  has_program_compatibility_assistant:false,
  has_pipe_reject_remote_clients:false,
  terminate_thread_frees_stack:false,
  has_precise_system_time:false,
  has_microsoft_accounts:false,
  has_set_thread_stack_guarantee:false,
  has_broken_rtl_query_process_debug_information:false,
  has_processor_groups:false,
  has_broken_prefetchvm:false,
};

wincaps wincap_2003 __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:1,
  max_sys_priv:SE_CREATE_GLOBAL_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:false,
  needs_count_in_si_lpres2:false,
  has_recycle_dot_bin:false,
  has_gaa_on_link_prefix:false,
  has_gaa_largeaddress_bug:false,
  supports_all_posix_ai_flags:false,
  has_restricted_stack_args:true,
  has_transactions:false,
  has_sendmsg:false,
  has_broken_udf:true,
  has_broken_alloc_console:false,
  has_always_all_codepages:false,
  has_localenames:false,
  has_fast_cwd:false,
  has_restricted_raw_disk_access:false,
  use_dont_resolve_hack:true,
  has_console_logon_sid:false,
  wow64_has_secondary_stack:true,
  has_program_compatibility_assistant:false,
  has_pipe_reject_remote_clients:false,
  terminate_thread_frees_stack:false,
  has_precise_system_time:false,
  has_microsoft_accounts:false,
  has_set_thread_stack_guarantee:true,
  has_broken_rtl_query_process_debug_information:true,
  has_processor_groups:false,
  has_broken_prefetchvm:false,
};

wincaps wincap_vista __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:1,
  max_sys_priv:SE_CREATE_SYMBOLIC_LINK_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:true,
  needs_count_in_si_lpres2:true,
  has_recycle_dot_bin:true,
  has_gaa_on_link_prefix:true,
  has_gaa_largeaddress_bug:true,
  supports_all_posix_ai_flags:true,
  has_restricted_stack_args:false,
  has_transactions:true,
  has_sendmsg:true,
  has_broken_udf:false,
  has_broken_alloc_console:false,
  has_always_all_codepages:true,
  has_localenames:true,
  has_fast_cwd:true,
  has_restricted_raw_disk_access:true,
  use_dont_resolve_hack:false,
  has_console_logon_sid:false,
  wow64_has_secondary_stack:false,
  has_program_compatibility_assistant:true,
  has_pipe_reject_remote_clients:true,
  terminate_thread_frees_stack:true,
  has_precise_system_time:false,
  has_microsoft_accounts:false,
  has_set_thread_stack_guarantee:true,
  has_broken_rtl_query_process_debug_information:false,
  has_processor_groups:false,
  has_broken_prefetchvm:false,
};

wincaps wincap_7 __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:1,
  max_sys_priv:SE_CREATE_SYMBOLIC_LINK_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:true,
  needs_count_in_si_lpres2:false,
  has_recycle_dot_bin:true,
  has_gaa_on_link_prefix:true,
  has_gaa_largeaddress_bug:true,
  supports_all_posix_ai_flags:true,
  has_restricted_stack_args:false,
  has_transactions:true,
  has_sendmsg:true,
  has_broken_udf:false,
  has_broken_alloc_console:true,
  has_always_all_codepages:true,
  has_localenames:true,
  has_fast_cwd:true,
  has_restricted_raw_disk_access:true,
  use_dont_resolve_hack:false,
  has_console_logon_sid:true,
  wow64_has_secondary_stack:false,
  has_program_compatibility_assistant:true,
  has_pipe_reject_remote_clients:true,
  terminate_thread_frees_stack:true,
  has_precise_system_time:false,
  has_microsoft_accounts:false,
  has_set_thread_stack_guarantee:true,
  has_broken_rtl_query_process_debug_information:false,
  has_processor_groups:true,
  has_broken_prefetchvm:false,
};

wincaps wincap_8 __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:2,
  max_sys_priv:SE_CREATE_SYMBOLIC_LINK_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:true,
  needs_count_in_si_lpres2:false,
  has_recycle_dot_bin:true,
  has_gaa_on_link_prefix:true,
  has_gaa_largeaddress_bug:false,
  supports_all_posix_ai_flags:true,
  has_restricted_stack_args:false,
  has_transactions:true,
  has_sendmsg:true,
  has_broken_udf:false,
  has_broken_alloc_console:true,
  has_always_all_codepages:true,
  has_localenames:true,
  has_fast_cwd:true,
  has_restricted_raw_disk_access:true,
  use_dont_resolve_hack:false,
  has_console_logon_sid:true,
  wow64_has_secondary_stack:false,
  has_program_compatibility_assistant:true,
  has_pipe_reject_remote_clients:true,
  terminate_thread_frees_stack:true,
  has_precise_system_time:true,
  has_microsoft_accounts:true,
  has_set_thread_stack_guarantee:true,
  has_broken_rtl_query_process_debug_information:false,
  has_processor_groups:true,
  has_broken_prefetchvm:false,
};

wincaps wincap_10 __attribute__((section (".cygwin_dll_common"), shared)) = {
  def_guard_pages:2,
  max_sys_priv:SE_CREATE_SYMBOLIC_LINK_PRIVILEGE,
  is_server:false,
  has_mandatory_integrity_control:true,
  needs_count_in_si_lpres2:false,
  has_recycle_dot_bin:true,
  has_gaa_on_link_prefix:true,
  has_gaa_largeaddress_bug:false,
  supports_all_posix_ai_flags:true,
  has_restricted_stack_args:false,
  has_transactions:true,
  has_sendmsg:true,
  has_broken_udf:false,
  has_broken_alloc_console:true,
  has_always_all_codepages:true,
  has_localenames:true,
  has_fast_cwd:true,
  has_restricted_raw_disk_access:true,
  use_dont_resolve_hack:false,
  has_console_logon_sid:true,
  wow64_has_secondary_stack:false,
  has_program_compatibility_assistant:true,
  has_pipe_reject_remote_clients:true,
  terminate_thread_frees_stack:true,
  has_precise_system_time:true,
  has_microsoft_accounts:true,
  has_set_thread_stack_guarantee:true,
  has_broken_rtl_query_process_debug_information:false,
  has_processor_groups:true,
  has_broken_prefetchvm:true,
};

wincapc wincap __attribute__((section (".cygwin_dll_common"), shared));

void
wincapc::init ()
{
  if (caps)
    return;		// already initialized

  GetSystemInfo (&system_info);
  version.dwOSVersionInfoSize = sizeof (RTL_OSVERSIONINFOEXW);
  RtlGetVersion (&version);

  switch (version.dwMajorVersion)
    {
      case 5:
	switch (version.dwMinorVersion)
	  {
	    case 1:
	      caps = &wincap_xpsp2;
	      break;

	    default:
	      caps = &wincap_2003;
	  }
	break;
      case 6:
	switch (version.dwMinorVersion)
	  {
	    case 0:
	      caps = &wincap_vista;
	      break;
	    case 1:
	      caps = &wincap_7;
	      break;
	    case 2:
	    case 3:
	      caps = &wincap_8;
	      break;
	    default:
	      caps = &wincap_10;
	      break;
	  }
	break;
      case 10:
      default:
	caps = &wincap_10;
	break;
    }

  ((wincaps *)caps)->is_server = (version.wProductType != VER_NT_WORKSTATION);
#ifdef __x86_64__
  wow64 = 0;
  /* 64 bit systems have one more guard page than their 32 bit counterpart. */
  ++((wincaps *)caps)->def_guard_pages;
#else
  /* RtlQueryProcessDebugInformation/CreateToolhelp32Snapshot both crash the
     target process on 64 bit XP/2003 in native 64 bit mode only.  Reset the
     flag here for 32 bit. */
  ((wincaps *)caps)->has_broken_rtl_query_process_debug_information = false;
  if (NT_SUCCESS (NtQueryInformationProcess (NtCurrentProcess (),
					     ProcessWow64Information,
					     &wow64, sizeof wow64, NULL))
      && !wow64)
#endif
    {
      ((wincaps *)caps)->needs_count_in_si_lpres2 = false;
      ((wincaps *)caps)->has_restricted_stack_args = false;
      ((wincaps *)caps)->wow64_has_secondary_stack = false;
      ((wincaps *)caps)->has_gaa_largeaddress_bug = false;
      ((wincaps *)caps)->has_broken_prefetchvm = false;
    }

  __small_sprintf (osnam, "NT-%d.%d", version.dwMajorVersion,
		   version.dwMinorVersion);
}
