# Microsoft Developer Studio Generated NMAKE File, Based on GmpDll.dsp
!IF "$(CFG)" == ""
CFG=GmpDll - Win32 Debug
!MESSAGE No configuration specified. Defaulting to GmpDll - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "GmpDll - Win32 Release" && "$(CFG)" != "GmpDll - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GmpDll.mak" CFG="GmpDll - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GmpDll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "GmpDll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "GmpDll - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\gmp33.dll"

!ELSE 

ALL : "$(OUTDIR)\gmp33.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ansi2knr.obj"
	-@erase "$(INTDIR)\assert.obj"
	-@erase "$(INTDIR)\compat.obj"
	-@erase "$(INTDIR)\errno.obj"
	-@erase "$(INTDIR)\extract-dbl.obj"
	-@erase "$(INTDIR)\insert-dbl.obj"
	-@erase "$(INTDIR)\memory.obj"
	-@erase "$(INTDIR)\mp_bpl.obj"
	-@erase "$(INTDIR)\mp_clz_tab.obj"
	-@erase "$(INTDIR)\mp_minv_tab.obj"
	-@erase "$(INTDIR)\mp_set_fns.obj"
	-@erase "$(INTDIR)\rand.obj"
	-@erase "$(INTDIR)\randclr.obj"
	-@erase "$(INTDIR)\randdef.obj"
	-@erase "$(INTDIR)\randlc.obj"
	-@erase "$(INTDIR)\randlc2s.obj"
	-@erase "$(INTDIR)\randlc2x.obj"
	-@erase "$(INTDIR)\randraw.obj"
	-@erase "$(INTDIR)\rands.obj"
	-@erase "$(INTDIR)\randsd.obj"
	-@erase "$(INTDIR)\randsdui.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\version.obj"
	-@erase "$(OUTDIR)\gmp33.dll"
	-@erase "$(OUTDIR)\gmp33.exp"
	-@erase "$(OUTDIR)\gmp33.lib"
	-@erase ".\ReleaseMPN\add.obj"
	-@erase ".\ReleaseMPN\add_1.obj"
	-@erase ".\ReleaseMPN\add_n.obj"
	-@erase ".\ReleaseMPN\addmul_1.obj"
	-@erase ".\ReleaseMPN\addsub_n.obj"
	-@erase ".\ReleaseMPN\bdivmod.obj"
	-@erase ".\ReleaseMPN\cmp.obj"
	-@erase ".\ReleaseMPN\dc_divrem_n.obj"
	-@erase ".\ReleaseMPN\dive_1.obj"
	-@erase ".\ReleaseMPN\diveby3.obj"
	-@erase ".\ReleaseMPN\divis.obj"
	-@erase ".\ReleaseMPN\divrem.obj"
	-@erase ".\ReleaseMPN\divrem_1.obj"
	-@erase ".\ReleaseMPN\divrem_2.obj"
	-@erase ".\ReleaseMPN\dump.obj"
	-@erase ".\ReleaseMPN\fib2_ui.obj"
	-@erase ".\ReleaseMPN\gcd.obj"
	-@erase ".\ReleaseMPN\gcd_1.obj"
	-@erase ".\ReleaseMPN\gcdext.obj"
	-@erase ".\ReleaseMPN\get_str.obj"
	-@erase ".\ReleaseMPN\hamdist.obj"
	-@erase ".\ReleaseMPN\jacbase.obj"
	-@erase ".\ReleaseMPN\lshift.obj"
	-@erase ".\ReleaseMPN\mod_1.obj"
	-@erase ".\ReleaseMPN\mod_34lsub1.obj"
	-@erase ".\ReleaseMPN\mode1o.obj"
	-@erase ".\ReleaseMPN\mp_bases.obj"
	-@erase ".\ReleaseMPN\mul.obj"
	-@erase ".\ReleaseMPN\mul_1.obj"
	-@erase ".\ReleaseMPN\mul_basecase.obj"
	-@erase ".\ReleaseMPN\mul_fft.obj"
	-@erase ".\ReleaseMPN\mul_n.obj"
	-@erase ".\ReleaseMPN\perfsqr.obj"
	-@erase ".\ReleaseMPN\popcount.obj"
	-@erase ".\ReleaseMPN\pow_1.obj"
	-@erase ".\ReleaseMPN\pre_divrem_1.obj"
	-@erase ".\ReleaseMPN\pre_mod_1.obj"
	-@erase ".\ReleaseMPN\random.obj"
	-@erase ".\ReleaseMPN\random2.obj"
	-@erase ".\ReleaseMPN\rootrem.obj"
	-@erase ".\ReleaseMPN\rshift.obj"
	-@erase ".\ReleaseMPN\sb_divrem_mn.obj"
	-@erase ".\ReleaseMPN\scan0.obj"
	-@erase ".\ReleaseMPN\scan1.obj"
	-@erase ".\ReleaseMPN\set_str.obj"
	-@erase ".\ReleaseMPN\sizeinbase.obj"
	-@erase ".\ReleaseMPN\sqr_basecase.obj"
	-@erase ".\ReleaseMPN\sqrtrem.obj"
	-@erase ".\ReleaseMPN\sub.obj"
	-@erase ".\ReleaseMPN\sub_1.obj"
	-@erase ".\ReleaseMPN\sub_n.obj"
	-@erase ".\ReleaseMPN\submul_1.obj"
	-@erase ".\ReleaseMPN\tdiv_qr.obj"
	-@erase ".\ReleaseMPN\vc50.idb"
	-@erase ".\ReleaseMPQ\abs.obj"
	-@erase ".\ReleaseMPQ\aors.obj"
	-@erase ".\ReleaseMPQ\canonicalize.obj"
	-@erase ".\ReleaseMPQ\clear.obj"
	-@erase ".\ReleaseMPQ\cmp.obj"
	-@erase ".\ReleaseMPQ\cmp_si.obj"
	-@erase ".\ReleaseMPQ\cmp_ui.obj"
	-@erase ".\ReleaseMPQ\div.obj"
	-@erase ".\ReleaseMPQ\equal.obj"
	-@erase ".\ReleaseMPQ\get_d.obj"
	-@erase ".\ReleaseMPQ\get_den.obj"
	-@erase ".\ReleaseMPQ\get_num.obj"
	-@erase ".\ReleaseMPQ\get_str.obj"
	-@erase ".\ReleaseMPQ\init.obj"
	-@erase ".\ReleaseMPQ\inp_str.obj"
	-@erase ".\ReleaseMPQ\inv.obj"
	-@erase ".\ReleaseMPQ\md_2exp.obj"
	-@erase ".\ReleaseMPQ\mul.obj"
	-@erase ".\ReleaseMPQ\neg.obj"
	-@erase ".\ReleaseMPQ\out_str.obj"
	-@erase ".\ReleaseMPQ\set.obj"
	-@erase ".\ReleaseMPQ\set_d.obj"
	-@erase ".\ReleaseMPQ\set_den.obj"
	-@erase ".\ReleaseMPQ\set_f.obj"
	-@erase ".\ReleaseMPQ\set_num.obj"
	-@erase ".\ReleaseMPQ\set_si.obj"
	-@erase ".\ReleaseMPQ\set_str.obj"
	-@erase ".\ReleaseMPQ\set_ui.obj"
	-@erase ".\ReleaseMPQ\set_z.obj"
	-@erase ".\ReleaseMPQ\swap.obj"
	-@erase ".\ReleaseMPQ\vc50.idb"
	-@erase ".\ReleaseMPZ\abs.obj"
	-@erase ".\ReleaseMPZ\add.obj"
	-@erase ".\ReleaseMPZ\add_ui.obj"
	-@erase ".\ReleaseMPZ\and.obj"
	-@erase ".\ReleaseMPZ\aorsmul.obj"
	-@erase ".\ReleaseMPZ\aorsmul_i.obj"
	-@erase ".\ReleaseMPZ\array_init.obj"
	-@erase ".\ReleaseMPZ\bin_ui.obj"
	-@erase ".\ReleaseMPZ\bin_uiui.obj"
	-@erase ".\ReleaseMPZ\cdiv_q.obj"
	-@erase ".\ReleaseMPZ\cdiv_q_ui.obj"
	-@erase ".\ReleaseMPZ\cdiv_qr.obj"
	-@erase ".\ReleaseMPZ\cdiv_qr_ui.obj"
	-@erase ".\ReleaseMPZ\cdiv_r.obj"
	-@erase ".\ReleaseMPZ\cdiv_r_ui.obj"
	-@erase ".\ReleaseMPZ\cdiv_ui.obj"
	-@erase ".\ReleaseMPZ\cfdiv_q_2exp.obj"
	-@erase ".\ReleaseMPZ\cfdiv_r_2exp.obj"
	-@erase ".\ReleaseMPZ\clear.obj"
	-@erase ".\ReleaseMPZ\clrbit.obj"
	-@erase ".\ReleaseMPZ\cmp.obj"
	-@erase ".\ReleaseMPZ\cmp_d.obj"
	-@erase ".\ReleaseMPZ\cmp_si.obj"
	-@erase ".\ReleaseMPZ\cmp_ui.obj"
	-@erase ".\ReleaseMPZ\cmpabs.obj"
	-@erase ".\ReleaseMPZ\cmpabs_d.obj"
	-@erase ".\ReleaseMPZ\cmpabs_ui.obj"
	-@erase ".\ReleaseMPZ\com.obj"
	-@erase ".\ReleaseMPZ\cong.obj"
	-@erase ".\ReleaseMPZ\cong_2exp.obj"
	-@erase ".\ReleaseMPZ\cong_ui.obj"
	-@erase ".\ReleaseMPZ\dive_ui.obj"
	-@erase ".\ReleaseMPZ\divegcd.obj"
	-@erase ".\ReleaseMPZ\divexact.obj"
	-@erase ".\ReleaseMPZ\divis.obj"
	-@erase ".\ReleaseMPZ\divis_2exp.obj"
	-@erase ".\ReleaseMPZ\divis_ui.obj"
	-@erase ".\ReleaseMPZ\dump.obj"
	-@erase ".\ReleaseMPZ\export.obj"
	-@erase ".\ReleaseMPZ\fac_ui.obj"
	-@erase ".\ReleaseMPZ\fdiv_q.obj"
	-@erase ".\ReleaseMPZ\fdiv_q_ui.obj"
	-@erase ".\ReleaseMPZ\fdiv_qr.obj"
	-@erase ".\ReleaseMPZ\fdiv_qr_ui.obj"
	-@erase ".\ReleaseMPZ\fdiv_r.obj"
	-@erase ".\ReleaseMPZ\fdiv_r_ui.obj"
	-@erase ".\ReleaseMPZ\fdiv_ui.obj"
	-@erase ".\ReleaseMPZ\fib2_ui.obj"
	-@erase ".\ReleaseMPZ\fib_ui.obj"
	-@erase ".\ReleaseMPZ\fits_sint.obj"
	-@erase ".\ReleaseMPZ\fits_slong.obj"
	-@erase ".\ReleaseMPZ\fits_sshort.obj"
	-@erase ".\ReleaseMPZ\fits_uint.obj"
	-@erase ".\ReleaseMPZ\fits_ulong.obj"
	-@erase ".\ReleaseMPZ\fits_ushort.obj"
	-@erase ".\ReleaseMPZ\gcd.obj"
	-@erase ".\ReleaseMPZ\gcd_ui.obj"
	-@erase ".\ReleaseMPZ\gcdext.obj"
	-@erase ".\ReleaseMPZ\get_d.obj"
	-@erase ".\ReleaseMPZ\get_d_2exp.obj"
	-@erase ".\ReleaseMPZ\get_si.obj"
	-@erase ".\ReleaseMPZ\get_str.obj"
	-@erase ".\ReleaseMPZ\get_ui.obj"
	-@erase ".\ReleaseMPZ\getlimbn.obj"
	-@erase ".\ReleaseMPZ\hamdist.obj"
	-@erase ".\ReleaseMPZ\import.obj"
	-@erase ".\ReleaseMPZ\init.obj"
	-@erase ".\ReleaseMPZ\init2.obj"
	-@erase ".\ReleaseMPZ\inp_raw.obj"
	-@erase ".\ReleaseMPZ\inp_str.obj"
	-@erase ".\ReleaseMPZ\invert.obj"
	-@erase ".\ReleaseMPZ\ior.obj"
	-@erase ".\ReleaseMPZ\iset.obj"
	-@erase ".\ReleaseMPZ\iset_d.obj"
	-@erase ".\ReleaseMPZ\iset_si.obj"
	-@erase ".\ReleaseMPZ\iset_str.obj"
	-@erase ".\ReleaseMPZ\iset_ui.obj"
	-@erase ".\ReleaseMPZ\jacobi.obj"
	-@erase ".\ReleaseMPZ\kronsz.obj"
	-@erase ".\ReleaseMPZ\kronuz.obj"
	-@erase ".\ReleaseMPZ\kronzs.obj"
	-@erase ".\ReleaseMPZ\kronzu.obj"
	-@erase ".\ReleaseMPZ\lcm.obj"
	-@erase ".\ReleaseMPZ\lcm_ui.obj"
	-@erase ".\ReleaseMPZ\lucnum2_ui.obj"
	-@erase ".\ReleaseMPZ\lucnum_ui.obj"
	-@erase ".\ReleaseMPZ\millerrabin.obj"
	-@erase ".\ReleaseMPZ\mod.obj"
	-@erase ".\ReleaseMPZ\mul.obj"
	-@erase ".\ReleaseMPZ\mul_2exp.obj"
	-@erase ".\ReleaseMPZ\mul_si.obj"
	-@erase ".\ReleaseMPZ\mul_ui.obj"
	-@erase ".\ReleaseMPZ\n_pow_ui.obj"
	-@erase ".\ReleaseMPZ\neg.obj"
	-@erase ".\ReleaseMPZ\nextprime.obj"
	-@erase ".\ReleaseMPZ\out_raw.obj"
	-@erase ".\ReleaseMPZ\out_str.obj"
	-@erase ".\ReleaseMPZ\perfpow.obj"
	-@erase ".\ReleaseMPZ\perfsqr.obj"
	-@erase ".\ReleaseMPZ\popcount.obj"
	-@erase ".\ReleaseMPZ\pow_ui.obj"
	-@erase ".\ReleaseMPZ\powm.obj"
	-@erase ".\ReleaseMPZ\powm_ui.obj"
	-@erase ".\ReleaseMPZ\pprime_p.obj"
	-@erase ".\ReleaseMPZ\random.obj"
	-@erase ".\ReleaseMPZ\random2.obj"
	-@erase ".\ReleaseMPZ\realloc.obj"
	-@erase ".\ReleaseMPZ\realloc2.obj"
	-@erase ".\ReleaseMPZ\remove.obj"
	-@erase ".\ReleaseMPZ\root.obj"
	-@erase ".\ReleaseMPZ\rrandomb.obj"
	-@erase ".\ReleaseMPZ\scan0.obj"
	-@erase ".\ReleaseMPZ\scan1.obj"
	-@erase ".\ReleaseMPZ\set.obj"
	-@erase ".\ReleaseMPZ\set_d.obj"
	-@erase ".\ReleaseMPZ\set_f.obj"
	-@erase ".\ReleaseMPZ\set_q.obj"
	-@erase ".\ReleaseMPZ\set_si.obj"
	-@erase ".\ReleaseMPZ\set_str.obj"
	-@erase ".\ReleaseMPZ\set_ui.obj"
	-@erase ".\ReleaseMPZ\setbit.obj"
	-@erase ".\ReleaseMPZ\size.obj"
	-@erase ".\ReleaseMPZ\sizeinbase.obj"
	-@erase ".\ReleaseMPZ\sqrt.obj"
	-@erase ".\ReleaseMPZ\sqrtrem.obj"
	-@erase ".\ReleaseMPZ\sub.obj"
	-@erase ".\ReleaseMPZ\sub_ui.obj"
	-@erase ".\ReleaseMPZ\swap.obj"
	-@erase ".\ReleaseMPZ\tdiv_q.obj"
	-@erase ".\ReleaseMPZ\tdiv_q_2exp.obj"
	-@erase ".\ReleaseMPZ\tdiv_q_ui.obj"
	-@erase ".\ReleaseMPZ\tdiv_qr.obj"
	-@erase ".\ReleaseMPZ\tdiv_qr_ui.obj"
	-@erase ".\ReleaseMPZ\tdiv_r.obj"
	-@erase ".\ReleaseMPZ\tdiv_r_2exp.obj"
	-@erase ".\ReleaseMPZ\tdiv_r_ui.obj"
	-@erase ".\ReleaseMPZ\tdiv_ui.obj"
	-@erase ".\ReleaseMPZ\tstbit.obj"
	-@erase ".\ReleaseMPZ\ui_pow_ui.obj"
	-@erase ".\ReleaseMPZ\ui_sub.obj"
	-@erase ".\ReleaseMPZ\urandomb.obj"
	-@erase ".\ReleaseMPZ\urandomm.obj"
	-@erase ".\ReleaseMPZ\vc50.idb"
	-@erase ".\ReleaseMPZ\xor.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse" /I\
 "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER" /D\
 "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"$(INTDIR)\GmpDll.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Release/
CPP_SBRS=.
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\GmpDll.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /version:3.1 /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)\gmp33.pdb" /machine:I386 /out:"$(OUTDIR)\gmp33.dll"\
 /implib:"$(OUTDIR)\gmp33.lib" 
LINK32_OBJS= \
	"$(INTDIR)\ansi2knr.obj" \
	"$(INTDIR)\assert.obj" \
	"$(INTDIR)\compat.obj" \
	"$(INTDIR)\errno.obj" \
	"$(INTDIR)\extract-dbl.obj" \
	"$(INTDIR)\insert-dbl.obj" \
	"$(INTDIR)\memory.obj" \
	"$(INTDIR)\mp_bpl.obj" \
	"$(INTDIR)\mp_clz_tab.obj" \
	"$(INTDIR)\mp_minv_tab.obj" \
	"$(INTDIR)\mp_set_fns.obj" \
	"$(INTDIR)\rand.obj" \
	"$(INTDIR)\randclr.obj" \
	"$(INTDIR)\randdef.obj" \
	"$(INTDIR)\randlc.obj" \
	"$(INTDIR)\randlc2s.obj" \
	"$(INTDIR)\randlc2x.obj" \
	"$(INTDIR)\randraw.obj" \
	"$(INTDIR)\rands.obj" \
	"$(INTDIR)\randsd.obj" \
	"$(INTDIR)\randsdui.obj" \
	"$(INTDIR)\version.obj" \
	".\ReleaseMPN\add.obj" \
	".\ReleaseMPN\add_1.obj" \
	".\ReleaseMPN\add_n.obj" \
	".\ReleaseMPN\addmul_1.obj" \
	".\ReleaseMPN\addsub_n.obj" \
	".\ReleaseMPN\bdivmod.obj" \
	".\ReleaseMPN\cmp.obj" \
	".\ReleaseMPN\dc_divrem_n.obj" \
	".\ReleaseMPN\dive_1.obj" \
	".\ReleaseMPN\diveby3.obj" \
	".\ReleaseMPN\divis.obj" \
	".\ReleaseMPN\divrem.obj" \
	".\ReleaseMPN\divrem_1.obj" \
	".\ReleaseMPN\divrem_2.obj" \
	".\ReleaseMPN\dump.obj" \
	".\ReleaseMPN\fib2_ui.obj" \
	".\ReleaseMPN\gcd.obj" \
	".\ReleaseMPN\gcd_1.obj" \
	".\ReleaseMPN\gcdext.obj" \
	".\ReleaseMPN\get_str.obj" \
	".\ReleaseMPN\hamdist.obj" \
	".\ReleaseMPN\jacbase.obj" \
	".\ReleaseMPN\lshift.obj" \
	".\ReleaseMPN\mod_1.obj" \
	".\ReleaseMPN\mod_34lsub1.obj" \
	".\ReleaseMPN\mode1o.obj" \
	".\ReleaseMPN\mp_bases.obj" \
	".\ReleaseMPN\mul.obj" \
	".\ReleaseMPN\mul_1.obj" \
	".\ReleaseMPN\mul_basecase.obj" \
	".\ReleaseMPN\mul_fft.obj" \
	".\ReleaseMPN\mul_n.obj" \
	".\ReleaseMPN\perfsqr.obj" \
	".\ReleaseMPN\popcount.obj" \
	".\ReleaseMPN\pow_1.obj" \
	".\ReleaseMPN\pre_divrem_1.obj" \
	".\ReleaseMPN\pre_mod_1.obj" \
	".\ReleaseMPN\random.obj" \
	".\ReleaseMPN\random2.obj" \
	".\ReleaseMPN\rootrem.obj" \
	".\ReleaseMPN\rshift.obj" \
	".\ReleaseMPN\sb_divrem_mn.obj" \
	".\ReleaseMPN\scan0.obj" \
	".\ReleaseMPN\scan1.obj" \
	".\ReleaseMPN\set_str.obj" \
	".\ReleaseMPN\sizeinbase.obj" \
	".\ReleaseMPN\sqr_basecase.obj" \
	".\ReleaseMPN\sqrtrem.obj" \
	".\ReleaseMPN\sub.obj" \
	".\ReleaseMPN\sub_1.obj" \
	".\ReleaseMPN\sub_n.obj" \
	".\ReleaseMPN\submul_1.obj" \
	".\ReleaseMPN\tdiv_qr.obj" \
	".\ReleaseMPQ\abs.obj" \
	".\ReleaseMPQ\aors.obj" \
	".\ReleaseMPQ\canonicalize.obj" \
	".\ReleaseMPQ\clear.obj" \
	".\ReleaseMPQ\cmp.obj" \
	".\ReleaseMPQ\cmp_si.obj" \
	".\ReleaseMPQ\cmp_ui.obj" \
	".\ReleaseMPQ\div.obj" \
	".\ReleaseMPQ\equal.obj" \
	".\ReleaseMPQ\get_d.obj" \
	".\ReleaseMPQ\get_den.obj" \
	".\ReleaseMPQ\get_num.obj" \
	".\ReleaseMPQ\get_str.obj" \
	".\ReleaseMPQ\init.obj" \
	".\ReleaseMPQ\inp_str.obj" \
	".\ReleaseMPQ\inv.obj" \
	".\ReleaseMPQ\md_2exp.obj" \
	".\ReleaseMPQ\mul.obj" \
	".\ReleaseMPQ\neg.obj" \
	".\ReleaseMPQ\out_str.obj" \
	".\ReleaseMPQ\set.obj" \
	".\ReleaseMPQ\set_d.obj" \
	".\ReleaseMPQ\set_den.obj" \
	".\ReleaseMPQ\set_f.obj" \
	".\ReleaseMPQ\set_num.obj" \
	".\ReleaseMPQ\set_si.obj" \
	".\ReleaseMPQ\set_str.obj" \
	".\ReleaseMPQ\set_ui.obj" \
	".\ReleaseMPQ\set_z.obj" \
	".\ReleaseMPQ\swap.obj" \
	".\ReleaseMPZ\abs.obj" \
	".\ReleaseMPZ\add.obj" \
	".\ReleaseMPZ\add_ui.obj" \
	".\ReleaseMPZ\and.obj" \
	".\ReleaseMPZ\aorsmul.obj" \
	".\ReleaseMPZ\aorsmul_i.obj" \
	".\ReleaseMPZ\array_init.obj" \
	".\ReleaseMPZ\bin_ui.obj" \
	".\ReleaseMPZ\bin_uiui.obj" \
	".\ReleaseMPZ\cdiv_q.obj" \
	".\ReleaseMPZ\cdiv_q_ui.obj" \
	".\ReleaseMPZ\cdiv_qr.obj" \
	".\ReleaseMPZ\cdiv_qr_ui.obj" \
	".\ReleaseMPZ\cdiv_r.obj" \
	".\ReleaseMPZ\cdiv_r_ui.obj" \
	".\ReleaseMPZ\cdiv_ui.obj" \
	".\ReleaseMPZ\cfdiv_q_2exp.obj" \
	".\ReleaseMPZ\cfdiv_r_2exp.obj" \
	".\ReleaseMPZ\clear.obj" \
	".\ReleaseMPZ\clrbit.obj" \
	".\ReleaseMPZ\cmp.obj" \
	".\ReleaseMPZ\cmp_d.obj" \
	".\ReleaseMPZ\cmp_si.obj" \
	".\ReleaseMPZ\cmp_ui.obj" \
	".\ReleaseMPZ\cmpabs.obj" \
	".\ReleaseMPZ\cmpabs_d.obj" \
	".\ReleaseMPZ\cmpabs_ui.obj" \
	".\ReleaseMPZ\com.obj" \
	".\ReleaseMPZ\cong.obj" \
	".\ReleaseMPZ\cong_2exp.obj" \
	".\ReleaseMPZ\cong_ui.obj" \
	".\ReleaseMPZ\dive_ui.obj" \
	".\ReleaseMPZ\divegcd.obj" \
	".\ReleaseMPZ\divexact.obj" \
	".\ReleaseMPZ\divis.obj" \
	".\ReleaseMPZ\divis_2exp.obj" \
	".\ReleaseMPZ\divis_ui.obj" \
	".\ReleaseMPZ\dump.obj" \
	".\ReleaseMPZ\export.obj" \
	".\ReleaseMPZ\fac_ui.obj" \
	".\ReleaseMPZ\fdiv_q.obj" \
	".\ReleaseMPZ\fdiv_q_ui.obj" \
	".\ReleaseMPZ\fdiv_qr.obj" \
	".\ReleaseMPZ\fdiv_qr_ui.obj" \
	".\ReleaseMPZ\fdiv_r.obj" \
	".\ReleaseMPZ\fdiv_r_ui.obj" \
	".\ReleaseMPZ\fdiv_ui.obj" \
	".\ReleaseMPZ\fib2_ui.obj" \
	".\ReleaseMPZ\fib_ui.obj" \
	".\ReleaseMPZ\fits_sint.obj" \
	".\ReleaseMPZ\fits_slong.obj" \
	".\ReleaseMPZ\fits_sshort.obj" \
	".\ReleaseMPZ\fits_uint.obj" \
	".\ReleaseMPZ\fits_ulong.obj" \
	".\ReleaseMPZ\fits_ushort.obj" \
	".\ReleaseMPZ\gcd.obj" \
	".\ReleaseMPZ\gcd_ui.obj" \
	".\ReleaseMPZ\gcdext.obj" \
	".\ReleaseMPZ\get_d.obj" \
	".\ReleaseMPZ\get_d_2exp.obj" \
	".\ReleaseMPZ\get_si.obj" \
	".\ReleaseMPZ\get_str.obj" \
	".\ReleaseMPZ\get_ui.obj" \
	".\ReleaseMPZ\getlimbn.obj" \
	".\ReleaseMPZ\hamdist.obj" \
	".\ReleaseMPZ\import.obj" \
	".\ReleaseMPZ\init.obj" \
	".\ReleaseMPZ\init2.obj" \
	".\ReleaseMPZ\inp_raw.obj" \
	".\ReleaseMPZ\inp_str.obj" \
	".\ReleaseMPZ\invert.obj" \
	".\ReleaseMPZ\ior.obj" \
	".\ReleaseMPZ\iset.obj" \
	".\ReleaseMPZ\iset_d.obj" \
	".\ReleaseMPZ\iset_si.obj" \
	".\ReleaseMPZ\iset_str.obj" \
	".\ReleaseMPZ\iset_ui.obj" \
	".\ReleaseMPZ\jacobi.obj" \
	".\ReleaseMPZ\kronsz.obj" \
	".\ReleaseMPZ\kronuz.obj" \
	".\ReleaseMPZ\kronzs.obj" \
	".\ReleaseMPZ\kronzu.obj" \
	".\ReleaseMPZ\lcm.obj" \
	".\ReleaseMPZ\lcm_ui.obj" \
	".\ReleaseMPZ\lucnum2_ui.obj" \
	".\ReleaseMPZ\lucnum_ui.obj" \
	".\ReleaseMPZ\millerrabin.obj" \
	".\ReleaseMPZ\mod.obj" \
	".\ReleaseMPZ\mul.obj" \
	".\ReleaseMPZ\mul_2exp.obj" \
	".\ReleaseMPZ\mul_si.obj" \
	".\ReleaseMPZ\mul_ui.obj" \
	".\ReleaseMPZ\n_pow_ui.obj" \
	".\ReleaseMPZ\neg.obj" \
	".\ReleaseMPZ\nextprime.obj" \
	".\ReleaseMPZ\out_raw.obj" \
	".\ReleaseMPZ\out_str.obj" \
	".\ReleaseMPZ\perfpow.obj" \
	".\ReleaseMPZ\perfsqr.obj" \
	".\ReleaseMPZ\popcount.obj" \
	".\ReleaseMPZ\pow_ui.obj" \
	".\ReleaseMPZ\powm.obj" \
	".\ReleaseMPZ\powm_ui.obj" \
	".\ReleaseMPZ\pprime_p.obj" \
	".\ReleaseMPZ\random.obj" \
	".\ReleaseMPZ\random2.obj" \
	".\ReleaseMPZ\realloc.obj" \
	".\ReleaseMPZ\realloc2.obj" \
	".\ReleaseMPZ\remove.obj" \
	".\ReleaseMPZ\root.obj" \
	".\ReleaseMPZ\rrandomb.obj" \
	".\ReleaseMPZ\scan0.obj" \
	".\ReleaseMPZ\scan1.obj" \
	".\ReleaseMPZ\set.obj" \
	".\ReleaseMPZ\set_d.obj" \
	".\ReleaseMPZ\set_f.obj" \
	".\ReleaseMPZ\set_q.obj" \
	".\ReleaseMPZ\set_si.obj" \
	".\ReleaseMPZ\set_str.obj" \
	".\ReleaseMPZ\set_ui.obj" \
	".\ReleaseMPZ\setbit.obj" \
	".\ReleaseMPZ\size.obj" \
	".\ReleaseMPZ\sizeinbase.obj" \
	".\ReleaseMPZ\sqrt.obj" \
	".\ReleaseMPZ\sqrtrem.obj" \
	".\ReleaseMPZ\sub.obj" \
	".\ReleaseMPZ\sub_ui.obj" \
	".\ReleaseMPZ\swap.obj" \
	".\ReleaseMPZ\tdiv_q.obj" \
	".\ReleaseMPZ\tdiv_q_2exp.obj" \
	".\ReleaseMPZ\tdiv_q_ui.obj" \
	".\ReleaseMPZ\tdiv_qr.obj" \
	".\ReleaseMPZ\tdiv_qr_ui.obj" \
	".\ReleaseMPZ\tdiv_r.obj" \
	".\ReleaseMPZ\tdiv_r_2exp.obj" \
	".\ReleaseMPZ\tdiv_r_ui.obj" \
	".\ReleaseMPZ\tdiv_ui.obj" \
	".\ReleaseMPZ\tstbit.obj" \
	".\ReleaseMPZ\ui_pow_ui.obj" \
	".\ReleaseMPZ\ui_sub.obj" \
	".\ReleaseMPZ\urandomb.obj" \
	".\ReleaseMPZ\urandomm.obj" \
	".\ReleaseMPZ\xor.obj"

"$(OUTDIR)\gmp33.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying gmp dll to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\gmp33.dll"
   copy Release\gmp33.dll ..\..\lib\i386_nt\gmp33.dll
	copy   Release\gmp33.lib ..\..\lib\i386_nt\gmp33.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\gmp33.dll"

!ELSE 

ALL : "$(OUTDIR)\gmp33.dll"

!ENDIF 

CLEAN :
	-@erase "$(INTDIR)\ansi2knr.obj"
	-@erase "$(INTDIR)\assert.obj"
	-@erase "$(INTDIR)\compat.obj"
	-@erase "$(INTDIR)\errno.obj"
	-@erase "$(INTDIR)\extract-dbl.obj"
	-@erase "$(INTDIR)\insert-dbl.obj"
	-@erase "$(INTDIR)\memory.obj"
	-@erase "$(INTDIR)\mp_bpl.obj"
	-@erase "$(INTDIR)\mp_clz_tab.obj"
	-@erase "$(INTDIR)\mp_minv_tab.obj"
	-@erase "$(INTDIR)\mp_set_fns.obj"
	-@erase "$(INTDIR)\rand.obj"
	-@erase "$(INTDIR)\randclr.obj"
	-@erase "$(INTDIR)\randdef.obj"
	-@erase "$(INTDIR)\randlc.obj"
	-@erase "$(INTDIR)\randlc2s.obj"
	-@erase "$(INTDIR)\randlc2x.obj"
	-@erase "$(INTDIR)\randraw.obj"
	-@erase "$(INTDIR)\rands.obj"
	-@erase "$(INTDIR)\randsd.obj"
	-@erase "$(INTDIR)\randsdui.obj"
	-@erase "$(INTDIR)\vc50.idb"
	-@erase "$(INTDIR)\vc50.pdb"
	-@erase "$(INTDIR)\version.obj"
	-@erase "$(OUTDIR)\gmp33.dll"
	-@erase "$(OUTDIR)\gmp33.exp"
	-@erase "$(OUTDIR)\gmp33.ilk"
	-@erase "$(OUTDIR)\gmp33.lib"
	-@erase "$(OUTDIR)\gmp33.pdb"
	-@erase ".\DebugMPN\add.obj"
	-@erase ".\DebugMPN\add_1.obj"
	-@erase ".\DebugMPN\add_n.obj"
	-@erase ".\DebugMPN\addmul_1.obj"
	-@erase ".\DebugMPN\addsub_n.obj"
	-@erase ".\DebugMPN\bdivmod.obj"
	-@erase ".\DebugMPN\cmp.obj"
	-@erase ".\DebugMPN\dc_divrem_n.obj"
	-@erase ".\DebugMPN\dive_1.obj"
	-@erase ".\DebugMPN\diveby3.obj"
	-@erase ".\DebugMPN\divis.obj"
	-@erase ".\DebugMPN\divrem.obj"
	-@erase ".\DebugMPN\divrem_1.obj"
	-@erase ".\DebugMPN\divrem_2.obj"
	-@erase ".\DebugMPN\dump.obj"
	-@erase ".\DebugMPN\fib2_ui.obj"
	-@erase ".\DebugMPN\gcd.obj"
	-@erase ".\DebugMPN\gcd_1.obj"
	-@erase ".\DebugMPN\gcdext.obj"
	-@erase ".\DebugMPN\get_str.obj"
	-@erase ".\DebugMPN\hamdist.obj"
	-@erase ".\DebugMPN\jacbase.obj"
	-@erase ".\DebugMPN\lshift.obj"
	-@erase ".\DebugMPN\mod_1.obj"
	-@erase ".\DebugMPN\mod_34lsub1.obj"
	-@erase ".\DebugMPN\mode1o.obj"
	-@erase ".\DebugMPN\mp_bases.obj"
	-@erase ".\DebugMPN\mul.obj"
	-@erase ".\DebugMPN\mul_1.obj"
	-@erase ".\DebugMPN\mul_basecase.obj"
	-@erase ".\DebugMPN\mul_fft.obj"
	-@erase ".\DebugMPN\mul_n.obj"
	-@erase ".\DebugMPN\perfsqr.obj"
	-@erase ".\DebugMPN\popcount.obj"
	-@erase ".\DebugMPN\pow_1.obj"
	-@erase ".\DebugMPN\pre_divrem_1.obj"
	-@erase ".\DebugMPN\pre_mod_1.obj"
	-@erase ".\DebugMPN\random.obj"
	-@erase ".\DebugMPN\random2.obj"
	-@erase ".\DebugMPN\rootrem.obj"
	-@erase ".\DebugMPN\rshift.obj"
	-@erase ".\DebugMPN\sb_divrem_mn.obj"
	-@erase ".\DebugMPN\scan0.obj"
	-@erase ".\DebugMPN\scan1.obj"
	-@erase ".\DebugMPN\set_str.obj"
	-@erase ".\DebugMPN\sizeinbase.obj"
	-@erase ".\DebugMPN\sqr_basecase.obj"
	-@erase ".\DebugMPN\sqrtrem.obj"
	-@erase ".\DebugMPN\sub.obj"
	-@erase ".\DebugMPN\sub_1.obj"
	-@erase ".\DebugMPN\sub_n.obj"
	-@erase ".\DebugMPN\submul_1.obj"
	-@erase ".\DebugMPN\tdiv_qr.obj"
	-@erase ".\DebugMPN\vc50.idb"
	-@erase ".\DebugMPN\vc50.pdb"
	-@erase ".\DebugMPQ\abs.obj"
	-@erase ".\DebugMPQ\aors.obj"
	-@erase ".\DebugMPQ\canonicalize.obj"
	-@erase ".\DebugMPQ\clear.obj"
	-@erase ".\DebugMPQ\cmp.obj"
	-@erase ".\DebugMPQ\cmp_si.obj"
	-@erase ".\DebugMPQ\cmp_ui.obj"
	-@erase ".\DebugMPQ\div.obj"
	-@erase ".\DebugMPQ\equal.obj"
	-@erase ".\DebugMPQ\get_d.obj"
	-@erase ".\DebugMPQ\get_den.obj"
	-@erase ".\DebugMPQ\get_num.obj"
	-@erase ".\DebugMPQ\get_str.obj"
	-@erase ".\DebugMPQ\init.obj"
	-@erase ".\DebugMPQ\inp_str.obj"
	-@erase ".\DebugMPQ\inv.obj"
	-@erase ".\DebugMPQ\md_2exp.obj"
	-@erase ".\DebugMPQ\mul.obj"
	-@erase ".\DebugMPQ\neg.obj"
	-@erase ".\DebugMPQ\out_str.obj"
	-@erase ".\DebugMPQ\set.obj"
	-@erase ".\DebugMPQ\set_d.obj"
	-@erase ".\DebugMPQ\set_den.obj"
	-@erase ".\DebugMPQ\set_f.obj"
	-@erase ".\DebugMPQ\set_num.obj"
	-@erase ".\DebugMPQ\set_si.obj"
	-@erase ".\DebugMPQ\set_str.obj"
	-@erase ".\DebugMPQ\set_ui.obj"
	-@erase ".\DebugMPQ\set_z.obj"
	-@erase ".\DebugMPQ\swap.obj"
	-@erase ".\DebugMPQ\vc50.idb"
	-@erase ".\DebugMPQ\vc50.pdb"
	-@erase ".\DebugMPZ\abs.obj"
	-@erase ".\DebugMPZ\add.obj"
	-@erase ".\DebugMPZ\add_ui.obj"
	-@erase ".\DebugMPZ\and.obj"
	-@erase ".\DebugMPZ\aorsmul.obj"
	-@erase ".\DebugMPZ\aorsmul_i.obj"
	-@erase ".\DebugMPZ\array_init.obj"
	-@erase ".\DebugMPZ\bin_ui.obj"
	-@erase ".\DebugMPZ\bin_uiui.obj"
	-@erase ".\DebugMPZ\cdiv_q.obj"
	-@erase ".\DebugMPZ\cdiv_q_ui.obj"
	-@erase ".\DebugMPZ\cdiv_qr.obj"
	-@erase ".\DebugMPZ\cdiv_qr_ui.obj"
	-@erase ".\DebugMPZ\cdiv_r.obj"
	-@erase ".\DebugMPZ\cdiv_r_ui.obj"
	-@erase ".\DebugMPZ\cdiv_ui.obj"
	-@erase ".\DebugMPZ\cfdiv_q_2exp.obj"
	-@erase ".\DebugMPZ\cfdiv_r_2exp.obj"
	-@erase ".\DebugMPZ\clear.obj"
	-@erase ".\DebugMPZ\clrbit.obj"
	-@erase ".\DebugMPZ\cmp.obj"
	-@erase ".\DebugMPZ\cmp_d.obj"
	-@erase ".\DebugMPZ\cmp_si.obj"
	-@erase ".\DebugMPZ\cmp_ui.obj"
	-@erase ".\DebugMPZ\cmpabs.obj"
	-@erase ".\DebugMPZ\cmpabs_d.obj"
	-@erase ".\DebugMPZ\cmpabs_ui.obj"
	-@erase ".\DebugMPZ\com.obj"
	-@erase ".\DebugMPZ\cong.obj"
	-@erase ".\DebugMPZ\cong_2exp.obj"
	-@erase ".\DebugMPZ\cong_ui.obj"
	-@erase ".\DebugMPZ\dive_ui.obj"
	-@erase ".\DebugMPZ\divegcd.obj"
	-@erase ".\DebugMPZ\divexact.obj"
	-@erase ".\DebugMPZ\divis.obj"
	-@erase ".\DebugMPZ\divis_2exp.obj"
	-@erase ".\DebugMPZ\divis_ui.obj"
	-@erase ".\DebugMPZ\dump.obj"
	-@erase ".\DebugMPZ\export.obj"
	-@erase ".\DebugMPZ\fac_ui.obj"
	-@erase ".\DebugMPZ\fdiv_q.obj"
	-@erase ".\DebugMPZ\fdiv_q_ui.obj"
	-@erase ".\DebugMPZ\fdiv_qr.obj"
	-@erase ".\DebugMPZ\fdiv_qr_ui.obj"
	-@erase ".\DebugMPZ\fdiv_r.obj"
	-@erase ".\DebugMPZ\fdiv_r_ui.obj"
	-@erase ".\DebugMPZ\fdiv_ui.obj"
	-@erase ".\DebugMPZ\fib2_ui.obj"
	-@erase ".\DebugMPZ\fib_ui.obj"
	-@erase ".\DebugMPZ\fits_sint.obj"
	-@erase ".\DebugMPZ\fits_slong.obj"
	-@erase ".\DebugMPZ\fits_sshort.obj"
	-@erase ".\DebugMPZ\fits_uint.obj"
	-@erase ".\DebugMPZ\fits_ulong.obj"
	-@erase ".\DebugMPZ\fits_ushort.obj"
	-@erase ".\DebugMPZ\gcd.obj"
	-@erase ".\DebugMPZ\gcd_ui.obj"
	-@erase ".\DebugMPZ\gcdext.obj"
	-@erase ".\DebugMPZ\get_d.obj"
	-@erase ".\DebugMPZ\get_d_2exp.obj"
	-@erase ".\DebugMPZ\get_si.obj"
	-@erase ".\DebugMPZ\get_str.obj"
	-@erase ".\DebugMPZ\get_ui.obj"
	-@erase ".\DebugMPZ\getlimbn.obj"
	-@erase ".\DebugMPZ\hamdist.obj"
	-@erase ".\DebugMPZ\import.obj"
	-@erase ".\DebugMPZ\init.obj"
	-@erase ".\DebugMPZ\init2.obj"
	-@erase ".\DebugMPZ\inp_raw.obj"
	-@erase ".\DebugMPZ\inp_str.obj"
	-@erase ".\DebugMPZ\invert.obj"
	-@erase ".\DebugMPZ\ior.obj"
	-@erase ".\DebugMPZ\iset.obj"
	-@erase ".\DebugMPZ\iset_d.obj"
	-@erase ".\DebugMPZ\iset_si.obj"
	-@erase ".\DebugMPZ\iset_str.obj"
	-@erase ".\DebugMPZ\iset_ui.obj"
	-@erase ".\DebugMPZ\jacobi.obj"
	-@erase ".\DebugMPZ\kronsz.obj"
	-@erase ".\DebugMPZ\kronuz.obj"
	-@erase ".\DebugMPZ\kronzs.obj"
	-@erase ".\DebugMPZ\kronzu.obj"
	-@erase ".\DebugMPZ\lcm.obj"
	-@erase ".\DebugMPZ\lcm_ui.obj"
	-@erase ".\DebugMPZ\lucnum2_ui.obj"
	-@erase ".\DebugMPZ\lucnum_ui.obj"
	-@erase ".\DebugMPZ\millerrabin.obj"
	-@erase ".\DebugMPZ\mod.obj"
	-@erase ".\DebugMPZ\mul.obj"
	-@erase ".\DebugMPZ\mul_2exp.obj"
	-@erase ".\DebugMPZ\mul_si.obj"
	-@erase ".\DebugMPZ\mul_ui.obj"
	-@erase ".\DebugMPZ\n_pow_ui.obj"
	-@erase ".\DebugMPZ\neg.obj"
	-@erase ".\DebugMPZ\nextprime.obj"
	-@erase ".\DebugMPZ\out_raw.obj"
	-@erase ".\DebugMPZ\out_str.obj"
	-@erase ".\DebugMPZ\perfpow.obj"
	-@erase ".\DebugMPZ\perfsqr.obj"
	-@erase ".\DebugMPZ\popcount.obj"
	-@erase ".\DebugMPZ\pow_ui.obj"
	-@erase ".\DebugMPZ\powm.obj"
	-@erase ".\DebugMPZ\powm_ui.obj"
	-@erase ".\DebugMPZ\pprime_p.obj"
	-@erase ".\DebugMPZ\random.obj"
	-@erase ".\DebugMPZ\random2.obj"
	-@erase ".\DebugMPZ\realloc.obj"
	-@erase ".\DebugMPZ\realloc2.obj"
	-@erase ".\DebugMPZ\remove.obj"
	-@erase ".\DebugMPZ\root.obj"
	-@erase ".\DebugMPZ\rrandomb.obj"
	-@erase ".\DebugMPZ\scan0.obj"
	-@erase ".\DebugMPZ\scan1.obj"
	-@erase ".\DebugMPZ\set.obj"
	-@erase ".\DebugMPZ\set_d.obj"
	-@erase ".\DebugMPZ\set_f.obj"
	-@erase ".\DebugMPZ\set_q.obj"
	-@erase ".\DebugMPZ\set_si.obj"
	-@erase ".\DebugMPZ\set_str.obj"
	-@erase ".\DebugMPZ\set_ui.obj"
	-@erase ".\DebugMPZ\setbit.obj"
	-@erase ".\DebugMPZ\size.obj"
	-@erase ".\DebugMPZ\sizeinbase.obj"
	-@erase ".\DebugMPZ\sqrt.obj"
	-@erase ".\DebugMPZ\sqrtrem.obj"
	-@erase ".\DebugMPZ\sub.obj"
	-@erase ".\DebugMPZ\sub_ui.obj"
	-@erase ".\DebugMPZ\swap.obj"
	-@erase ".\DebugMPZ\tdiv_q.obj"
	-@erase ".\DebugMPZ\tdiv_q_2exp.obj"
	-@erase ".\DebugMPZ\tdiv_q_ui.obj"
	-@erase ".\DebugMPZ\tdiv_qr.obj"
	-@erase ".\DebugMPZ\tdiv_qr_ui.obj"
	-@erase ".\DebugMPZ\tdiv_r.obj"
	-@erase ".\DebugMPZ\tdiv_r_2exp.obj"
	-@erase ".\DebugMPZ\tdiv_r_ui.obj"
	-@erase ".\DebugMPZ\tdiv_ui.obj"
	-@erase ".\DebugMPZ\tstbit.obj"
	-@erase ".\DebugMPZ\ui_pow_ui.obj"
	-@erase ".\DebugMPZ\ui_sub.obj"
	-@erase ".\DebugMPZ\urandomb.obj"
	-@erase ".\DebugMPZ\urandomm.obj"
	-@erase ".\DebugMPZ\vc50.idb"
	-@erase ".\DebugMPZ\vc50.pdb"
	-@erase ".\DebugMPZ\xor.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse" /I\
 "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER" /D\
 "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"$(INTDIR)\GmpDll.pch" /YX\
 /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
CPP_OBJS=.\Debug/
CPP_SBRS=.
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /o NUL /win32 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\GmpDll.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)\gmp33.pdb" /debug /machine:I386 /out:"$(OUTDIR)\gmp33.dll"\
 /implib:"$(OUTDIR)\gmp33.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\ansi2knr.obj" \
	"$(INTDIR)\assert.obj" \
	"$(INTDIR)\compat.obj" \
	"$(INTDIR)\errno.obj" \
	"$(INTDIR)\extract-dbl.obj" \
	"$(INTDIR)\insert-dbl.obj" \
	"$(INTDIR)\memory.obj" \
	"$(INTDIR)\mp_bpl.obj" \
	"$(INTDIR)\mp_clz_tab.obj" \
	"$(INTDIR)\mp_minv_tab.obj" \
	"$(INTDIR)\mp_set_fns.obj" \
	"$(INTDIR)\rand.obj" \
	"$(INTDIR)\randclr.obj" \
	"$(INTDIR)\randdef.obj" \
	"$(INTDIR)\randlc.obj" \
	"$(INTDIR)\randlc2s.obj" \
	"$(INTDIR)\randlc2x.obj" \
	"$(INTDIR)\randraw.obj" \
	"$(INTDIR)\rands.obj" \
	"$(INTDIR)\randsd.obj" \
	"$(INTDIR)\randsdui.obj" \
	"$(INTDIR)\version.obj" \
	".\DebugMPN\add.obj" \
	".\DebugMPN\add_1.obj" \
	".\DebugMPN\add_n.obj" \
	".\DebugMPN\addmul_1.obj" \
	".\DebugMPN\addsub_n.obj" \
	".\DebugMPN\bdivmod.obj" \
	".\DebugMPN\cmp.obj" \
	".\DebugMPN\dc_divrem_n.obj" \
	".\DebugMPN\dive_1.obj" \
	".\DebugMPN\diveby3.obj" \
	".\DebugMPN\divis.obj" \
	".\DebugMPN\divrem.obj" \
	".\DebugMPN\divrem_1.obj" \
	".\DebugMPN\divrem_2.obj" \
	".\DebugMPN\dump.obj" \
	".\DebugMPN\fib2_ui.obj" \
	".\DebugMPN\gcd.obj" \
	".\DebugMPN\gcd_1.obj" \
	".\DebugMPN\gcdext.obj" \
	".\DebugMPN\get_str.obj" \
	".\DebugMPN\hamdist.obj" \
	".\DebugMPN\jacbase.obj" \
	".\DebugMPN\lshift.obj" \
	".\DebugMPN\mod_1.obj" \
	".\DebugMPN\mod_34lsub1.obj" \
	".\DebugMPN\mode1o.obj" \
	".\DebugMPN\mp_bases.obj" \
	".\DebugMPN\mul.obj" \
	".\DebugMPN\mul_1.obj" \
	".\DebugMPN\mul_basecase.obj" \
	".\DebugMPN\mul_fft.obj" \
	".\DebugMPN\mul_n.obj" \
	".\DebugMPN\perfsqr.obj" \
	".\DebugMPN\popcount.obj" \
	".\DebugMPN\pow_1.obj" \
	".\DebugMPN\pre_divrem_1.obj" \
	".\DebugMPN\pre_mod_1.obj" \
	".\DebugMPN\random.obj" \
	".\DebugMPN\random2.obj" \
	".\DebugMPN\rootrem.obj" \
	".\DebugMPN\rshift.obj" \
	".\DebugMPN\sb_divrem_mn.obj" \
	".\DebugMPN\scan0.obj" \
	".\DebugMPN\scan1.obj" \
	".\DebugMPN\set_str.obj" \
	".\DebugMPN\sizeinbase.obj" \
	".\DebugMPN\sqr_basecase.obj" \
	".\DebugMPN\sqrtrem.obj" \
	".\DebugMPN\sub.obj" \
	".\DebugMPN\sub_1.obj" \
	".\DebugMPN\sub_n.obj" \
	".\DebugMPN\submul_1.obj" \
	".\DebugMPN\tdiv_qr.obj" \
	".\DebugMPQ\abs.obj" \
	".\DebugMPQ\aors.obj" \
	".\DebugMPQ\canonicalize.obj" \
	".\DebugMPQ\clear.obj" \
	".\DebugMPQ\cmp.obj" \
	".\DebugMPQ\cmp_si.obj" \
	".\DebugMPQ\cmp_ui.obj" \
	".\DebugMPQ\div.obj" \
	".\DebugMPQ\equal.obj" \
	".\DebugMPQ\get_d.obj" \
	".\DebugMPQ\get_den.obj" \
	".\DebugMPQ\get_num.obj" \
	".\DebugMPQ\get_str.obj" \
	".\DebugMPQ\init.obj" \
	".\DebugMPQ\inp_str.obj" \
	".\DebugMPQ\inv.obj" \
	".\DebugMPQ\md_2exp.obj" \
	".\DebugMPQ\mul.obj" \
	".\DebugMPQ\neg.obj" \
	".\DebugMPQ\out_str.obj" \
	".\DebugMPQ\set.obj" \
	".\DebugMPQ\set_d.obj" \
	".\DebugMPQ\set_den.obj" \
	".\DebugMPQ\set_f.obj" \
	".\DebugMPQ\set_num.obj" \
	".\DebugMPQ\set_si.obj" \
	".\DebugMPQ\set_str.obj" \
	".\DebugMPQ\set_ui.obj" \
	".\DebugMPQ\set_z.obj" \
	".\DebugMPQ\swap.obj" \
	".\DebugMPZ\abs.obj" \
	".\DebugMPZ\add.obj" \
	".\DebugMPZ\add_ui.obj" \
	".\DebugMPZ\and.obj" \
	".\DebugMPZ\aorsmul.obj" \
	".\DebugMPZ\aorsmul_i.obj" \
	".\DebugMPZ\array_init.obj" \
	".\DebugMPZ\bin_ui.obj" \
	".\DebugMPZ\bin_uiui.obj" \
	".\DebugMPZ\cdiv_q.obj" \
	".\DebugMPZ\cdiv_q_ui.obj" \
	".\DebugMPZ\cdiv_qr.obj" \
	".\DebugMPZ\cdiv_qr_ui.obj" \
	".\DebugMPZ\cdiv_r.obj" \
	".\DebugMPZ\cdiv_r_ui.obj" \
	".\DebugMPZ\cdiv_ui.obj" \
	".\DebugMPZ\cfdiv_q_2exp.obj" \
	".\DebugMPZ\cfdiv_r_2exp.obj" \
	".\DebugMPZ\clear.obj" \
	".\DebugMPZ\clrbit.obj" \
	".\DebugMPZ\cmp.obj" \
	".\DebugMPZ\cmp_d.obj" \
	".\DebugMPZ\cmp_si.obj" \
	".\DebugMPZ\cmp_ui.obj" \
	".\DebugMPZ\cmpabs.obj" \
	".\DebugMPZ\cmpabs_d.obj" \
	".\DebugMPZ\cmpabs_ui.obj" \
	".\DebugMPZ\com.obj" \
	".\DebugMPZ\cong.obj" \
	".\DebugMPZ\cong_2exp.obj" \
	".\DebugMPZ\cong_ui.obj" \
	".\DebugMPZ\dive_ui.obj" \
	".\DebugMPZ\divegcd.obj" \
	".\DebugMPZ\divexact.obj" \
	".\DebugMPZ\divis.obj" \
	".\DebugMPZ\divis_2exp.obj" \
	".\DebugMPZ\divis_ui.obj" \
	".\DebugMPZ\dump.obj" \
	".\DebugMPZ\export.obj" \
	".\DebugMPZ\fac_ui.obj" \
	".\DebugMPZ\fdiv_q.obj" \
	".\DebugMPZ\fdiv_q_ui.obj" \
	".\DebugMPZ\fdiv_qr.obj" \
	".\DebugMPZ\fdiv_qr_ui.obj" \
	".\DebugMPZ\fdiv_r.obj" \
	".\DebugMPZ\fdiv_r_ui.obj" \
	".\DebugMPZ\fdiv_ui.obj" \
	".\DebugMPZ\fib2_ui.obj" \
	".\DebugMPZ\fib_ui.obj" \
	".\DebugMPZ\fits_sint.obj" \
	".\DebugMPZ\fits_slong.obj" \
	".\DebugMPZ\fits_sshort.obj" \
	".\DebugMPZ\fits_uint.obj" \
	".\DebugMPZ\fits_ulong.obj" \
	".\DebugMPZ\fits_ushort.obj" \
	".\DebugMPZ\gcd.obj" \
	".\DebugMPZ\gcd_ui.obj" \
	".\DebugMPZ\gcdext.obj" \
	".\DebugMPZ\get_d.obj" \
	".\DebugMPZ\get_d_2exp.obj" \
	".\DebugMPZ\get_si.obj" \
	".\DebugMPZ\get_str.obj" \
	".\DebugMPZ\get_ui.obj" \
	".\DebugMPZ\getlimbn.obj" \
	".\DebugMPZ\hamdist.obj" \
	".\DebugMPZ\import.obj" \
	".\DebugMPZ\init.obj" \
	".\DebugMPZ\init2.obj" \
	".\DebugMPZ\inp_raw.obj" \
	".\DebugMPZ\inp_str.obj" \
	".\DebugMPZ\invert.obj" \
	".\DebugMPZ\ior.obj" \
	".\DebugMPZ\iset.obj" \
	".\DebugMPZ\iset_d.obj" \
	".\DebugMPZ\iset_si.obj" \
	".\DebugMPZ\iset_str.obj" \
	".\DebugMPZ\iset_ui.obj" \
	".\DebugMPZ\jacobi.obj" \
	".\DebugMPZ\kronsz.obj" \
	".\DebugMPZ\kronuz.obj" \
	".\DebugMPZ\kronzs.obj" \
	".\DebugMPZ\kronzu.obj" \
	".\DebugMPZ\lcm.obj" \
	".\DebugMPZ\lcm_ui.obj" \
	".\DebugMPZ\lucnum2_ui.obj" \
	".\DebugMPZ\lucnum_ui.obj" \
	".\DebugMPZ\millerrabin.obj" \
	".\DebugMPZ\mod.obj" \
	".\DebugMPZ\mul.obj" \
	".\DebugMPZ\mul_2exp.obj" \
	".\DebugMPZ\mul_si.obj" \
	".\DebugMPZ\mul_ui.obj" \
	".\DebugMPZ\n_pow_ui.obj" \
	".\DebugMPZ\neg.obj" \
	".\DebugMPZ\nextprime.obj" \
	".\DebugMPZ\out_raw.obj" \
	".\DebugMPZ\out_str.obj" \
	".\DebugMPZ\perfpow.obj" \
	".\DebugMPZ\perfsqr.obj" \
	".\DebugMPZ\popcount.obj" \
	".\DebugMPZ\pow_ui.obj" \
	".\DebugMPZ\powm.obj" \
	".\DebugMPZ\powm_ui.obj" \
	".\DebugMPZ\pprime_p.obj" \
	".\DebugMPZ\random.obj" \
	".\DebugMPZ\random2.obj" \
	".\DebugMPZ\realloc.obj" \
	".\DebugMPZ\realloc2.obj" \
	".\DebugMPZ\remove.obj" \
	".\DebugMPZ\root.obj" \
	".\DebugMPZ\rrandomb.obj" \
	".\DebugMPZ\scan0.obj" \
	".\DebugMPZ\scan1.obj" \
	".\DebugMPZ\set.obj" \
	".\DebugMPZ\set_d.obj" \
	".\DebugMPZ\set_f.obj" \
	".\DebugMPZ\set_q.obj" \
	".\DebugMPZ\set_si.obj" \
	".\DebugMPZ\set_str.obj" \
	".\DebugMPZ\set_ui.obj" \
	".\DebugMPZ\setbit.obj" \
	".\DebugMPZ\size.obj" \
	".\DebugMPZ\sizeinbase.obj" \
	".\DebugMPZ\sqrt.obj" \
	".\DebugMPZ\sqrtrem.obj" \
	".\DebugMPZ\sub.obj" \
	".\DebugMPZ\sub_ui.obj" \
	".\DebugMPZ\swap.obj" \
	".\DebugMPZ\tdiv_q.obj" \
	".\DebugMPZ\tdiv_q_2exp.obj" \
	".\DebugMPZ\tdiv_q_ui.obj" \
	".\DebugMPZ\tdiv_qr.obj" \
	".\DebugMPZ\tdiv_qr_ui.obj" \
	".\DebugMPZ\tdiv_r.obj" \
	".\DebugMPZ\tdiv_r_2exp.obj" \
	".\DebugMPZ\tdiv_r_ui.obj" \
	".\DebugMPZ\tdiv_ui.obj" \
	".\DebugMPZ\tstbit.obj" \
	".\DebugMPZ\ui_pow_ui.obj" \
	".\DebugMPZ\ui_sub.obj" \
	".\DebugMPZ\urandomb.obj" \
	".\DebugMPZ\urandomm.obj" \
	".\DebugMPZ\xor.obj"

"$(OUTDIR)\gmp33.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE=$(InputPath)
PostBuild_Desc=Copying gmp dll to lib/i386_nt
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

$(DS_POSTBUILD_DEP) : "$(OUTDIR)\gmp33.dll"
   copy Debug\gmp33.dll ..\..\lib\i386_nt\gmp33.dll
	copy   Debug\gmp33.lib ..\..\lib\i386_nt\gmp33.lib
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ENDIF 

.c{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_OBJS)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(CPP_SBRS)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF "$(CFG)" == "GmpDll - Win32 Release" || "$(CFG)" == "GmpDll - Win32 Debug"
SOURCE=..\..\Gmp\src\mpn\generic\add.c
DEP_CPP_ADD_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\add.obj" : $(SOURCE) $(DEP_CPP_ADD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\add.obj" : $(SOURCE) $(DEP_CPP_ADD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\add_1.c
DEP_CPP_ADD_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\add_1.obj" : $(SOURCE) $(DEP_CPP_ADD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\add_1.obj" : $(SOURCE) $(DEP_CPP_ADD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\add_n.c
DEP_CPP_ADD_N=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\add_n.obj" : $(SOURCE) $(DEP_CPP_ADD_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\add_n.obj" : $(SOURCE) $(DEP_CPP_ADD_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\addmul_1.c
DEP_CPP_ADDMU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\addmul_1.obj" : $(SOURCE) $(DEP_CPP_ADDMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\addmul_1.obj" : $(SOURCE) $(DEP_CPP_ADDMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\addsub_n.c
DEP_CPP_ADDSU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_ADDSU=\
	"..\..\Gmp\src\mpn\generic\timing.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\addsub_n.obj" : $(SOURCE) $(DEP_CPP_ADDSU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\addsub_n.obj" : $(SOURCE) $(DEP_CPP_ADDSU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\bdivmod.c
DEP_CPP_BDIVM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\bdivmod.obj" : $(SOURCE) $(DEP_CPP_BDIVM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\bdivmod.obj" : $(SOURCE) $(DEP_CPP_BDIVM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\cmp.c
DEP_CPP_CMP_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\dc_divrem_n.c
DEP_CPP_DC_DI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\dc_divrem_n.obj" : $(SOURCE) $(DEP_CPP_DC_DI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\dc_divrem_n.obj" : $(SOURCE) $(DEP_CPP_DC_DI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\dive_1.c
DEP_CPP_DIVE_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\dive_1.obj" : $(SOURCE) $(DEP_CPP_DIVE_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\dive_1.obj" : $(SOURCE) $(DEP_CPP_DIVE_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\diveby3.c
DEP_CPP_DIVEB=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\diveby3.obj" : $(SOURCE) $(DEP_CPP_DIVEB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\diveby3.obj" : $(SOURCE) $(DEP_CPP_DIVEB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\divis.c
DEP_CPP_DIVIS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\divis.obj" : $(SOURCE) $(DEP_CPP_DIVIS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\divis.obj" : $(SOURCE) $(DEP_CPP_DIVIS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\divrem.c
DEP_CPP_DIVRE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\divrem.obj" : $(SOURCE) $(DEP_CPP_DIVRE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\divrem.obj" : $(SOURCE) $(DEP_CPP_DIVRE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\divrem_1.c
DEP_CPP_DIVREM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\divrem_1.obj" : $(SOURCE) $(DEP_CPP_DIVREM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\divrem_1.obj" : $(SOURCE) $(DEP_CPP_DIVREM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\divrem_2.c
DEP_CPP_DIVREM_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\divrem_2.obj" : $(SOURCE) $(DEP_CPP_DIVREM_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\divrem_2.obj" : $(SOURCE) $(DEP_CPP_DIVREM_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\dump.c
DEP_CPP_DUMP_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\fib2_ui.c
DEP_CPP_FIB2_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\fib2_ui.obj" : $(SOURCE) $(DEP_CPP_FIB2_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\fib2_ui.obj" : $(SOURCE) $(DEP_CPP_FIB2_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\gcd.c
DEP_CPP_GCD_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\gcd.obj" : $(SOURCE) $(DEP_CPP_GCD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\gcd.obj" : $(SOURCE) $(DEP_CPP_GCD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\gcd_1.c
DEP_CPP_GCD_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\gcd_1.obj" : $(SOURCE) $(DEP_CPP_GCD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\gcd_1.obj" : $(SOURCE) $(DEP_CPP_GCD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\gcdext.c
DEP_CPP_GCDEX=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\gcdext.obj" : $(SOURCE) $(DEP_CPP_GCDEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\gcdext.obj" : $(SOURCE) $(DEP_CPP_GCDEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\get_str.c
DEP_CPP_GET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\hamdist.c
DEP_CPP_HAMDI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\hamdist.obj" : $(SOURCE) $(DEP_CPP_HAMDI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\hamdist.obj" : $(SOURCE) $(DEP_CPP_HAMDI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\jacbase.c
DEP_CPP_JACBA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\jacbase.obj" : $(SOURCE) $(DEP_CPP_JACBA) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\jacbase.obj" : $(SOURCE) $(DEP_CPP_JACBA) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\lshift.c
DEP_CPP_LSHIF=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\lshift.obj" : $(SOURCE) $(DEP_CPP_LSHIF) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\lshift.obj" : $(SOURCE) $(DEP_CPP_LSHIF) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mod_1.c
DEP_CPP_MOD_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mod_1.obj" : $(SOURCE) $(DEP_CPP_MOD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mod_1.obj" : $(SOURCE) $(DEP_CPP_MOD_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mod_34lsub1.c
DEP_CPP_MOD_3=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mod_34lsub1.obj" : $(SOURCE) $(DEP_CPP_MOD_3) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mod_34lsub1.obj" : $(SOURCE) $(DEP_CPP_MOD_3) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mode1o.c
DEP_CPP_MODE1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mode1o.obj" : $(SOURCE) $(DEP_CPP_MODE1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mode1o.obj" : $(SOURCE) $(DEP_CPP_MODE1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\mp_bases.c
DEP_CPP_MP_BA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mp_bases.obj" : $(SOURCE) $(DEP_CPP_MP_BA) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mp_bases.obj" : $(SOURCE) $(DEP_CPP_MP_BA) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mul.c
DEP_CPP_MUL_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mul_1.c
DEP_CPP_MUL_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mul_1.obj" : $(SOURCE) $(DEP_CPP_MUL_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mul_1.obj" : $(SOURCE) $(DEP_CPP_MUL_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mul_basecase.c
DEP_CPP_MUL_B=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mul_basecase.obj" : $(SOURCE) $(DEP_CPP_MUL_B) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mul_basecase.obj" : $(SOURCE) $(DEP_CPP_MUL_B) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mul_fft.c
DEP_CPP_MUL_F=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mul_fft.obj" : $(SOURCE) $(DEP_CPP_MUL_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mul_fft.obj" : $(SOURCE) $(DEP_CPP_MUL_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\mul_n.c
DEP_CPP_MUL_N=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\mul_n.obj" : $(SOURCE) $(DEP_CPP_MUL_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\mul_n.obj" : $(SOURCE) $(DEP_CPP_MUL_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\perfsqr.c
DEP_CPP_PERFS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\perfsqr.obj" : $(SOURCE) $(DEP_CPP_PERFS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\perfsqr.obj" : $(SOURCE) $(DEP_CPP_PERFS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\popcount.c
DEP_CPP_POPCO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\popcount.obj" : $(SOURCE) $(DEP_CPP_POPCO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\popcount.obj" : $(SOURCE) $(DEP_CPP_POPCO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\pow_1.c
DEP_CPP_POW_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\pow_1.obj" : $(SOURCE) $(DEP_CPP_POW_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\pow_1.obj" : $(SOURCE) $(DEP_CPP_POW_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\pre_divrem_1.c
DEP_CPP_PRE_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\pre_divrem_1.obj" : $(SOURCE) $(DEP_CPP_PRE_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\pre_divrem_1.obj" : $(SOURCE) $(DEP_CPP_PRE_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\pre_mod_1.c
DEP_CPP_PRE_M=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\pre_mod_1.obj" : $(SOURCE) $(DEP_CPP_PRE_M) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\pre_mod_1.obj" : $(SOURCE) $(DEP_CPP_PRE_M) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\random.c
DEP_CPP_RANDO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\random.obj" : $(SOURCE) $(DEP_CPP_RANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\random.obj" : $(SOURCE) $(DEP_CPP_RANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\random2.c
DEP_CPP_RANDOM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\random2.obj" : $(SOURCE) $(DEP_CPP_RANDOM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\random2.obj" : $(SOURCE) $(DEP_CPP_RANDOM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\rootrem.c
DEP_CPP_ROOTR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\rootrem.obj" : $(SOURCE) $(DEP_CPP_ROOTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\rootrem.obj" : $(SOURCE) $(DEP_CPP_ROOTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\rshift.c
DEP_CPP_RSHIF=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\rshift.obj" : $(SOURCE) $(DEP_CPP_RSHIF) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\rshift.obj" : $(SOURCE) $(DEP_CPP_RSHIF) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sb_divrem_mn.c
DEP_CPP_SB_DI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sb_divrem_mn.obj" : $(SOURCE) $(DEP_CPP_SB_DI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sb_divrem_mn.obj" : $(SOURCE) $(DEP_CPP_SB_DI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\scan0.c
DEP_CPP_SCAN0=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\scan0.obj" : $(SOURCE) $(DEP_CPP_SCAN0) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\scan0.obj" : $(SOURCE) $(DEP_CPP_SCAN0) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\scan1.c
DEP_CPP_SCAN1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\scan1.obj" : $(SOURCE) $(DEP_CPP_SCAN1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\scan1.obj" : $(SOURCE) $(DEP_CPP_SCAN1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\set_str.c
DEP_CPP_SET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sizeinbase.c
DEP_CPP_SIZEI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sizeinbase.obj" : $(SOURCE) $(DEP_CPP_SIZEI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sizeinbase.obj" : $(SOURCE) $(DEP_CPP_SIZEI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sqr_basecase.c
DEP_CPP_SQR_B=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sqr_basecase.obj" : $(SOURCE) $(DEP_CPP_SQR_B) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sqr_basecase.obj" : $(SOURCE) $(DEP_CPP_SQR_B) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sqrtrem.c
DEP_CPP_SQRTR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sqrtrem.obj" : $(SOURCE) $(DEP_CPP_SQRTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sqrtrem.obj" : $(SOURCE) $(DEP_CPP_SQRTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sub.c
DEP_CPP_SUB_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sub.obj" : $(SOURCE) $(DEP_CPP_SUB_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sub.obj" : $(SOURCE) $(DEP_CPP_SUB_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sub_1.c
DEP_CPP_SUB_1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sub_1.obj" : $(SOURCE) $(DEP_CPP_SUB_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sub_1.obj" : $(SOURCE) $(DEP_CPP_SUB_1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\sub_n.c
DEP_CPP_SUB_N=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\sub_n.obj" : $(SOURCE) $(DEP_CPP_SUB_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\sub_n.obj" : $(SOURCE) $(DEP_CPP_SUB_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\submul_1.c
DEP_CPP_SUBMU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\submul_1.obj" : $(SOURCE) $(DEP_CPP_SUBMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\submul_1.obj" : $(SOURCE) $(DEP_CPP_SUBMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpn\generic\tdiv_qr.c
DEP_CPP_TDIV_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\Gmp\src\mpn\generic\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPN
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPN\tdiv_qr.obj" : $(SOURCE) $(DEP_CPP_TDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPN\tdiv_qr.obj" : $(SOURCE) $(DEP_CPP_TDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\abs.c
DEP_CPP_ABS_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\abs.obj" : $(SOURCE) $(DEP_CPP_ABS_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\abs.obj" : $(SOURCE) $(DEP_CPP_ABS_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\aors.c
DEP_CPP_AORS_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\aors.obj" : $(SOURCE) $(DEP_CPP_AORS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\aors.obj" : $(SOURCE) $(DEP_CPP_AORS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\canonicalize.c
DEP_CPP_CANON=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\canonicalize.obj" : $(SOURCE) $(DEP_CPP_CANON) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\canonicalize.obj" : $(SOURCE) $(DEP_CPP_CANON) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\clear.c
DEP_CPP_CLEAR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\clear.obj" : $(SOURCE) $(DEP_CPP_CLEAR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\clear.obj" : $(SOURCE) $(DEP_CPP_CLEAR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\cmp.c
DEP_CPP_CMP_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\cmp_si.c
DEP_CPP_CMP_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\cmp_si.obj" : $(SOURCE) $(DEP_CPP_CMP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\cmp_si.obj" : $(SOURCE) $(DEP_CPP_CMP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\cmp_ui.c
DEP_CPP_CMP_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\cmp_ui.obj" : $(SOURCE) $(DEP_CPP_CMP_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\cmp_ui.obj" : $(SOURCE) $(DEP_CPP_CMP_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\div.c
DEP_CPP_DIV_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\div.obj" : $(SOURCE) $(DEP_CPP_DIV_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\div.obj" : $(SOURCE) $(DEP_CPP_DIV_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\equal.c
DEP_CPP_EQUAL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\equal.obj" : $(SOURCE) $(DEP_CPP_EQUAL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\equal.obj" : $(SOURCE) $(DEP_CPP_EQUAL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\get_d.c
DEP_CPP_GET_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\get_d.obj" : $(SOURCE) $(DEP_CPP_GET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\get_d.obj" : $(SOURCE) $(DEP_CPP_GET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\get_den.c
DEP_CPP_GET_DE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\get_den.obj" : $(SOURCE) $(DEP_CPP_GET_DE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\get_den.obj" : $(SOURCE) $(DEP_CPP_GET_DE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\get_num.c
DEP_CPP_GET_N=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\get_num.obj" : $(SOURCE) $(DEP_CPP_GET_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\get_num.obj" : $(SOURCE) $(DEP_CPP_GET_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\get_str.c
DEP_CPP_GET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\init.c
DEP_CPP_INIT_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\inp_str.c
DEP_CPP_INP_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\inp_str.obj" : $(SOURCE) $(DEP_CPP_INP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\inp_str.obj" : $(SOURCE) $(DEP_CPP_INP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\inv.c
DEP_CPP_INV_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\inv.obj" : $(SOURCE) $(DEP_CPP_INV_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\inv.obj" : $(SOURCE) $(DEP_CPP_INV_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\md_2exp.c
DEP_CPP_MD_2E=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\md_2exp.obj" : $(SOURCE) $(DEP_CPP_MD_2E) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\md_2exp.obj" : $(SOURCE) $(DEP_CPP_MD_2E) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\mul.c
DEP_CPP_MUL_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\neg.c
DEP_CPP_NEG_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\neg.obj" : $(SOURCE) $(DEP_CPP_NEG_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\neg.obj" : $(SOURCE) $(DEP_CPP_NEG_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\out_str.c
DEP_CPP_OUT_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\out_str.obj" : $(SOURCE) $(DEP_CPP_OUT_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\out_str.obj" : $(SOURCE) $(DEP_CPP_OUT_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set.c
DEP_CPP_SET_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set.obj" : $(SOURCE) $(DEP_CPP_SET_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set.obj" : $(SOURCE) $(DEP_CPP_SET_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_d.c
DEP_CPP_SET_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_d.obj" : $(SOURCE) $(DEP_CPP_SET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_d.obj" : $(SOURCE) $(DEP_CPP_SET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_den.c
DEP_CPP_SET_DE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_den.obj" : $(SOURCE) $(DEP_CPP_SET_DE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_den.obj" : $(SOURCE) $(DEP_CPP_SET_DE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_f.c
DEP_CPP_SET_F=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_f.obj" : $(SOURCE) $(DEP_CPP_SET_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_f.obj" : $(SOURCE) $(DEP_CPP_SET_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_num.c
DEP_CPP_SET_N=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_num.obj" : $(SOURCE) $(DEP_CPP_SET_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_num.obj" : $(SOURCE) $(DEP_CPP_SET_N) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_si.c
DEP_CPP_SET_SI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_si.obj" : $(SOURCE) $(DEP_CPP_SET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_si.obj" : $(SOURCE) $(DEP_CPP_SET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_str.c
DEP_CPP_SET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_ui.c
DEP_CPP_SET_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_ui.obj" : $(SOURCE) $(DEP_CPP_SET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_ui.obj" : $(SOURCE) $(DEP_CPP_SET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\set_z.c
DEP_CPP_SET_Z=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\set_z.obj" : $(SOURCE) $(DEP_CPP_SET_Z) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\set_z.obj" : $(SOURCE) $(DEP_CPP_SET_Z) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpq\swap.c
DEP_CPP_SWAP_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPQ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPQ\swap.obj" : $(SOURCE) $(DEP_CPP_SWAP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPQ\swap.obj" : $(SOURCE) $(DEP_CPP_SWAP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\abs.c
DEP_CPP_ABS_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\abs.obj" : $(SOURCE) $(DEP_CPP_ABS_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\abs.obj" : $(SOURCE) $(DEP_CPP_ABS_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\add.c
DEP_CPP_ADD_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\aors.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_ADD_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\add.obj" : $(SOURCE) $(DEP_CPP_ADD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\add.obj" : $(SOURCE) $(DEP_CPP_ADD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\add_ui.c
DEP_CPP_ADD_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\aors_ui.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\add_ui.obj" : $(SOURCE) $(DEP_CPP_ADD_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\add_ui.obj" : $(SOURCE) $(DEP_CPP_ADD_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\and.c
DEP_CPP_AND_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\and.obj" : $(SOURCE) $(DEP_CPP_AND_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\and.obj" : $(SOURCE) $(DEP_CPP_AND_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\aorsmul.c
DEP_CPP_AORSM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\aorsmul.obj" : $(SOURCE) $(DEP_CPP_AORSM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\aorsmul.obj" : $(SOURCE) $(DEP_CPP_AORSM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\aorsmul_i.c
DEP_CPP_AORSMU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\aorsmul_i.obj" : $(SOURCE) $(DEP_CPP_AORSMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\aorsmul_i.obj" : $(SOURCE) $(DEP_CPP_AORSMU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\array_init.c
DEP_CPP_ARRAY=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\array_init.obj" : $(SOURCE) $(DEP_CPP_ARRAY) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\array_init.obj" : $(SOURCE) $(DEP_CPP_ARRAY) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\bin_ui.c
DEP_CPP_BIN_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\bin_ui.obj" : $(SOURCE) $(DEP_CPP_BIN_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\bin_ui.obj" : $(SOURCE) $(DEP_CPP_BIN_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\bin_uiui.c
DEP_CPP_BIN_UI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\bin_uiui.obj" : $(SOURCE) $(DEP_CPP_BIN_UI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\bin_uiui.obj" : $(SOURCE) $(DEP_CPP_BIN_UI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_q.c
DEP_CPP_CDIV_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_q.obj" : $(SOURCE) $(DEP_CPP_CDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_q.obj" : $(SOURCE) $(DEP_CPP_CDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_q_ui.c
DEP_CPP_CDIV_Q=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_qr.c
DEP_CPP_CDIV_QR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_qr.obj" : $(SOURCE) $(DEP_CPP_CDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_qr.obj" : $(SOURCE) $(DEP_CPP_CDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_qr_ui.c
DEP_CPP_CDIV_QR_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_QR_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_QR_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_r.c
DEP_CPP_CDIV_R=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_r.obj" : $(SOURCE) $(DEP_CPP_CDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_r.obj" : $(SOURCE) $(DEP_CPP_CDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_r_ui.c
DEP_CPP_CDIV_R_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cdiv_ui.c
DEP_CPP_CDIV_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cdiv_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cdiv_ui.obj" : $(SOURCE) $(DEP_CPP_CDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cfdiv_q_2exp.c
DEP_CPP_CFDIV=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cfdiv_q_2exp.obj" : $(SOURCE) $(DEP_CPP_CFDIV) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cfdiv_q_2exp.obj" : $(SOURCE) $(DEP_CPP_CFDIV) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cfdiv_r_2exp.c
DEP_CPP_CFDIV_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cfdiv_r_2exp.obj" : $(SOURCE) $(DEP_CPP_CFDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cfdiv_r_2exp.obj" : $(SOURCE) $(DEP_CPP_CFDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\clear.c
DEP_CPP_CLEAR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\clear.obj" : $(SOURCE) $(DEP_CPP_CLEAR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\clear.obj" : $(SOURCE) $(DEP_CPP_CLEAR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\clrbit.c
DEP_CPP_CLRBI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\clrbit.obj" : $(SOURCE) $(DEP_CPP_CLRBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\clrbit.obj" : $(SOURCE) $(DEP_CPP_CLRBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmp.c
DEP_CPP_CMP_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_CMP_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmp.obj" : $(SOURCE) $(DEP_CPP_CMP_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmp_d.c
DEP_CPP_CMP_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmp_d.obj" : $(SOURCE) $(DEP_CPP_CMP_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmp_d.obj" : $(SOURCE) $(DEP_CPP_CMP_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmp_si.c
DEP_CPP_CMP_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmp_si.obj" : $(SOURCE) $(DEP_CPP_CMP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmp_si.obj" : $(SOURCE) $(DEP_CPP_CMP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmp_ui.c
DEP_CPP_CMP_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmp_ui.obj" : $(SOURCE) $(DEP_CPP_CMP_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmp_ui.obj" : $(SOURCE) $(DEP_CPP_CMP_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmpabs.c
DEP_CPP_CMPAB=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmpabs.obj" : $(SOURCE) $(DEP_CPP_CMPAB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmpabs.obj" : $(SOURCE) $(DEP_CPP_CMPAB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmpabs_d.c
DEP_CPP_CMPABS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmpabs_d.obj" : $(SOURCE) $(DEP_CPP_CMPABS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmpabs_d.obj" : $(SOURCE) $(DEP_CPP_CMPABS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cmpabs_ui.c
DEP_CPP_CMPABS_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cmpabs_ui.obj" : $(SOURCE) $(DEP_CPP_CMPABS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cmpabs_ui.obj" : $(SOURCE) $(DEP_CPP_CMPABS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\com.c
DEP_CPP_COM_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\com.obj" : $(SOURCE) $(DEP_CPP_COM_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\com.obj" : $(SOURCE) $(DEP_CPP_COM_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cong.c
DEP_CPP_CONG_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cong.obj" : $(SOURCE) $(DEP_CPP_CONG_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cong.obj" : $(SOURCE) $(DEP_CPP_CONG_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cong_2exp.c
DEP_CPP_CONG_2=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cong_2exp.obj" : $(SOURCE) $(DEP_CPP_CONG_2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cong_2exp.obj" : $(SOURCE) $(DEP_CPP_CONG_2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\cong_ui.c
DEP_CPP_CONG_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\cong_ui.obj" : $(SOURCE) $(DEP_CPP_CONG_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\cong_ui.obj" : $(SOURCE) $(DEP_CPP_CONG_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\dive_ui.c
DEP_CPP_DIVE_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\dive_ui.obj" : $(SOURCE) $(DEP_CPP_DIVE_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\dive_ui.obj" : $(SOURCE) $(DEP_CPP_DIVE_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\divegcd.c
DEP_CPP_DIVEG=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\divegcd.obj" : $(SOURCE) $(DEP_CPP_DIVEG) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\divegcd.obj" : $(SOURCE) $(DEP_CPP_DIVEG) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\divexact.c
DEP_CPP_DIVEX=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\divexact.obj" : $(SOURCE) $(DEP_CPP_DIVEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\divexact.obj" : $(SOURCE) $(DEP_CPP_DIVEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\divis.c
DEP_CPP_DIVIS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\divis.obj" : $(SOURCE) $(DEP_CPP_DIVIS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\divis.obj" : $(SOURCE) $(DEP_CPP_DIVIS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\divis_2exp.c
DEP_CPP_DIVIS_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\divis_2exp.obj" : $(SOURCE) $(DEP_CPP_DIVIS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\divis_2exp.obj" : $(SOURCE) $(DEP_CPP_DIVIS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\divis_ui.c
DEP_CPP_DIVIS_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\divis_ui.obj" : $(SOURCE) $(DEP_CPP_DIVIS_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\divis_ui.obj" : $(SOURCE) $(DEP_CPP_DIVIS_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\dump.c
DEP_CPP_DUMP_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\dump.obj" : $(SOURCE) $(DEP_CPP_DUMP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\export.c
DEP_CPP_EXPOR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\export.obj" : $(SOURCE) $(DEP_CPP_EXPOR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\export.obj" : $(SOURCE) $(DEP_CPP_EXPOR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fac_ui.c
DEP_CPP_FAC_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fac_ui.obj" : $(SOURCE) $(DEP_CPP_FAC_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fac_ui.obj" : $(SOURCE) $(DEP_CPP_FAC_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_q.c
DEP_CPP_FDIV_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_q.obj" : $(SOURCE) $(DEP_CPP_FDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_q.obj" : $(SOURCE) $(DEP_CPP_FDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_q_ui.c
DEP_CPP_FDIV_Q=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_qr.c
DEP_CPP_FDIV_QR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_qr.obj" : $(SOURCE) $(DEP_CPP_FDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_qr.obj" : $(SOURCE) $(DEP_CPP_FDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_qr_ui.c
DEP_CPP_FDIV_QR_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_QR_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_QR_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_r.c
DEP_CPP_FDIV_R=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_r.obj" : $(SOURCE) $(DEP_CPP_FDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_r.obj" : $(SOURCE) $(DEP_CPP_FDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_r_ui.c
DEP_CPP_FDIV_R_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fdiv_ui.c
DEP_CPP_FDIV_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fdiv_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fdiv_ui.obj" : $(SOURCE) $(DEP_CPP_FDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fib2_ui.c
DEP_CPP_FIB2_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fib2_ui.obj" : $(SOURCE) $(DEP_CPP_FIB2_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fib2_ui.obj" : $(SOURCE) $(DEP_CPP_FIB2_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fib_ui.c
DEP_CPP_FIB_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fib_ui.obj" : $(SOURCE) $(DEP_CPP_FIB_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fib_ui.obj" : $(SOURCE) $(DEP_CPP_FIB_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_sint.c
DEP_CPP_FITS_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\fits_s.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_sint.obj" : $(SOURCE) $(DEP_CPP_FITS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_sint.obj" : $(SOURCE) $(DEP_CPP_FITS_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_slong.c
DEP_CPP_FITS_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\fits_s.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_slong.obj" : $(SOURCE) $(DEP_CPP_FITS_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_slong.obj" : $(SOURCE) $(DEP_CPP_FITS_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_sshort.c
DEP_CPP_FITS_SS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\fits_s.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_sshort.obj" : $(SOURCE) $(DEP_CPP_FITS_SS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_sshort.obj" : $(SOURCE) $(DEP_CPP_FITS_SS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_uint.c
DEP_CPP_FITS_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_uint.obj" : $(SOURCE) $(DEP_CPP_FITS_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_uint.obj" : $(SOURCE) $(DEP_CPP_FITS_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_ulong.c
DEP_CPP_FITS_UL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_ulong.obj" : $(SOURCE) $(DEP_CPP_FITS_UL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_ulong.obj" : $(SOURCE) $(DEP_CPP_FITS_UL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\fits_ushort.c
DEP_CPP_FITS_US=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\fits_ushort.obj" : $(SOURCE) $(DEP_CPP_FITS_US) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\fits_ushort.obj" : $(SOURCE) $(DEP_CPP_FITS_US) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\gcd.c
DEP_CPP_GCD_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_GCD_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\gcd.obj" : $(SOURCE) $(DEP_CPP_GCD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\gcd.obj" : $(SOURCE) $(DEP_CPP_GCD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\gcd_ui.c
DEP_CPP_GCD_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\gcd_ui.obj" : $(SOURCE) $(DEP_CPP_GCD_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\gcd_ui.obj" : $(SOURCE) $(DEP_CPP_GCD_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\gcdext.c
DEP_CPP_GCDEX=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\gcdext.obj" : $(SOURCE) $(DEP_CPP_GCDEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\gcdext.obj" : $(SOURCE) $(DEP_CPP_GCDEX) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\get_d.c
DEP_CPP_GET_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\get_d.obj" : $(SOURCE) $(DEP_CPP_GET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\get_d.obj" : $(SOURCE) $(DEP_CPP_GET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\get_d_2exp.c
DEP_CPP_GET_D_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\get_d_2exp.obj" : $(SOURCE) $(DEP_CPP_GET_D_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\get_d_2exp.obj" : $(SOURCE) $(DEP_CPP_GET_D_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\get_si.c
DEP_CPP_GET_SI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\get_si.obj" : $(SOURCE) $(DEP_CPP_GET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\get_si.obj" : $(SOURCE) $(DEP_CPP_GET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\get_str.c
DEP_CPP_GET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\get_str.obj" : $(SOURCE) $(DEP_CPP_GET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\get_ui.c
DEP_CPP_GET_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\get_ui.obj" : $(SOURCE) $(DEP_CPP_GET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\get_ui.obj" : $(SOURCE) $(DEP_CPP_GET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\getlimbn.c
DEP_CPP_GETLI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\getlimbn.obj" : $(SOURCE) $(DEP_CPP_GETLI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\getlimbn.obj" : $(SOURCE) $(DEP_CPP_GETLI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\hamdist.c
DEP_CPP_HAMDI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\hamdist.obj" : $(SOURCE) $(DEP_CPP_HAMDI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\hamdist.obj" : $(SOURCE) $(DEP_CPP_HAMDI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\import.c
DEP_CPP_IMPOR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\import.obj" : $(SOURCE) $(DEP_CPP_IMPOR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\import.obj" : $(SOURCE) $(DEP_CPP_IMPOR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\init.c
DEP_CPP_INIT_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\init.obj" : $(SOURCE) $(DEP_CPP_INIT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\init2.c
DEP_CPP_INIT2=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\init2.obj" : $(SOURCE) $(DEP_CPP_INIT2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\init2.obj" : $(SOURCE) $(DEP_CPP_INIT2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\inp_raw.c
DEP_CPP_INP_R=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\inp_raw.obj" : $(SOURCE) $(DEP_CPP_INP_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\inp_raw.obj" : $(SOURCE) $(DEP_CPP_INP_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\inp_str.c
DEP_CPP_INP_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\inp_str.obj" : $(SOURCE) $(DEP_CPP_INP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\inp_str.obj" : $(SOURCE) $(DEP_CPP_INP_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\invert.c
DEP_CPP_INVER=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\invert.obj" : $(SOURCE) $(DEP_CPP_INVER) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\invert.obj" : $(SOURCE) $(DEP_CPP_INVER) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\ior.c
DEP_CPP_IOR_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\ior.obj" : $(SOURCE) $(DEP_CPP_IOR_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\ior.obj" : $(SOURCE) $(DEP_CPP_IOR_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\iset.c
DEP_CPP_ISET_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\iset.obj" : $(SOURCE) $(DEP_CPP_ISET_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\iset.obj" : $(SOURCE) $(DEP_CPP_ISET_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\iset_d.c
DEP_CPP_ISET_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\iset_d.obj" : $(SOURCE) $(DEP_CPP_ISET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\iset_d.obj" : $(SOURCE) $(DEP_CPP_ISET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\iset_si.c
DEP_CPP_ISET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\iset_si.obj" : $(SOURCE) $(DEP_CPP_ISET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\iset_si.obj" : $(SOURCE) $(DEP_CPP_ISET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\iset_str.c
DEP_CPP_ISET_ST=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\iset_str.obj" : $(SOURCE) $(DEP_CPP_ISET_ST) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\iset_str.obj" : $(SOURCE) $(DEP_CPP_ISET_ST) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\iset_ui.c
DEP_CPP_ISET_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\iset_ui.obj" : $(SOURCE) $(DEP_CPP_ISET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\iset_ui.obj" : $(SOURCE) $(DEP_CPP_ISET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\jacobi.c
DEP_CPP_JACOB=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\jacobi.obj" : $(SOURCE) $(DEP_CPP_JACOB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\jacobi.obj" : $(SOURCE) $(DEP_CPP_JACOB) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\kronsz.c
DEP_CPP_KRONS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\kronsz.obj" : $(SOURCE) $(DEP_CPP_KRONS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\kronsz.obj" : $(SOURCE) $(DEP_CPP_KRONS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\kronuz.c
DEP_CPP_KRONU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\kronuz.obj" : $(SOURCE) $(DEP_CPP_KRONU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\kronuz.obj" : $(SOURCE) $(DEP_CPP_KRONU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\kronzs.c
DEP_CPP_KRONZ=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\kronzs.obj" : $(SOURCE) $(DEP_CPP_KRONZ) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\kronzs.obj" : $(SOURCE) $(DEP_CPP_KRONZ) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\kronzu.c
DEP_CPP_KRONZU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\kronzu.obj" : $(SOURCE) $(DEP_CPP_KRONZU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\kronzu.obj" : $(SOURCE) $(DEP_CPP_KRONZU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\lcm.c
DEP_CPP_LCM_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\lcm.obj" : $(SOURCE) $(DEP_CPP_LCM_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\lcm.obj" : $(SOURCE) $(DEP_CPP_LCM_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\lcm_ui.c
DEP_CPP_LCM_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\lcm_ui.obj" : $(SOURCE) $(DEP_CPP_LCM_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\lcm_ui.obj" : $(SOURCE) $(DEP_CPP_LCM_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\lucnum2_ui.c
DEP_CPP_LUCNU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\lucnum2_ui.obj" : $(SOURCE) $(DEP_CPP_LUCNU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\lucnum2_ui.obj" : $(SOURCE) $(DEP_CPP_LUCNU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\lucnum_ui.c
DEP_CPP_LUCNUM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\lucnum_ui.obj" : $(SOURCE) $(DEP_CPP_LUCNUM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\lucnum_ui.obj" : $(SOURCE) $(DEP_CPP_LUCNUM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\millerrabin.c
DEP_CPP_MILLE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\millerrabin.obj" : $(SOURCE) $(DEP_CPP_MILLE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\millerrabin.obj" : $(SOURCE) $(DEP_CPP_MILLE) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\mod.c
DEP_CPP_MOD_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\mod.obj" : $(SOURCE) $(DEP_CPP_MOD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\mod.obj" : $(SOURCE) $(DEP_CPP_MOD_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\mul.c
DEP_CPP_MUL_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_MUL_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\mul.obj" : $(SOURCE) $(DEP_CPP_MUL_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\mul_2exp.c
DEP_CPP_MUL_2=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\mul_2exp.obj" : $(SOURCE) $(DEP_CPP_MUL_2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\mul_2exp.obj" : $(SOURCE) $(DEP_CPP_MUL_2) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\mul_si.c
DEP_CPP_MUL_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\mul_i.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\mul_si.obj" : $(SOURCE) $(DEP_CPP_MUL_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\mul_si.obj" : $(SOURCE) $(DEP_CPP_MUL_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\mul_ui.c
DEP_CPP_MUL_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\mul_i.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\mul_ui.obj" : $(SOURCE) $(DEP_CPP_MUL_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\mul_ui.obj" : $(SOURCE) $(DEP_CPP_MUL_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\n_pow_ui.c
DEP_CPP_N_POW=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\n_pow_ui.obj" : $(SOURCE) $(DEP_CPP_N_POW) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\n_pow_ui.obj" : $(SOURCE) $(DEP_CPP_N_POW) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\neg.c
DEP_CPP_NEG_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\neg.obj" : $(SOURCE) $(DEP_CPP_NEG_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\neg.obj" : $(SOURCE) $(DEP_CPP_NEG_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\nextprime.c
DEP_CPP_NEXTP=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\nextprime.obj" : $(SOURCE) $(DEP_CPP_NEXTP) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\nextprime.obj" : $(SOURCE) $(DEP_CPP_NEXTP) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\out_raw.c
DEP_CPP_OUT_R=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\out_raw.obj" : $(SOURCE) $(DEP_CPP_OUT_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\out_raw.obj" : $(SOURCE) $(DEP_CPP_OUT_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\out_str.c
DEP_CPP_OUT_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\out_str.obj" : $(SOURCE) $(DEP_CPP_OUT_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\out_str.obj" : $(SOURCE) $(DEP_CPP_OUT_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\perfpow.c
DEP_CPP_PERFP=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\perfpow.obj" : $(SOURCE) $(DEP_CPP_PERFP) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\perfpow.obj" : $(SOURCE) $(DEP_CPP_PERFP) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\perfsqr.c
DEP_CPP_PERFS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\perfsqr.obj" : $(SOURCE) $(DEP_CPP_PERFS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\perfsqr.obj" : $(SOURCE) $(DEP_CPP_PERFS) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\popcount.c
DEP_CPP_POPCO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\popcount.obj" : $(SOURCE) $(DEP_CPP_POPCO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\popcount.obj" : $(SOURCE) $(DEP_CPP_POPCO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\pow_ui.c
DEP_CPP_POW_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\pow_ui.obj" : $(SOURCE) $(DEP_CPP_POW_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\pow_ui.obj" : $(SOURCE) $(DEP_CPP_POW_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\powm.c
DEP_CPP_POWM_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_POWM_=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\powm.obj" : $(SOURCE) $(DEP_CPP_POWM_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\powm.obj" : $(SOURCE) $(DEP_CPP_POWM_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\powm_ui.c
DEP_CPP_POWM_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\powm_ui.obj" : $(SOURCE) $(DEP_CPP_POWM_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\powm_ui.obj" : $(SOURCE) $(DEP_CPP_POWM_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\pprime_p.c
DEP_CPP_PPRIM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\pprime_p.obj" : $(SOURCE) $(DEP_CPP_PPRIM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\pprime_p.obj" : $(SOURCE) $(DEP_CPP_PPRIM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\random.c
DEP_CPP_RANDO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\random.obj" : $(SOURCE) $(DEP_CPP_RANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\random.obj" : $(SOURCE) $(DEP_CPP_RANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\random2.c
DEP_CPP_RANDOM=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\random2.obj" : $(SOURCE) $(DEP_CPP_RANDOM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\random2.obj" : $(SOURCE) $(DEP_CPP_RANDOM) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\realloc.c
DEP_CPP_REALL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\realloc.obj" : $(SOURCE) $(DEP_CPP_REALL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\realloc.obj" : $(SOURCE) $(DEP_CPP_REALL) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\realloc2.c
DEP_CPP_REALLO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\realloc2.obj" : $(SOURCE) $(DEP_CPP_REALLO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\realloc2.obj" : $(SOURCE) $(DEP_CPP_REALLO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\remove.c
DEP_CPP_REMOV=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\remove.obj" : $(SOURCE) $(DEP_CPP_REMOV) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\remove.obj" : $(SOURCE) $(DEP_CPP_REMOV) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\root.c
DEP_CPP_ROOT_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\root.obj" : $(SOURCE) $(DEP_CPP_ROOT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\root.obj" : $(SOURCE) $(DEP_CPP_ROOT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\rrandomb.c
DEP_CPP_RRAND=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\rrandomb.obj" : $(SOURCE) $(DEP_CPP_RRAND) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\rrandomb.obj" : $(SOURCE) $(DEP_CPP_RRAND) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\scan0.c
DEP_CPP_SCAN0=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\scan0.obj" : $(SOURCE) $(DEP_CPP_SCAN0) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\scan0.obj" : $(SOURCE) $(DEP_CPP_SCAN0) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\scan1.c
DEP_CPP_SCAN1=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\scan1.obj" : $(SOURCE) $(DEP_CPP_SCAN1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\scan1.obj" : $(SOURCE) $(DEP_CPP_SCAN1) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set.c
DEP_CPP_SET_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_SET_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set.obj" : $(SOURCE) $(DEP_CPP_SET_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set.obj" : $(SOURCE) $(DEP_CPP_SET_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_d.c
DEP_CPP_SET_D=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_d.obj" : $(SOURCE) $(DEP_CPP_SET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_d.obj" : $(SOURCE) $(DEP_CPP_SET_D) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_f.c
DEP_CPP_SET_F=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_f.obj" : $(SOURCE) $(DEP_CPP_SET_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_f.obj" : $(SOURCE) $(DEP_CPP_SET_F) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_q.c
DEP_CPP_SET_Q=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_q.obj" : $(SOURCE) $(DEP_CPP_SET_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_q.obj" : $(SOURCE) $(DEP_CPP_SET_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_si.c
DEP_CPP_SET_SI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_si.obj" : $(SOURCE) $(DEP_CPP_SET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_si.obj" : $(SOURCE) $(DEP_CPP_SET_SI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_str.c
DEP_CPP_SET_S=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_str.obj" : $(SOURCE) $(DEP_CPP_SET_S) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\set_ui.c
DEP_CPP_SET_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\set_ui.obj" : $(SOURCE) $(DEP_CPP_SET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\set_ui.obj" : $(SOURCE) $(DEP_CPP_SET_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\setbit.c
DEP_CPP_SETBI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\setbit.obj" : $(SOURCE) $(DEP_CPP_SETBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\setbit.obj" : $(SOURCE) $(DEP_CPP_SETBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\size.c
DEP_CPP_SIZE_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\size.obj" : $(SOURCE) $(DEP_CPP_SIZE_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\size.obj" : $(SOURCE) $(DEP_CPP_SIZE_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\sizeinbase.c
DEP_CPP_SIZEI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\sizeinbase.obj" : $(SOURCE) $(DEP_CPP_SIZEI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\sizeinbase.obj" : $(SOURCE) $(DEP_CPP_SIZEI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\sqrt.c
DEP_CPP_SQRT_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\sqrt.obj" : $(SOURCE) $(DEP_CPP_SQRT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\sqrt.obj" : $(SOURCE) $(DEP_CPP_SQRT_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\sqrtrem.c
DEP_CPP_SQRTR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_SQRTR=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\sqrtrem.obj" : $(SOURCE) $(DEP_CPP_SQRTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\sqrtrem.obj" : $(SOURCE) $(DEP_CPP_SQRTR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\sub.c
DEP_CPP_SUB_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\aors.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_SUB_C=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\sub.obj" : $(SOURCE) $(DEP_CPP_SUB_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\sub.obj" : $(SOURCE) $(DEP_CPP_SUB_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\sub_ui.c
DEP_CPP_SUB_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\Gmp\src\mpz\aors_ui.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\sub_ui.obj" : $(SOURCE) $(DEP_CPP_SUB_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\sub_ui.obj" : $(SOURCE) $(DEP_CPP_SUB_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\swap.c
DEP_CPP_SWAP_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\swap.obj" : $(SOURCE) $(DEP_CPP_SWAP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\swap.obj" : $(SOURCE) $(DEP_CPP_SWAP_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_q.c
DEP_CPP_TDIV_Q=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_q.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_q.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_q_2exp.c
DEP_CPP_TDIV_Q_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_q_2exp.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_q_2exp.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_q_ui.c
DEP_CPP_TDIV_Q_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_q_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_Q_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_qr.c
DEP_CPP_TDIV_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
NODEP_CPP_TDIV_=\
	"..\..\Gmp\src\mpz\mp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_qr.obj" : $(SOURCE) $(DEP_CPP_TDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_qr.obj" : $(SOURCE) $(DEP_CPP_TDIV_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_qr_ui.c
DEP_CPP_TDIV_QR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_qr_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_QR) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_r.c
DEP_CPP_TDIV_R=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_r.obj" : $(SOURCE) $(DEP_CPP_TDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_r.obj" : $(SOURCE) $(DEP_CPP_TDIV_R) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_r_2exp.c
DEP_CPP_TDIV_R_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_r_2exp.obj" : $(SOURCE) $(DEP_CPP_TDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_r_2exp.obj" : $(SOURCE) $(DEP_CPP_TDIV_R_) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_r_ui.c
DEP_CPP_TDIV_R_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_R_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_r_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_R_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tdiv_ui.c
DEP_CPP_TDIV_U=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tdiv_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tdiv_ui.obj" : $(SOURCE) $(DEP_CPP_TDIV_U) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\tstbit.c
DEP_CPP_TSTBI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\tstbit.obj" : $(SOURCE) $(DEP_CPP_TSTBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\tstbit.obj" : $(SOURCE) $(DEP_CPP_TSTBI) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\ui_pow_ui.c
DEP_CPP_UI_PO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\ui_pow_ui.obj" : $(SOURCE) $(DEP_CPP_UI_PO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\ui_pow_ui.obj" : $(SOURCE) $(DEP_CPP_UI_PO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\ui_sub.c
DEP_CPP_UI_SU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\ui_sub.obj" : $(SOURCE) $(DEP_CPP_UI_SU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\ui_sub.obj" : $(SOURCE) $(DEP_CPP_UI_SU) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\urandomb.c
DEP_CPP_URAND=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\urandomb.obj" : $(SOURCE) $(DEP_CPP_URAND) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\urandomb.obj" : $(SOURCE) $(DEP_CPP_URAND) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\urandomm.c
DEP_CPP_URANDO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\urandomm.obj" : $(SOURCE) $(DEP_CPP_URANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\urandomm.obj" : $(SOURCE) $(DEP_CPP_URANDO) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\mpz\xor.c
DEP_CPP_XOR_C=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	
INTDIR_SRC=.\ReleaseMPZ
"$(INTDIR_SRC)" :
    if not exist "$(INTDIR_SRC)/$(NULL)" mkdir "$(INTDIR_SRC)"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

CPP_SWITCHES=/nologo /MT /W3 /GX /O2 /I "../.." /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "NDEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Release/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\ReleaseMPZ\xor.obj" : $(SOURCE) $(DEP_CPP_XOR_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

CPP_SWITCHES=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "../../Gmp/src/win32eclipse"\
 /I "../../Gmp/src" /D "_DEBUG" /D "_WIN32" /D "__GMP_WITHIN_GMP" /D "_MSC_VER"\
 /D "_BRG_VER" /D "__STDC__" /D "DLL_EXPORT" /Fp"Debug/GmpDll.pch" /YX\
 /Fo"$(INTDIR_SRC)\\" /Fd"$(INTDIR_SRC)\\" /FD /c 

".\DebugMPZ\xor.obj" : $(SOURCE) $(DEP_CPP_XOR_C) "$(INTDIR_SRC)"
	$(CPP) @<<
  $(CPP_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=..\..\Gmp\src\ansi2knr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"


"$(INTDIR)\ansi2knr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_ANSI2=\
	"..\..\gmp\src\win32eclipse\config.h"\
	

"$(INTDIR)\ansi2knr.obj" : $(SOURCE) $(DEP_CPP_ANSI2) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\assert.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_ASSER=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\assert.obj" : $(SOURCE) $(DEP_CPP_ASSER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_ASSER=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\assert.obj" : $(SOURCE) $(DEP_CPP_ASSER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\compat.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_COMPA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\compat.obj" : $(SOURCE) $(DEP_CPP_COMPA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_COMPA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\compat.obj" : $(SOURCE) $(DEP_CPP_COMPA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\errno.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_ERRNO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\errno.obj" : $(SOURCE) $(DEP_CPP_ERRNO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_ERRNO=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\errno.obj" : $(SOURCE) $(DEP_CPP_ERRNO) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE="..\..\Gmp\src\extract-dbl.c"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_EXTRA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\extract-dbl.obj" : $(SOURCE) $(DEP_CPP_EXTRA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_EXTRA=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\extract-dbl.obj" : $(SOURCE) $(DEP_CPP_EXTRA) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE="..\..\Gmp\src\insert-dbl.c"

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_INSER=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\insert-dbl.obj" : $(SOURCE) $(DEP_CPP_INSER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_INSER=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\insert-dbl.obj" : $(SOURCE) $(DEP_CPP_INSER) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\memory.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_MEMOR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\memory.obj" : $(SOURCE) $(DEP_CPP_MEMOR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_MEMOR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\memory.obj" : $(SOURCE) $(DEP_CPP_MEMOR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\mp_bpl.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_MP_BP=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_bpl.obj" : $(SOURCE) $(DEP_CPP_MP_BP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_MP_BP=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_bpl.obj" : $(SOURCE) $(DEP_CPP_MP_BP) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\mp_clz_tab.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_MP_CL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_clz_tab.obj" : $(SOURCE) $(DEP_CPP_MP_CL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_MP_CL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_clz_tab.obj" : $(SOURCE) $(DEP_CPP_MP_CL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\mp_minv_tab.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_MP_MI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_minv_tab.obj" : $(SOURCE) $(DEP_CPP_MP_MI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_MP_MI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_minv_tab.obj" : $(SOURCE) $(DEP_CPP_MP_MI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\mp_set_fns.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_MP_SE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_set_fns.obj" : $(SOURCE) $(DEP_CPP_MP_SE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_MP_SE=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\mp_set_fns.obj" : $(SOURCE) $(DEP_CPP_MP_SE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\rand.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RAND_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\rand.obj" : $(SOURCE) $(DEP_CPP_RAND_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RAND_=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\rand.obj" : $(SOURCE) $(DEP_CPP_RAND_) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randclr.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDC=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randclr.obj" : $(SOURCE) $(DEP_CPP_RANDC) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDC=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randclr.obj" : $(SOURCE) $(DEP_CPP_RANDC) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randdef.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDD=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randdef.obj" : $(SOURCE) $(DEP_CPP_RANDD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDD=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randdef.obj" : $(SOURCE) $(DEP_CPP_RANDD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randlc.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc.obj" : $(SOURCE) $(DEP_CPP_RANDL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDL=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc.obj" : $(SOURCE) $(DEP_CPP_RANDL) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randlc2s.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDLC=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc2s.obj" : $(SOURCE) $(DEP_CPP_RANDLC) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDLC=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc2s.obj" : $(SOURCE) $(DEP_CPP_RANDLC) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randlc2x.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDLC2=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc2x.obj" : $(SOURCE) $(DEP_CPP_RANDLC2) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDLC2=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randlc2x.obj" : $(SOURCE) $(DEP_CPP_RANDLC2) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randraw.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randraw.obj" : $(SOURCE) $(DEP_CPP_RANDR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDR=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\longlong.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randraw.obj" : $(SOURCE) $(DEP_CPP_RANDR) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\rands.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\rands.obj" : $(SOURCE) $(DEP_CPP_RANDS) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDS=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\rands.obj" : $(SOURCE) $(DEP_CPP_RANDS) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randsd.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDSD=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randsd.obj" : $(SOURCE) $(DEP_CPP_RANDSD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDSD=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randsd.obj" : $(SOURCE) $(DEP_CPP_RANDSD) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\randsdui.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_RANDSDU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randsdui.obj" : $(SOURCE) $(DEP_CPP_RANDSDU) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_RANDSDU=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\randsdui.obj" : $(SOURCE) $(DEP_CPP_RANDSDU) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\..\Gmp\src\version.c

!IF  "$(CFG)" == "GmpDll - Win32 Release"

DEP_CPP_VERSI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\version.obj" : $(SOURCE) $(DEP_CPP_VERSI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "GmpDll - Win32 Debug"

DEP_CPP_VERSI=\
	"..\..\gmp\src\gmp-impl.h"\
	"..\..\gmp\src\win32eclipse\config.h"\
	"..\..\gmp\src\win32eclipse\gmp-mparam.h"\
	"..\..\gmp\src\win32eclipse\gmp.h"\
	

"$(INTDIR)\version.obj" : $(SOURCE) $(DEP_CPP_VERSI) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

