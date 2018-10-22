    
            DARPA/DOE HPC Challenge Benchmark version 1.4.2
            ***********************************************
                           Piotr Luszczek (1)
                           ==================
                            October 12, 2012
                            ================
  


1  Introduction
*=*=*=*=*=*=*=*

   This is a suite of benchmarks that measure performance of processor,
memory subsytem, and the interconnect. For details refer to the
HPC Challenge web site (http://icl.cs.utk.edu/hpcc/.)
  In essence, HPC Challenge consists of a number of tests each of which
measures performance of a different aspect of the system.
  If you are familiar with the High Performance Linpack (HPL) benchmark
code (see the HPL web site: http://www.netlib.org/benchmark/hpl/) then
you can reuse the build script file (input for make(1) command) and the
input file that you already have for HPL. The HPC Challenge benchmark
includes HPL and uses its build script and input files with only slight
modifications. The most important change must be done to the line that
sets the TOPdir variable. For HPC Challenge, the variable's value should
always be ../../.. regardless of what it was in the HPL build script
file.


2  Compiling
*=*=*=*=*=*=

   The first step is to create a build script file that reflects
characteristics of your machine. This file is reused by all the
components of the HPC Challenge suite. The build script file should be
created in the hpl directory. This directory contains instructions (the
files README and INSTALL) on how to create the build script file for
your system. The hpl/setup directory contains many examples of build
script files. A recommended approach is to copy one of them to the hpl
directory and if it doesn't work then change it.
  The build script file has a name that starts with Make. prefix and
usally ends with a suffix that identifies the target system. For
example, if the suffix chosen for the system is Unix, the file should be
named Make.Unix.
  To build the benchmark executable (for the system named Unix) type:
make arch=Unix. This command should be run in the top directory (not in
the hpl directory). It will look in the hpl directory for the build
script file and use it to build the benchmark executable.
  The runtime behavior of the HPC Challenge source code may be
configured at compiled time by defining a few C preprocessor symbols.
They can be defined by adding appropriate options to CCNOOPT and CCFLAGS
make variables. The former controls options for source code files that
need to be compiled without aggressive optimizations to ensure accurate
generation of system-specific parameters. The latter applies to the rest
of the files that need good compiler optimization for best performance.
To define a symbol S, the majority of compilers requires option -DS to
be used. Currently, the following options are available in the
HPC Challenge source code: 
 
 
   - HPCC_FFT_235: if this symbol is defined the FFTE code (an FFT
   implementation) will use vector sizes and processor counts that are
   not limited to powers of 2. Instead, the vector sizes and processor
   counts to be used will be a product of powers of 2, 3, and 5.
 
   - HPCC_FFTW_ESTIMATE: if this symbol is defined it will affect the
   way external FFTW library is called (it does not have any effect if
   the FFTW library is not used). When defined, this symbol will call
   the FFTW planning routine with FFTW_ESTIMATE flag (instead of
   FFTW_MEASURE). This might result with worse performance results but
   shorter execution time of the benchmark. Defining this symbol may
   also positively affect the memory fragmentation caused by the FFTW's
   planning routine.
 
   - HPCC_MEMALLCTR: if this symbol is defined a custom memory allocator
   will be used to alleviate effects of memory fragmentation and allow
   for larger data sets to be used which may result in obtaining better
   performance.
 
   - HPL_USE_GETPROCESSTIMES: if this symbol is defined then
   Windows-specific GetProcessTimes() function will be used to measure
   the elapsed CPU time.
 
   - USE_MULTIPLE_RECV: if this symbol is defined then multiple
   non-blocking receives will be posted simultaneously. By default only
   one non-blocking receive is posted.
 
   - RA_SANDIA_NOPT: if this symbol is defined the HPC Challenge
   standard algorithm for Global RandomAccess will not be used. Instead,
   an alternative implementation from Sandia National Laboratory will be
   used. It routes messages in software across virtual hyper-cube
   topology formed from MPI processes.
 
   - RA_SANDIA_OPT2: if this symbol is defined the HPC Challenge
   standard algorithm for Global RandomAccess will not be used. Instead,
   instead an alternative implementation from Sandia National Laboratory
   will be used. This implementation is optimized for number of
   processors being powers of two. The optimizations are sorting of data
   before sending and unrolling the data update loop. If the number of
   process is not a power two then the code is the same as the one
   performed with the RA_SANDIA_NOPT setting.
 
   - RA_TIME_BOUND_DISABLE: if this symbol is defined then the standard
   Global RandomAccess code will be used without time limits. This is
   discouraged for most runs because the standard algorithm tends to be
   slow for large array sizes due to a large overhead for short MPI
   messages.
 
   - USING_FFTW: if this symbol is defined the standard HPC Challenge
   FFT implemenation (called FFTE) will not be used. Instead, FFTW
   library will be called. Defining the USING_FFTW symbol is not
   sufficient: appropriate flags have to be added in the make script so
   that FFTW headers files can be found at compile time and the FFTW
   libraries at link time.
  


3  Runtime Configuration
*=*=*=*=*=*=*=*=*=*=*=*=

   The HPC Challenge is driven by a short input file named hpccinf.txt
that is almost the same as the input file for HPL (customarily called
HPL.dat). Refer to the directory hpl/www/tuning.html for details about
the input file for HPL. A sample input file is included with the
HPC Challenge distribution.
  The differences between HPL's input file and HPC Challenge's input
file can be summarized as follows:
  
  
   - Lines 3 and 4 are ignored. The output is always appended to the
   file named hpccoutf.txt. 
   - There are additional lines (starting with line 33) that may (but do
   not have to) be used to customize the HPC Challenge benchmark. They
   are described below. 
  
  The additional lines in the HPC Challenge input file (compared to the
HPL input file) are:
  
  
   - Lines 33 and 34 describe additional matrix sizes to be used for
   running the PTRANS benchmark (one of the components of the
   HPC Challenge benchmark). 
   - Lines 35 and 36 describe additional blocking factors to be used for
   running the PTRANS test. 
  
  Just for completeness, here is the list of lines of the HPC
Challenge's input file and brief description of their meaning: 
  
   - Line 1: ignored 
   - Line 2: ignored 
   - Line 3: ignored 
   - Line 4: ignored 
   - Line 5: number of matrix sizes for HPL (and PTRANS) 
   - Line 6: matrix sizes for HPL (and PTRANS) 
   - Line 7: number of blocking factors for HPL (and PTRANS) 
   - Line 8: blocking factors for HPL (and PTRANS) 
   - Line 9: type of process ordering for HPL 
   - Line 10: number of process grids for HPL (and PTRANS) 
   - Line 11: numbers of process rows of each process grid for HPL (and
   PTRANS) 
   - Line 12: numbers of process columns of each process grid for HPL
   (and PTRANS) 
   - Line 13: threshold value not to be exceeded by scaled residual for
   HPL (and PTRANS) 
   - Line 14: number of panel factorization methods for HPL 
   - Line 15: panel factorization methods for HPL 
   - Line 16: number of recursive stopping criteria for HPL 
   - Line 17: recursive stopping criteria for HPL 
   - Line 18: number of recursion panel counts for HPL 
   - Line 19: recursion panel counts for HPL 
   - Line 20: number of recursive panel factorization methods for HPL 
   - Line 21: recursive panel factorization methods for HPL 
   - Line 22: number of broadcast methods for HPL 
   - Line 23: broadcast methods for HPL 
   - Line 24: number of look-ahead depths for HPL 
   - Line 25: look-ahead depths for HPL 
   - Line 26: swap methods for HPL 
   - Line 27: swapping threshold for HPL 
   - Line 28: form of L1 for HPL 
   - Line 29: form of U for HPL 
   - Line 30: value that specifies whether equilibration should be used
   by HPL 
   - Line 31: memory alignment for HPL 
   - Line 32: ignored 
   - Line 33: number of additional problem sizes for PTRANS 
   - Line 34: additional problem sizes for PTRANS 
   - Line 35: number of additional blocking factors for PTRANS 
   - Line 36: additional blocking factors for PTRANS 
  


4  Running
*=*=*=*=*=

   The exact way to run the HPC Challenge benchmark depends on the MPI
implementation and system details. An example command to run the
benchmark could like like this: mpirun -np 4 hpcc. The meaning of the
command's components is as follows: 
  
   - mpirun is the command that starts execution of an MPI code.
   Depending on the system, it might also be aprun, mpiexec, mprun, poe,
   or something appropriate for your computer.
 
   - -np 4 is the argument that specifies that 4 MPI processes should be
   started. The number of MPI processes should be large enough to
   accomodate all the process grids specified in the hpccinf.txt file.
 
   - hpcc is the name of the HPC Challenge executable to run. 
  
  After the run, a file called hpccoutf.txt is created. It contains
results of the benchmark. This file should be uploaded through the web
form at the HPC Challenge website.


5  Source Code Changes across Versions (ChangeLog)
*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=

  


5.1  Version 1.4.3 (2013-08-26)
===============================
   
  
   1. Increased the size of scratch vector for local FFT tests that was
   missed in the previous version (reported by SGI). 
   2. Added Makefile for Blue Gene/P contributed by Vasil Tsanov. 
  


5.2  Version 1.4.2 (2012-10-12)
===============================
   
  
   1. Increased sizes of scratch vectors for local FFT tests to account
   for runs on systems with large main memory (reported by IBM, SGI and
   Intel). 
   2. Reduced vector size for local FFT tests due to larger scratch
   space needed. 
   3. Added a type cast to prevent overflow of a 32-bit integer vector
   size in FFT data generation routine (reported by IBM). 
   4. Fixed variable types to handle array sizes that overflow 32-bit
   integers in RandomAccess (reported by IBM and SGI). 
   5. Changed time-bound code to be used by default in Global
   RandomAccess and allowed for it to be switched off with a compile
   time flag if necessary. 
   6. Code cleanup to allow compilation without warnings of RandomAccess
   test. 
   7. Changed communication code in PTRANS to avoid large message sizes
   that caused problems in some MPI implementations. 
   8. Updated documentation in README.txt and README.html files. 
  


5.3  Version 1.4.1 (2010-06-01)
===============================
   
  
   1. Added optimized variants of RandomAccess that use Linear
   Congruential Generator for random number generation. 
   2. Made corrections to comments that provide definition of the
   RandomAccess test. 
   3. Removed initialization of the main array from the timed section of
   optimized versions of RandomAccess. 
   4. Fixed the length of the vector used to compute error when using
   MPI implementation from FFTW. 
   5. Added global reduction to error calculation in MPI FFT to achieve
   more accurate error estimate. 
   6. Updated documentation in README. 
  


5.4  Version 1.4.0 (2010-03-26)
===============================
   
  
   1. Added new variant of RandomAccess that uses Linear Congruential
   Generator for random number generation. 
   2. Rearranged the order of benchmarks so that HPL component runs last
   and may be aborted if the performance of other components was not
   satisfactory. RandomAccess is now first to assist in tuning the code.
   
   3. Added global initialization and finalization routine that allows
   to properly initialize and finalize external software and hardware
   components without changing the rest of the HPCC testing harness. 
   4. Lack of hpccinf.txt is no longer reported as error but as a
   warning. 
  


5.5  Version 1.3.2 (2009-03-24)
===============================
   
  
   1. Fixed memory leaks in G-RandomAccess driver routine. 
   2. Made the check for 32-bit vector sizes in G-FFT optional. MKL
   allows for 64-bit vector sizes in its FFTW wrapper. 
   3. Fixed memory bug in single-process FFT. 
   4. Update documentation (README). 
  


5.6  Version 1.3.1 (2008-12-09)
===============================
   
  
   1. Fixed a dead-lock problem in FFT component due to use of wrong
   communicator. 
   2. Fixed the 32-bit random number generator in PTRANS that was using
   64-bit routines from HPL. 
  


5.7  Version 1.3.0 (2008-11-13)
===============================
   
  
   1. Updated HPL component to use HPL 2.0 source code 
     
      1. Replaced 32-bit Pseudo Random Number Generator (PRNG) with a
      64-bit one. 
      2. Removed 3 numerical checks of the solution residual with a
      single one. 
      3. Added support for 64-bit systems with large memory sizes
      (before they would overflow during index calculations 32-bit
      integers.) 
  
   2. Introduced a limit on FFT vector size so they fit in a 32-bit
   integer (only applicable when using FFTW version 2.) 
  


5.8  Version 1.2.0 (2007-06-25)
===============================
  
  
  
   1. Changes in the FFT component: 
     
      1. Added flexibility in choosing vector sizes and processor
      counts: now the code can do powers of 2, 3, and 5 both
      sequentially and in parallel tests. 
      2. FFTW can now run with ESTIMATE (not just MEASURE) flag: it
      might produce worse performance results but often reduces time to
      run the test and cuases less memory fragmentation. 
  
   2. Changes in the DGEMM component: 
     
      1. Added more comprehensive checking of the numerical properties
      of the test's results. 
  
   3. Changes in the RandomAccess component: 
     
      1. Removed time-bound functionality: only runs that perform
      complete computation are now possible. 
      2. Made the timing more accurate: main array initialization is not
      counted towards performance timing. 
      3. Cleaned up the code: some non-portable C language constructs
      have been removed. 
      4. Added new algorithms: new algorithms from Sandia based on
      hypercube network topology can now be chosen at compile time which
      results on much better performance results on many types of
      parallel systems. 
      5. Fixed potential resource leaks by adding function calls rquired
      by the MPI standard. 
  
   4. Changes in the HPL component: 
     
      1. Cleaned up reporting of numerics: more accurate printing of
      scaled residual formula. 
  
   5. Changes in the PTRANS component: 
     
      1. Added randomization of virtual process grids to measure
      bandwidth of the network more accurately. 
  
   6. Miscellaneous changes: 
     
      1. Added better support for Windows-based clusters by taking
      advantage of Win32 API. 
      2. Added custom memory allocator to deal with memory fragmentation
      on some systems. 
      3. Added better reporting of configuration options in the output
      file. 
  
  


5.9  Version 1.0.0 (2005-06-11)
===============================
  


5.10  Version 0.8beta (2004-10-19)
==================================
  


5.11  Version 0.8alpha (2004-10-15)
===================================
  


5.12  Version 0.6beta (2004-08-21)
==================================
  


5.13  Version 0.6alpha (2004-05-31)
===================================
  


5.14  Version 0.5beta (2003-12-01)
==================================
  


5.15  Version 0.4alpha (2003-11-13)
===================================
  


5.16  Version 0.3alpha (2004-11-05)
===================================
  
-----------------------------------------------------------------------
  
   This document was translated from LaTeX by HeVeA (2).
-----------------------------------
  
  
 (1) University of Tennessee Knoxville, Innovative Computing Laboratory
 
 (2) http://hevea.inria.fr/index.html
