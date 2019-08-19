/* -*- mode: C; tab-width: 2; indent-tabs-mode: nil; -*- */

#include <stdio.h>
#include <stdlib.h>     /* srand, rand */
#include <time.h>       /* time */

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#else
#include <bench.h>
#endif



/* Random number generator */
#ifdef LONG_IS_64BITS
#define POLY 0x0000000000000007UL
#define PERIOD 1317624576693539401L
#else
#define POLY 0x0000000000000007ULL
#define PERIOD 1317624576693539401LL
#endif


#define WANT_MPI2_TEST 0


#define HPCC_TRUE 1
#define HPCC_FALSE 0
#define HPCC_DONE 0

#define FINISHED_TAG 1
#define UPDATE_TAG   2
#define USE_NONBLOCKING_SEND 1

#define MAX_TOTAL_PENDING_UPDATES 1024
#define LOCAL_BUFFER_SIZE MAX_TOTAL_PENDING_UPDATES

#define USE_MULTIPLE_RECV 1

#ifdef USE_MULTIPLE_RECV
#define MAX_RECV					16
#else
#define MAX_RECV					1
#endif

#define LCG_MUL64 6364136223846793005ULL
#define LCG_ADD64 1

extern uint64_t *HPCC_Table;

extern uint64_t LocalSendBuffer[LOCAL_BUFFER_SIZE];
extern uint64_t LocalRecvBuffer[MAX_RECV*LOCAL_BUFFER_SIZE];

typedef struct HPCC_RandomAccess_tabparams_s {
  int64_t LocalTableSize; /* local size of the table may be rounded up >= MinLocalTableSize */
  int64_t ProcNumUpdates; /* usually 4 times the local size except for time-bound runs */

  uint64_t logTableSize;   /* it is an unsigned 64-bit value to type-promote expressions */
  uint64_t TableSize;      /* always power of 2 */
  uint64_t MinLocalTableSize; /* TableSize/NumProcs */
  uint64_t GlobalStartMyProc; /* first global index of the global table stored locally */
  uint64_t Top; /* global indices below 'Top' are asigned in MinLocalTableSize+1 blocks;
                 above 'Top' -- in MinLocalTableSize blocks */
/*
  MPI_Datatype dtype64;
  MPI_Status *finish_statuses; // storage for 'NumProcs' worth of statuses
  MPI_Request *finish_req;     // storage for 'NumProcs' worth of requests
*/
  int logNumProcs, NumProcs, MyProc;

  int Remainder; /* TableSize % NumProcs */
} HPCC_RandomAccess_tabparams_t;

typedef struct HPCC_Params_s {
    double HPLMaxProcMem;
    int64_t RandomAccess_N;
    size_t TableAlignment;
    uint32_t NumReps;
} HPCC_Params;

extern void AnyNodesMPIRandomAccessUpdate(HPCC_RandomAccess_tabparams_t tparams);
extern void Power2NodesMPIRandomAccessUpdate(HPCC_RandomAccess_tabparams_t tparams);
extern void HPCC_AnyNodesMPIRandomAccessUpdate_LCG(HPCC_RandomAccess_tabparams_t tparams);
extern void HPCC_Power2NodesMPIRandomAccessUpdate_LCG(HPCC_RandomAccess_tabparams_t tparams);

extern int HPCC_RandomAccess(HPCC_Params *params, int doIO, double *GUPs, int *failure);
extern int HPCC_RandomAccess_LCG(HPCC_Params *params, int doIO, double *GUPs, int *failure);

extern void HPCC_Power2NodesMPIRandomAccessCheck(HPCC_RandomAccess_tabparams_t tparams, int64_t *NumErrors);
extern void HPCC_AnyNodesMPIRandomAccessCheck(HPCC_RandomAccess_tabparams_t tparams, int64_t *NumErrors);
extern void HPCC_Power2NodesMPIRandomAccessCheck_LCG(HPCC_RandomAccess_tabparams_t tparams, int64_t *NumErrors);
extern void HPCC_AnyNodesMPIRandomAccessCheck_LCG(HPCC_RandomAccess_tabparams_t tparams, int64_t *NumErrors);

#if defined( RA_SANDIA_NOPT )
#define HPCC_RA_ALGORITHM 1
#elif defined( RA_SANDIA_OPT2 )
#define HPCC_RA_ALGORITHM 2
#else
#define HPCC_RA_STDALG 1
#define HPCC_RA_ALGORITHM 0
#endif

uint64_t parse_memory(char *str);
void common_main(int argc, char *argv[], HPCC_Params *params);
void HPCC_free(void *addr);
void *HPCC_malloc(size_t bytes, size_t alignment);

#ifndef BARRELFISH
#include <sys/time.h>
#include <sys/resource.h>
static inline uint64_t get_timems()
        {
            struct timeval t;
            gettimeofday(&t, NULL);
            return (uint64_t)(t.tv_sec * 1000.0 + t.tv_usec*1e-3);
        }
#endif
