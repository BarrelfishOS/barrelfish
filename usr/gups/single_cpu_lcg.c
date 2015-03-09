/* -*- mode: C; tab-width: 2; indent-tabs-mode: nil; -*-
 *
 * This file contains the interface for the single cpu RandomAccess test.  The
 * test is only run on a single (random) node in the MPI universe, with all
 * other CPUs stuck (in theory, idle) in an MPI_Bcast waiting for the selected
 * CPU to finish the RandomAccess test.
 *
 * This test uses the computational core found in core_single_cpu.c
 */

#include "RandomAccess.h"

int main(int argc, char *argv[])
{

    int errCount, failure = 0;
    double localGUPs;

    HPCC_Params params;
    localGUPs = 0.0;

    common_main(argc, argv, &params);

#ifdef HAVE_MPI
    int rv, rank;
    int myRank, commSize;
    double scl = 1.0 / RAND_MAX;
    FILE *outputFile = NULL;
    MPI_Comm comm = MPI_COMM_WORLD;
    MPI_Comm_size( comm, &commSize );
    MPI_Comm_rank( comm, &myRank );
    scl *= commSize;

    /* select a node at random, but not node 0 (unless there is just one node) */
    if (1 == commSize)
    rank = 0;
    else
    for (rank = 0;; rank = (int)(scl * rand())) {
        if (rank > 0 && rank < commSize)
        break;
    }

    MPI_Bcast( &rank, 1, MPI_INT, 0, comm ); /* broadcast the rank selected on node 0 */

    if (myRank == rank) /* if this node has been selected */
    rv = HPCC_RandomAccess_LCG( params, 0 == myRank, &localGUPs, &failure );

    MPI_Bcast( &rv, 1, MPI_INT, rank, comm ); /* broadcast error code */
    MPI_Bcast( &localGUPs, 1, MPI_DOUBLE, rank, comm ); /* broadcast GUPs */
    MPI_Bcast( &failure, 1, MPI_INT, rank, comm ); /* broadcast failure indication */
    errCount = rv;
    params->Single_LCG_GUPs = localGUPs;
    if (failure) params->Failure = 1;
#else
    errCount = HPCC_RandomAccess_LCG( &params, 1, &localGUPs, &failure );
#endif

    //printf("Node(s) with error %d\n", errCount);
    //printf("Node selected %d\n", rank);
    printf("GUPS=%.6f\n", localGUPs);

    printf("# GUPS done.\n");

    return 0;
}
