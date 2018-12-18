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

#ifdef DUNE
#include <dune.h>
#endif

int main(int argc, char *argv[])
{
    /* just one node */

#ifdef DUNE
	if (dune_init_and_enter())
		errx(1, "dune_init_and_enter()");
#endif

    int errCount, failure = 0;
    double localGUPs;
    HPCC_Params params;
    localGUPs = 0.0;

    common_main(argc, argv, &params);

#ifdef HAVE_MPI
    int myRank = 0, commSize = 1;
    int rv, rank;
    scl *= commSize;
    double scl = 1.0 / RAND_MAX;
    /* select a node at random, but not node 0 (unless there is just one node) */

    if (1 == commSize) {
        rank = 0;
    } else {
        for (rank = 0;; rank = (int) (scl * rand())) {
            if (rank > 0 && rank < commSize)
                break;
        }
    }

    MPI_Bcast(&rank, 1, MPI_INT, 0, comm); /* broadcast the rank selected on node 0 */
    if (myRank == rank) {
        /* if this node has been selected */
        rv = HPCC_RandomAccess(&params, 0 == myRank, &localGUPs, &failure);
    }
    MPI_Bcast(&rv, 1, MPI_INT, rank, comm); /* broadcast error code */
    MPI_Bcast(&localGUPs, 1, MPI_DOUBLE, rank, comm); /* broadcast GUPs */
    MPI_Bcast(&failure, 1, MPI_INT, rank, comm); /* broadcast failure indication */
#else
    errCount = HPCC_RandomAccess(&params, 1, &localGUPs, &failure);
#endif
    if (errCount) {
        printf("ERROR: an error occured\n");
    }

    //printf("Node(s) with error %d\n", errCount);
    //printf("Node selected %d\n", rank);
    printf("GUPS=%.6f\n", localGUPs);

    printf("# GUPS done.\n");

    return 0;
}
