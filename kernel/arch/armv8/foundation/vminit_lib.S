/* Utility routines for booting. */

.section .vminit, "ax"

.global eret

/* Arguments x0-x3 are passed through */
eret:
    eret
