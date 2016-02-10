/* tests the montgomery routines */
#include <tommath.h>

int main(int argc, char **argv)
{
   mp_int modulus, R, p, pp;
   mp_digit mp;
   long x, y;

#if defined(BARRELFISH)
   if (argc > 1) {
       srand(strtoul(argv[1], NULL, 10));
   }
#else
   srand(time(NULL));
#endif
   mp_init_multi(&modulus, &R, &p, &pp, NULL);

   /* loop through various sizes */
   for (x = 4; x < 256; x++) {
       printf("DIGITS == %3ld...", x); fflush(stdout);
       
       /* make up the odd modulus */
       mp_rand(&modulus, x);
       modulus.dp[0] |= 1;
       
       /* now find the R value */
       mp_montgomery_calc_normalization(&R, &modulus);
       mp_montgomery_setup(&modulus, &mp);
       
       /* now run through a bunch tests */
       for (y = 0; y < 1000; y++) {
           mp_rand(&p, x/2);        /* p = random */
           mp_mul(&p, &R, &pp);     /* pp = R * p */
           mp_montgomery_reduce(&pp, &modulus, mp);
           
           /* should be equal to p */
           if (mp_cmp(&pp, &p) != MP_EQ) {
              printf("FAILURE!\n");
              exit(-1);
           }
       }
       printf("PASSED\n");
    }
    
    return 0;
}






/* $Source$ */
/* $Revision$ */
/* $Date$ */
