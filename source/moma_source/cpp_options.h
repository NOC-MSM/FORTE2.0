#ifdef OLD_NON_CONSERVING
# undef NONLIN_FREE_SURFACE
#endif

#ifdef REAL_8
# define _MOMA_REAL real*8
#else
# define _MOMA_REAL real*4
#endif

#ifdef msq
# ifndef halo2
# define halo2
# endif
#endif
