C
C -- printing.h   30-04-99   Version 2.3   Author: Sophie Valcke
C    **********  
C@
C@  Contents : variables related to printing level in cplout
C@  --------
C@
C@ -- nlogprt : printing level in output file cplout: 0 = no printing
C@      1 = main routines and field names when treated, 2 = complete output
C@
C     -------------------------------------------------------------------
C
      INTEGER nlogprt
C
      COMMON / comprt / nlogprt
C
C     -------------------------------------------------------------------




