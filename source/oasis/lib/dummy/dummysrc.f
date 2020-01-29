CCC
CCC OASIS 2.2: These routines are necessary for the linking stage
CCC            of the compilation of OASIS 2.2 on VPP machines
CCC
      INTEGER FUNCTION WAIT(k)
C
      INTEGER k
C
      wait=k
C
      RETURN
      END
CCC
CCC
CCC
      SUBROUTINE FSIGCTL (cd1,cd2,k)
C
      CHARACTER*6 cd1,cd2
      INTEGER k
C
      cd1=cd2
      k=1
C
      RETURN
      END
CCC
CCC
CCC
      REAL FUNCTION cvmgp (pfild,pmask,plast)
C
      REAL pfild,pmask,plast
C
      pfild=pmask
      cvmgp=plast
C
      RETURN
      END
