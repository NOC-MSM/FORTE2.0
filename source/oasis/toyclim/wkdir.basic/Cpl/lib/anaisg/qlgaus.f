      FUNCTION qlgaus (pu, pvar)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL 3 *
C               * --------------     ------- *
C               ******************************
C
C**** *qlgaus* - Gaussian weight function
C
C
C     Purpose: 
C     -------
C     To calculate exp [-.5 * u / pvar]
C
C
C**   Interface:
C     ---------
C       *zz=*   *qlgaus(pu, pvar)*
C
C     Input:
C     -----   
C                pu   : the distance squared
C                pvar :  the gaussian variance
C
C     Output:
C     ------
C     None
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     None
C
C     References:
C     ----------
C     O. Thual, Simple ocean-atmosphere interpolation. 
C               Part A: The method, EPICOA 0629 (1992)
C               Part B: Software implementation, EPICOA 0630 (1992)
C     See also OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      ----------- 
C       1.1       O. Thual       93/04/15  created 
C       2.0       L. Terray      95/10/01  modified: new structure
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Calculation
C        -----------
C
      qlgaus = exp(-.5*pu/pvar)
C
C* End of FUNCTION
C
      RETURN 
      END
