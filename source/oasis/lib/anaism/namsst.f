      SUBROUTINE namsst (pflda, kmska, kvmska, kngxa, kngya,
     $                   prbtoa, kbtoa, kwbtoa,
     $                   pfldb, kngxb, kngyb)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *namsst* -  Interpolation  Anaism method 
C
C     Purpose:
C     -------
C     Performs subgrid averaged interpolation
C
C**   Interface:
C     ---------
C       *CALL*  *namsst(pflda, kmska, kvmska, kngxa, kngya,
C                       prbtoa, kbtoa, kwbtoa,
C                       pfldb, kngxb, kngyb)*
C
C     Input:
C     -----
C                kmska  : mask for target grid (integer 2D)
C                kvmska : the value of the mask for target grid
C                kngxa  : number of longitudes for target grid
C                kngya  : number of latitudes for target grid
C                prbtoa : weights for Anaism interpolation (real 2D)
C                kbtoa  : source grid neighbors adresses (integer 2D)
C                kwbtoa : maximum number of overlapped neighbors
C                pfldb  : field on source grid (real 2D)
C                kngxb  : number of longitudes for source grid
C                kngyb  : number of latitudes for source grid
C
C     Output:
C     ------
C                pflda: field on target grid (real 2D)
C
C     Workspace:
C     ---------
C     None
C
C     External:
C     --------
C     plsst
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
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C     
      REAL pflda(kngxa,kngya), pfldb(kngxb,kngyb)
      REAL prbtoa(kwbtoa,kngxb*kngyb)
      INTEGER kmska(kngxa,kngya), kbtoa(kwbtoa,kngxb*kngyb)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) 
     $    '           ROUTINE namsst  -  Level 3'
          WRITE(UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) 
     $    ' Perform ANAIS-MESH interpolation'
          WRITE(UNIT = nulou,FMT = *) ' '
          WRITE(UNIT = nulou,FMT = *) ' '
      ENDIF
      inga = kngxa *kngya
      ingb = kngxb *kngyb
C
C
C*    2. Call interpolator
C        -----------------
C
      CALL plsst (pflda, prbtoa, kbtoa, kwbtoa, inga, pfldb,
     $            ingb, kmska, kvmska)
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine namsst ---------'
          CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
