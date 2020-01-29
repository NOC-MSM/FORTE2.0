      SUBROUTINE conserv (pfldb, ksizb, kmskb, psurfb,
     $                    pflda, ksiza, kmska, psurfa, 
     $                    psgrb, psgra, cdmet)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *conserv* - Flux conservation routine
C
C     Purpose:
C     -------
C     Use global lagrange multiplier to insure conservation
C
C**   Interface:
C     ---------
C       *CALL*  *conserv (pfldb, ksizb, kmskb, psurfb,
C                         pflda, ksiza, kmska, psurfa, cdmet)*
C
C     Input:
C     -----
C                pflda  : field on target grid (real 1D)
C                pfldb  : field on source grid (real 1D)
C                kmska  : mask for target grid (integer 1D)
C                kmskb  : mask for source grid (integer 1D)
C                psurfa : surfaces for target grid meshes (real 1D)
C                psurfb : surfaces for source grid meshes (real 1D)
C                ksizb  : source arrays size (integer)
C                ksiza  : target arrays size (integer)
C                psgrb  : work array (real 1D)
C                psgra  : work array (real 1D)
C                cdmet  : conservation method (character string)
C
C     Output:
C     ------
C                pflda  : field on target grid with conservation (real 1D)
C
C     Workspace:
C     ---------
C     None
C
C     Externals:
C     ---------
C     None
C
C     Reference:
C     ---------
C     See OASIS manual (1995)
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       1.0       L. Terray      94/01/01  created
C       2.0       L. Terray      95/10/01  modified: new structure
C       2.1       L. Terray      96/09/01  modified: printing
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
      REAL pflda(ksiza), pfldb(ksizb)
      REAL psurfa(ksiza), psurfb(ksizb)
      REAL psgra(ksiza), psgrb(ksizb)
      INTEGER kmska(ksiza), kmskb(ksizb)
      CHARACTER*8 cdmet
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE conserv  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Global or local flux conservation'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      zrad = 6371229.
      zrad2 = zrad * zrad
      zradi = 1./zrad2

C
C
C*    2. Calculate mesh surfaces
C        -----------------------
C
      DO 210 ji = 1, ksiza
        psgra(ji) = psurfa(ji) * zradi
 210  CONTINUE
      DO 220 ji = 1, ksizb
        psgrb(ji) = psurfb(ji) * zradi
 220  CONTINUE
C
C
C*    3. Lagrange multiplier for global conservation
C        -------------------------------------------
C
      IF (cdmet .EQ. 'GLOBAL') THEN
          zflxa = 0.
          ztsqi = 0.
C
C* Sum up flux on target grid
C
          DO 310 ji = 1, ksiza
            IF (kmska(ji) .eq. 0) THEN
                zflxa = zflxa + psgra(ji) * pflda(ji)
                ztsqi = ztsqi + psgra(ji) * psgra(ji)
            ENDIF
 310      CONTINUE
C
C* Sum up flux on source grid
C
          zflxb = 0.
          DO 320 ji = 1, ksizb
            IF (kmskb(ji) .eq. 0) THEN
                zflxb = zflxb + psgrb(ji) * pfldb(ji)
            ENDIF
 320      CONTINUE
C
C* Get global correction
C
          zlagr = (zflxa - zflxb) / ztsqi
C
C* Constrained solution: the error is uniformly shared between sea points
C
          DO 330 ji = 1, ksiza
            IF (kmska(ji) .EQ. 0) THEN
                pflda(ji) = pflda(ji) - zlagr * psgra(ji)
            ENDIF
 330      CONTINUE
C
C* Printing test
C
          zflxn = 0.
          DO 340 ji = 1, ksiza
            IF (kmska(ji) .eq. 0) THEN
                zflxn = zflxn + psgra(ji) * pflda(ji)
            ENDIF
 340      CONTINUE
          IF (nlogprt .GE. 2) THEN
              WRITE (UNIT = nulou,FMT = *) 
     $        ' Printing check for flux conservation '
              WRITE (UNIT = nulou,FMT = *) ' '
              WRITE (UNIT = nulou,FMT = *) 
     $        ' Total flux on source grid ZFLXB = ',zflxb
              WRITE (UNIT = nulou,FMT = *) 
     $        ' Total flux on target grid ZFLXA = ',zflxa
              WRITE (UNIT = nulou,FMT = *) 
     $        ' Idem after conservation   ZFLXN = ',zflxn
              WRITE (UNIT = nulou,FMT = *) ' '
          ENDIF
      ENDIF 
C
C
C*    4. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine conserv ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
