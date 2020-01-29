      SUBROUTINE glored (pzgg, pworkgr, klon, klat, ktronca)
C*****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *glored* - Global gaussian grid transformation 
C
C     Purpose:
C     -------
C     Linear interpolation of field from full gaussian grid to reduced grid
C
C     N.B: It is assumed that a preprocessing of the field on the full grid
C          was done to allocate ocean values to land points as no mask is used
C          in this routine. 
C
C**   Interface:
C     ---------
C       *CALL*  *glored (pzgg, pworkgr, klon, klat, ktronca)*
C
C     Input:
C     -----
C                pzgg    : field on global grid (real 2D)
C                pworkgr : array to store field on global grid (real 2D)
C                klon    : number of longitudes of global grid
C                klat    : number of latitudes of both grids
C                ktronca : truncature of gaussian grid
C                   
C     Output:
C     ------
C                pzgg: field on reduced grid (real 2D)
C
C     Workspace:
C     ---------
C     inip
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
C       2.0       L. Terray      95/09/01  modified
C       2.3       L. terray      99/03/01  bug corrected: inip DIMENSION
C       2.3       S. Valcke      99/03/16  modified for T213 and T319
C       2.3       S. Valcke      99/03/26  changed troncature for number of 
C                                          latitude between equator and pole
C       2.3       S. Valcke      99/04/30  added: printing levels
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'gauss.h'
      INCLUDE 'printing.h'
C
C* ---------------------------- Argument declarations -------------------
C

      REAL pzgg(klon,klat), pworkgr(klon*klat)
      INTEGER inip(320)
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
     $    '           ROUTINE glored  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' Go from global to reduced grid'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      CALL izero(inip,320)
C
C
C*    2. Full to reduced linear interpolation  
C        ------------------------------------
C
C
C* get number of longitudes by latitude circle
C
      IF (ktronca .EQ. 16) THEN
          DO 210 ji = 1, ktronca
            inip(ji) = ninip16(ji)
 210      CONTINUE 
        ELSE IF (ktronca .EQ. 24)  THEN 
          DO 220 ji = 1, ktronca
            inip(ji) = ninip24(ji)
 220      CONTINUE
        ELSE IF (ktronca .EQ. 32)  THEN 
          DO 230 ji = 1, ktronca 
            inip(ji) = ninip32(ji)
 230      CONTINUE 
        ELSE IF (ktronca .EQ. 48)  THEN 
          DO 240 ji = 1, ktronca 
            inip(ji) = ninip48(ji)
 240      CONTINUE    
        ELSE IF (ktronca .EQ. 80)  THEN 
          DO 250 ji = 1, ktronca 
            inip(ji) = ninip80(ji)
 250      CONTINUE 
        ELSE IF (ktronca .EQ. 160)  THEN 
          DO 255 ji = 1, ktronca 
            inip(ji) = ninip160(ji)
 255      CONTINUE 
        ELSE
          CALL prtout
     $          ('WARNING!!! Oasis cannot treat this grid with 2*NO 
     $          latitude lines with NO = ', ktronca, 2)
          CALL prtout('Implement data for NO =', ktronca, 2)
          CALL HALTE('STOP in glored')
      ENDIF
C
C* Extend inip array to both hemispheres
C
      DO 260 ji = klat/2 + 1, klat
        inip(ji) = inip(klat - ji + 1)
 260  CONTINUE
C
C* Do the interpolation global to reduced
C
      indice = 0
      DO 270 jk = 1, klat
        DO 280 ji = 1, inip(jk)
          zxi = 1 + ((ji - 1) * klon) / FLOAT(inip(jk))
          im = INT(zxi)
          zdx = zxi - im
          im = 1 + MOD(im + klon - 1,klon)
          ip = 1 + MOD(im,klon)
          pworkgr(indice + ji) = pzgg(im,jk) * (1.-zdx) 
     $                      + pzgg(ip,jk) * zdx
 280    CONTINUE
        indice = indice + inip(jk)
 270  CONTINUE
C
C* Assign reduced grid values to array pzgg
C
      DO 290 jj = 1, klat
        DO 295 ji = 1, klon
         pzgg(ji,jj) = pworkgr(klon*(jj-1)+ji) 
 295   CONTINUE 
 290  CONTINUE
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine glored ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
 
