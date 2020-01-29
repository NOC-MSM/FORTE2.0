      SUBROUTINE blasnew (pfild ,kmsize, kfield, pmcoeff, kaux, kaddr,
     $                    kasize, pacoeff, ksiztot, paux)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C
C     Purpose:
C     -------
C     Do linear combination of fields with given coefficients
C     after interpolation
C
C**   Interface:
C     ---------
C       *CALL*  *blasnew (pfild ,kmsize, kfield, pmcoeff, kaux, kaddr,
C                         kasize, pacoeff, ksiztot, paux)*
C
C     Input:
C     -----
C                pfild   : field array to be modified (real 1D)
C                kmsize  : size of the field array (integer)
C                kfield  : field identificator number (integer)
C                pmcoeff : field multiplicative coefficient (real)
C                kaux    : number of combined auxilary fields (integer)
C                kaddr   : pointer for auxilary fields (integer 1D)
C                kasize  : size of auxilary fields (integer 1D)
C                pacoeff : auxilary field multiplicative coefficient (real 1D)
C                ksiztot : total size of work memory used (integer)
C                paux    : global work array (real 1D)
C                
C     Output:
C     ------
C                pfild   : new field array (real 1D)
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
C       2.0       L. Terray      95/10/01  created
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
      REAL pfild(kmsize)
      REAL paux(ksiztot), pacoeff(kaux)
      INTEGER kaddr(kaux), kasize(kaux)
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER istop
      LOGICAL llchk
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
     $    '           ROUTINE blasnew  -  Level 3'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ***************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Do linear combination of fields'
          WRITE (UNIT = nulou,FMT = *) ' after interpolation '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
      istop = 0
C
C* Check all auxilary fields have same dimensions as current field
C
      DO 110 jc = 1, kaux
        llchk = (kasize(jc) - kmsize) .EQ. 0
        IF (.NOT. llchk) THEN
            CALL prtout ('WARNING!!! 
     $          Pb in combining field number =', kfield, 2)
            CALL prtout ('Different size for auxilary field number =',
     $          jc, 2)
            istop = istop + 1
        ENDIF 
 110  CONTINUE 
      IF (istop .GT. 0) CALL HALTE('STOP in blasnew')
C
C
C*    2. Linear combination
C        ------------------
C
C* Multiply current field by main coefficient
C
      DO 210 ja = 1, kmsize
        pfild(ja) = pmcoeff * pfild(ja)
 210  CONTINUE
C
C* Combine with other fields if required
C
      IF (kaux .GE. 1) THEN 
          DO 220 jc = 1, kaux
            DO 230 ja = 1, kmsize
              pfild(ja) = pfild(ja) + pacoeff(jc) *
     $                    paux(kaddr(jc)+ja-1)
 230        CONTINUE
 220      CONTINUE
      ENDIF 
C
C
C*    3. End of routine
C        --------------
C
      IF (nlogprt .GE. 2) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine blasnew ---------'
c         CALL FLUSH (nulou)
      ENDIF
      RETURN
      END
