      FUNCTION rminim (pa, kmsk, kna, kind, kflag)
C****
C               ******************************
C               * OASIS FUNCTION  -  LEVEL T *
C               * --------------     ------- *
C               ******************************
C
C**** *rminim*  - Search function
C
C     Purpose:
C     -------
C     Search the minimum of the elements of a real array subject
C     or not to a mask condition
C
C**   Interface:
C     ---------
C       *zz =*  *rminim (pa, kmsk, kna, kind, kflag)*
C
C     Input:
C     -----
C                pa     : array to be searched (real 1D)
C                kmsk   : mask array (integer 1D)
C                kna    : array dimension (integer)

C                kflag  : type of search (integer)
C                         0 --> Global  1 --> Land  2 --> Sea
C
C     Output:
C     ------
C                kind   : point index of minimum (integer)
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
C       2.1       L. Terray      96/09/25  created
C       2.2       L. Terray      97/02/12  modified: printing bug on 
C                                          variable kind corrected
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
C
C* ---------------------------- Argument declarations -------------------
C
      REAL rminim, pa(kna)
      INTEGER kmsk(kna)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Find the maximum 
C        ----------------
C
      itemp = 0
      ztemp = 0.0
      IF (kna .LT. 1) GO TO 110
 130  itemp = itemp + 1
      IF (itemp .GT. kna) THEN
          CALL prtout
     $        ('WARNING!!! initial search exceeds array size 
     $        kna  = ', kna, 2)
          GO TO 140
      ENDIF 
      IF (kflag .EQ. 0) THEN
          ztemp = pa(itemp)
          GO TO 140
        ELSE IF (kflag .EQ. 1) THEN
          IF (kmsk(itemp) .EQ. 1) THEN 
              ztemp = pa(itemp)
              GO TO 140
          ENDIF 
        ELSE IF (kflag .EQ. 2) THEN
          IF (kmsk(itemp) .EQ. 0) THEN 
              ztemp = pa(itemp)
              GO TO 140
          ENDIF
      ENDIF 
      GO TO 130
 140  CONTINUE 
C* Assign index to initial point
      kind = itemp
      IF (kna .LT. 2) GO TO 110
C* Start looping on all other points
      IF (kflag .EQ. 1) THEN
	DO 120 ja = itemp+1, kna 
            IF (pa(ja) .LT. ztemp .AND. kmsk(ja) .EQ. 1) THEN
                ztemp = pa(ja)
                kind = ja
            ENDIF
 120    CONTINUE 
      ELSE IF (kflag .EQ. 2) THEN 
	DO 125 ja = itemp+1, kna
            IF (pa(ja) .LT. ztemp .AND. kmsk(ja) .EQ. 0) THEN
                ztemp = pa(ja)
                kind = ja
            ENDIF 
 125    CONTINUE
      ELSE IF (kflag .EQ. 0) THEN 
	DO 127 ja = itemp+1, kna
            IF (pa(ja) .LT. ztemp) THEN
                ztemp = pa(ja)
                kind = ja
            ENDIF
 127    CONTINUE
      ENDIF 
 110  CONTINUE
      rminim = ztemp
C
C
C*    2. End of routine
C        --------------
C
      RETURN
      END
