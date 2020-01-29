      SUBROUTINE qlins (prho, kto, kwg, prnew, kjwg)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *qlins* - Insert a new element in a sorted list
C
C     Purpose:
C     -------
C     Given a sorted array prho and associated array kto of indices
C     of length klwg, insert a new element prnew in prho, if needed,
C     and  kjwg in kto accordingly, and trash the lowest element.
C
C     N.B: The method is a trivial one. The first element is assumed 
C     to be the smallest and then tested against the following 
C     one. If an element is smallest a permutation is made, 
C     and the testing goes on with the next elements. There
C     is no need to test again the  elements previously tested
C     as they are known to be greater. At the end of the loop
C     the smallest is in first position, and we repeat the 
C     the procedure with the array starting in position two
C     and so on.
C
C**   Interface:
C     ---------
C       *CALL*  *qlins (prho, kto, kwg, prnew, kjwg)*
C
C     Input:
C     -----
C                prho  : array to be sorted
C                kto   : array to be re-arranged as prho
C                kwg   : size of prho and kto
C                prnew : the new vallue to insert in prho, if needed
C                kjwg  : to be inserted in kto accordingly
C 
C     Output:
C     ------
C                prho  : the sorted array
C                kto   : the re-arranged array
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
C* ---------------------------- Argument declarations -------------------
C
      REAL prho(kwg)
      INTEGER kto(kwg)
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 
C*    1. Insertion
C        ---------
C     
C* If kwg is equal to one: nothing special 
C
C* Loop on all the positions
C
      DO 110 jwg = 1, kwg
        zsmal = prho(jwg)
C          
C* Testing if insertion is needed 
C
        IF (zsmal .GT. prnew) THEN
C             
C* Shift to insert new value excepted the last element
C
            IF (jwg .NE. kwg) THEN
                ijwgp = jwg + 1
                DO 120 ji = kwg, ijwgp, -1
                  prho(ji) = prho(ji-1)
                  kto(ji) = kto(ji-1)
 120            CONTINUE
            ENDIF
            prho(jwg) = prnew
            kto(jwg) = kjwg
            RETURN
        ENDIF
 110  CONTINUE
C
C* End of routine
C
      RETURN 
      END
