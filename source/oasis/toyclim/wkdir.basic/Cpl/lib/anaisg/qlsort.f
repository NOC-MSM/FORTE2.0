      SUBROUTINE qlsort (prho, kto, kwg)
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *qlsort* - Sort kwg numbers with adresses
C
C     Purpose:
C     -------
C     Given kwg real in prho, this routine sort them and
C     rearrange the array kto in the same way.
C
C     N.B: The method is a trivial one. the first element is assumed 
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
C       *CALL*  *qlsort (prho, kto, kwg)*
C
C     Input:
C     -----
C                prho  : array to be sorted
C                kto   : array to be re-arranged as prho
C                kwg   : size of prho and kto
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
C*    1. Sorting algorithm
C        -----------------
C     
C* If kwg is equal to one no sorting is needed
C
      IF (kwg .EQ. 1) RETURN
C
C* Loop on all the positions
C
      ikwgm = kwg - 1
      DO 110 jwg = 1, ikwgm
        ijwgp = jwg + 1
C
C* Loop on the element following the position
C
        DO 120 jnext = ijwgp, kwg 
          zsmal = prho(jwg)
          zbig = prho(jnext)
C
C* Testing the smallness assumption
C
          IF (zsmal .GT. zbig) THEN
C
C* Permutation of prho
C 
              prho(jnext) = zsmal
              prho(jwg) = zbig
C
C* Permutation of kto
C
              ikbig = kto(jnext)
              kto(jnext) = kto(jwg)
              kto(jwg) = ikbig
          ENDIF
 120    CONTINUE
 110  CONTINUE
C
C* End of routine
C
      RETURN 
      END
