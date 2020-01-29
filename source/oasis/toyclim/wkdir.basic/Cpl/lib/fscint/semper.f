      MODULE memoir
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 3 *
C               * -------------     ------- *
C               *****************************
C
C**** *memoir*  - F90 interface for dynamic allocation in FSCINT
C
C     Purpose:
C     -------
C     Handle dynamic allocation in FSCINT
C
C**   Interface:
C     ---------
C       *CALL*  *memoir(r-i)(pw,koff,ksize,koldsize)
C
C     Input:
C     -----
C                pw       : array to be allocated
C                ksize    : new size to be allocated 
C                koldsize : old size of allocation
C
C     Output:
C     ------
C                koff     : status flag
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
C     This F90 module handles dynamic allocation within FSCINT.
C     It deals with both INTEGER and REAL memory allocation.
C     It has been tested on a variety of platforms (SGI, VPP, T3E, C90)
C     and is truly portable. The compilation step may vary across
C     platforms (see OASIS documentation 2.2). The module has been
C     written with fixed format and should then be named with suffix .f
C
C     History:
C     -------
C       Version   Programmer     Date      Description
C       -------   ----------     ----      -----------  
C       2.2       A. Piacentini  97/09/15  Created
C       2.3       A. Piacentini  98/10/01  Modified: Bug corrected in case
C                                          of reallocation
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C* ------------------------------------------------------------------
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* Block interface
C
          INTERFACE memoirh
             MODULE PROCEDURE memoirr,memoiri
          END INTERFACE
C
      CONTAINS
C
C* Real allocation
C
      SUBROUTINE memoirr(pw,koff,ksize,koldsize)
C
      REAL, DIMENSION(:), POINTER  :: pw
      INTEGER                      :: koff
      INTEGER                      :: ksize
      INTEGER                      :: koldsize
C
      REAL, DIMENSION(:), POINTER  :: aw
      INTEGER                      :: ierr
C
C      PRINT *,'memoirR ksize ',ksize
C
      IF (ksize > 0) THEN
          IF (koldsize > 0) THEN
              allocate(aw(ksize),stat=ierr)
C              PRINT *,'ierr= ',ierr
              IF(.NOT. associated(aw)) PRINT *,'memoirR Something Wrong'
              koff=1
              aw(1:koldsize)=pw
              deallocate(pw,stat=ierr)
C              PRINT *,'memoirR dealloc of old pw ierr= ',ierr
              pw=>aw
          ELSE   
              allocate(pw(ksize),stat=ierr)
C              PRINT *,'ierr= ',ierr
              IF(.NOT. associated(pw)) PRINT *,'memoirR Something Wrong'
C
C             PRINT *,'memoirR allocated'
C
              koff=1
          ENDIF 
      ELSE
          IF(associated(pw)) THEN
              deallocate(pw,stat=ierr)
C              PRINT *,'memoirR dealloc ierr= ',ierr
          ELSE
              STOP 'error in memoirR deallocation'
          END IF
C
C          PRINT *,'memoirR deallocated'
C
      ENDIF
C
      END SUBROUTINE memoirr
C
C* Integer allocation 
C
      SUBROUTINE memoiri(kw,koff,ksize,koldsize)
C
      INTEGER , DIMENSION(:), POINTER  :: kw
      INTEGER                      :: koff
      INTEGER                      :: ksize
      INTEGER                      :: koldsize
C
      INTEGER , DIMENSION(:), POINTER  :: iw
      INTEGER                      :: ierr
C
C      PRINT *,'memoirI ksize ',ksize
C
      IF (ksize > 0) THEN
          IF (koldsize > 0) THEN
              allocate(iw(ksize),stat=ierr)
C              PRINT *,'ierr= ',ierr
              IF(.NOT. associated(iw)) PRINT *,'memoirI Something Wrong'
              koff=1
              iw(1:koldsize)=kw
              deallocate(kw,stat=ierr)
C              PRINT *,'memoirI dealloc of old kw ierr= ',ierr
              kw=>iw
          ELSE   
              allocate(kw(ksize),stat=ierr)
C              PRINT *,'ierr= ',ierr
              IF(.NOT. associated(kw)) PRINT *,'memoirI Something Wrong'
C
C             PRINT *,'memoirI allocated'
C
              koff=1
          ENDIF 
      ELSE
          IF(associated(kw)) THEN
              deallocate(kw,stat=ierr)
C              PRINT *,'memoirI dealloc ierr= ',ierr
          ELSE
              STOP 'error in memoirI deallocation'
          END IF
C
C          PRINT *,'memoirI deallocated'
C
      ENDIF
C
      END SUBROUTINE memoiri
C
      END MODULE memoir




