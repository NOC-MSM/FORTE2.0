      SUBROUTINE inilun
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL 0 *
C               * -------------     ------- *
C               *****************************
C
C**** *inilun*  - Initialize logical unit numbers
C
C     Purpose:
C     -------
C     Creates and prints logical unit numbers used to deal with
C     grids, masks and surfaces files as well as anais-related files
C
C**   Interface:
C     ---------
C       *CALL*  *inilun*
C
C     Input:
C     -----
C     None
C
C     Output:
C     ------
C     None
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
C       2.0       L. Terray      95/08/23  modified: new structure
C       2.2       L. Terray      97/10/10  added: unit nudum for SVIPC
C       2.3       S. Valcke      99/03/30  added: unit nulgn for NINENN
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
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Assign unit numbers and initialize comlun
C        -----------------------------------------
C
C* First we open output file for coupler
C
      IF (nlogprt .GE. 1) THEN
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '           ROUTINE inilun  -  Level 0'
          WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    ' Set up logical unit numbers'
          WRITE (UNIT = nulou,FMT = *) ' '
      ENDIF
C* Grids file
      nulgr = 11
C* Masks file
      nulma = 12
C* Surfaces file
      nulsu = 13
C* File for reduced grid masks
      nulrd = 14
C* Trace file for CLIM and PVM
      nultr = 7
C* Output file for ANAIS interpolation
      nulan = 8
C* Dummy file for SVIPC library
      nudum = 9
C* Anaism weights file
      nulcc = 16
C* Anaisg weights file
      nulgg = 17
C* NINENN weight and address file
      nulgn = 18
C
C
C*     2. Print comlun
C         ------------
C
      IF (nlogprt .GE. 1) THEN 
          WRITE (UNIT = nulou,FMT ='(
     $        '' nulin ='',i3,'' nulou ='',i3,
     $        '' nulgr ='',i3,'' nulma ='',i3,
     $        '' nulsu ='',i3,'' nultr ='',i3,   
     $        '' nulcc ='',i3,'' nulgg ='',i3,'' nulgn ='',i3,
     $        '' nulan ='',i3,'' nulrd ='',i3,'' nudum ='',i3,/)')
     $        nulin, nulou, nulgr, nulma, nulsu, 
     $        nultr, nulcc, nulgg, nulgn, nulan, nulrd, nudum
      ENDIF 
C
C
C*     3. End of routine
C         --------------
C
      IF (nlogprt .GE. 1) THEN 
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $    '          --------- End of routine inilun ---------'
c         CALL FLUSH (nulou)
      ENDIF 
      RETURN
      END






