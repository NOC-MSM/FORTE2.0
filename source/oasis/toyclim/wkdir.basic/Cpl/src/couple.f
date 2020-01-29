      PROGRAM couple
C=====================================================================C
C                                                                     C
C                 ####     ##     ####     #     ####                 C
C                #    #   #  #   #         #    #                     C
C                #    #  #    #   ####     #     ####                 C
C                #    #  ######       #    #         #                C
C                #    #  #    #  #    #    #    #    #                C
C                 ####   #    #   ####     #     ####                 C
C                                                                     C
C=====================================================================C
C                                                                     C
C            Ocean Atmosphere Sea Ice Soil Simulation Software        C 
C                          -- VERSION 2.3 --                          C
C                                                                     C
C    CONTRIBUTORS:  L.TERRAY, S. VALCKE, A. PIACENTINI,               C
C                   E.SEVAULT, S. SAARINEN, O. THUAL                  C
C                   E.GUILYARDI, P.NOYRET, Y. CHARTIER                C
C                   H. RITZDORF                                       C
C                                                                     C
C              CLIMATE MODELLING AND GLOBAL CHANGE TEAM               C
C                            C.E.R.F.A.C.S                            C
C                      42, Ave. Gustave Coriolis                      C
C                        31057  TOULOUSE CEDEX                        C
C                              FRANCE                                 C
C                                                                     C
C=====================================================================C
C                                                                     C
C CONTACT LAURENT TERRAY, SOPHIE VALCKE OR ANDREA PIACENTINI          C
C             Email: oasishelp@cerfacs.fr Tel:(33)5.61.19.30.15       C
C                                                                     C
C   WITH QUESTIONS OR COMMENTS CONCERNING THIS SOFTWARE               C
C                                                                     C
C=====================================================================C
C   V1.0 (2/94) IS THE ORIGINAL VERSION OF THIS SOFTWARE              C
C   ***********                                                       C
C                                                                     C
C The OASIS software allows coupling of General Circulation Models of C
C the Atmosphere and the Ocean (AGCM and OGCM).                       C
C                                                                     C
C---------------------------------------------------------------------C
C   V1.1 (8/94) includes the following changes:                       C
C   ***********                                                       C
C                                                                     C
C - Bug corrected for interpolation in fscint package near poles:     C
C   different treatment for scalar and vector.                        C
C   P. Braconnot, O. Marti, L. Terray                                 C
C                                                                     C
C - Extension of Z grids incorporated in fscint package.              C
C   P. Braconnot, O. Marti, L. Terray                                 C
C                                                                     C
C - Bug corrected in prtfld routine in case nmode = 1,2 due to non    C
C   initialized variables not taken care of by namelist options.      C
C   L. Fairhead, L. Terray                                            C
C                                                                     C
C - Bug corrected in routines naflux, qlflux (argument list mismatch) C
C   L. Terray                                                         C
C                                                                     C
C - Addition of fluxes to force a sea-ice model.                      C
C   L. Terray, G. Garric                                              C
C                                                                     C
C - Sea-ice extent can be prescribed from climatology or read from    C
C   ogcm + sea-ice model.                                             C
C   L. Terray                                                         C
C                                                                     C
C - Adding of the no-interpolation case.                              C
C   L. Terray                                                         C
C                                                                     C
C - Global flux conservation implemented.                             C
C   L. Terray                                                         C
C---------------------------------------------------------------------C
C   V2.0 (1/96) is a major enhancement of the software.               C
C   ***********                                                       C
C                                                                     C
C New main features are as follows:                                   C
C                                                                     C
C - Distributed computing allowed (Use of CLIM library based on PVM). C
C                                                                     C
C - Dynamic definition of coupling fields.                            C
C                                                                     C
C - Choice of the time coupling strategy.                             C
C                                                                     C
C - Binary format to transfer coupling fields.                        C
C                                                                     C
C - Workstation version.                                              C
C                                                                     C
C - And much more... (See manual for details).                        C
C                                                                     C
C---------------------------------------------------------------------C
C   V2.2 (1/98) has some new interesting features:                    C
C                                                                     C
C - a new communication technique based on Sytem V shared memory      C
C   segments developed by S.Saarinen (ECMWF)                          C
C                                                                     C
C - new extrapolation method                                          C
C                                                                     C
C - mode "interpolator only"                                          C
C                                                                     C
C - possibility to have an extended header for the coupling fields    C
C                                                                     C
C - new memory allocation in fscint package (with F90 features)       C
C                                                                     C
C - atmosphere and ocean toys provided with full environment for the  C
C   pipe and svipc techniques                                         C
C                                                                     C
C The code has been successfully run on various platforms:            C
C Crays (C90, J90, T3E), VPPs (300, 700), SGIs (Origin 200, 2000) ... C
C The portability on NEC machines is currently being investigated     C
C                                                                     C
C---------------------------------------------------------------------C
C   V2.3 (10/99) has some new features:                               C
C                                                                     C
C - a new communication technique for NEC machines which is based on  C
C   the global memory concept                                         C
C                                                                     C
C - optimization of the extrapolation package                         C
C                                                                     C
C - introduction of several printing levels for oasis output file     C
C                                                                     C
C - new definition of gaussian grids                                  C
C                                                                     C
C - new definition of grid periodicity                                C
C                                                                     C
C - bugs correction: fscint, fiasco, leap years etc ...               C
C                                                                     C
C---------------------------------------------------------------------C
C                                                                     C
C Reference for this software is the OASIS User Guide written by      C
C Laurent Terray, Sophie Valcke and Andrea Piacentini                 C
C under the CERFACS Technical Report No TR/CMGC/99-37                 C
C                                                                     C
C=====================================================================C
C                                                                     C
C                ####     ##     ####      #     ####                 C
C               #    #   #  #   #          #    #                     C
C               #    #  #    #   ####      #     ####                 C
C               #    #  ######       #     #         #                C
C               #    #  #    #  #    #     #    #    #                C
C                ####   #    #   ####      #     ####                 C
C                                                                     C
C=====================================================================C
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
C*    1. Open main output file for OASIS coupler.
C        ----------------------------------------
C
C* First we open output file connected to unit nulou = 6
C
      nulou = 10
      iost = 0
      OPEN (UNIT = nulou,FILE ='cplout',STATUS='NEW',
     $      FORM ='FORMATTED',ERR = 110,IOSTAT = iost)
C
C* Let 's the fun begin
C
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                ANOTHER FANTASTIC RUN OF THE SUPER '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                  Ocean Atmosphere Sea Ice Soil '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                  ----------------------------- '
      WRITE (UNIT = nulou,FMT = *) 
     $    '               A fractal, random and poetic software '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                for surrealistic simulations of the '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                           EARTH CLIMATE '
      WRITE (UNIT = nulou,FMT = *) 
     $    '                           ************* '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $    '           PROGRAM couple  -  Level 0'
      WRITE (UNIT = nulou,FMT = *) 
     $    '           **************     *******'
      WRITE (UNIT = nulou,FMT = *) ' '
      WRITE (UNIT = nulou,FMT = *) 
     $       ' Open main input-output files and launch the simulation'
      WRITE (UNIT = nulou,FMT = *) ' '
      CALL prtout 
     $    ('Open output file cplout connected to unit',nulou,1)
 110  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>> : ERROR opening output FILE'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ======   =====                ===='
          WRITE (UNIT = nulou,FMT = *) 
     $           ' Logical unit ',nulou,' error number = ',iost
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP!!! Verify the file cplout'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in couple')
      ENDIF
C
C
C*    2. Open main input file for OASIS coupler.
C        ----------------------------------------
C
C* Open input file connected to unit nulin=4
C
      nulin = 4
      iost = 0
      OPEN (UNIT = nulin,FILE ='namcouple',STATUS='OLD',
     $      FORM ='FORMATTED',ERR = 210,IOSTAT = iost)
      CALL prtout 
     $    ('Open input file namcouple connected to unit ',nulin,1)
 210  CONTINUE
      IF (iost .ne. 0) THEN
          WRITE (UNIT = nulou,FMT = *) '        ***WARNING***' 
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ===>>> : ERROR opening input  FILE'
          WRITE (UNIT = nulou,FMT = *) 
     $        ' ======   =====                ===='
          WRITE (UNIT = nulou,FMT = *) 
     $        ' Logical unit ',nulin,' error number = ',iost
          WRITE (UNIT = nulou,FMT = *) ' '
          WRITE (UNIT = nulou,FMT = *) 
     $        ' We STOP!!! Verify the file namcouple'
          WRITE (UNIT = nulou,FMT = *) ' '
          CALL HALTE ('STOP in couple')
      ENDIF
C
C
C*    3. Launch the run by calling the simulation driver
C        -----------------------------------------------
C
      CALL driver
C
C
C*    4. End of the fun
C        --------------
C
      STOP 'END OF OASIS SIMULATION'
      END
