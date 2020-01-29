      SUBROUTINE driver
C****
C               *****************************
C               * OASIS ROUTINE  -  LEVEL C *
C               * -------------     ------- *
C               *****************************
C
C**** *driver* - Main OASIS routine
C
C
C     Purpose:
C     -------
C     Drive and control the simulation between GCMs and coupler.
C     Contain the time loop. A coupled simulation with OASIS 2.0
C     starts with the interpolation of the boundary conditions
C     from their original grid to the target grid in contrast with
C     the previous versions. Consequently, the GCMs pause initially
C     until the coupling variables have been interpolated.
C
C     N.B: Note that the time loop goes from 0 to niter-1 in contrast
C          with previous versions. The iteration 0 of oasis DOES NOT
C          increment the simulation time.
C
C**   Interface:
C     ---------
C       *CALL*  *driver*
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
C               iindex : index array for field identificators
C
C     Externals:
C     ---------
C                        Initialization
C                        **************
C     inipar, inilun, iniiof, inidya, initim, inicmc, chkpar, inigrd,
C
C                        Temporal loop
C                        ************* 
C     getfld, preproc, interp, cookart, postpro, givfld, reset, updtim
C
C                        Synchronization
C                        ***************
C     modsgc, waitpc
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
C       2.0beta   L. Terray      95/07/24  modified: new structure
C       2.0       L. Terray      96/02/01  modified: change in time loop
C       2.1       O. Marti, L.T  96/09/25  added: extra time step
C       2.2       S. Valcke, L.T 97/11/13  added: SIPC call to modsgc
C                                                 mode no message passing
C       2.3       L. Terray      99/09/15  added: GMEM branch
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C* ---------------------------- Include files ---------------------------
C
      INCLUDE 'doctor.h'
      INCLUDE 'unit.h'
      INCLUDE 'parameter.h'
      INCLUDE 'timestep.h'
      INCLUDE 'experiment.h'
      INCLUDE 'field.h'
      INCLUDE 'string.h'
      INCLUDE 'analysis.h'
      INCLUDE 'hardware.h'
      INCLUDE 'memory.h'
C
C* ---------------------------- Local declarations ----------------------
C
      INTEGER iindex(jpfield)
      LOGICAL lltime, llstrt, llseqn, llend
c      new bit for adding locwrite statements - by bs 12/6/2000
      CHARACTER*8 clname, clfic, clstat
c      end of new bit
C
C* ---------------------------- Poema verses ----------------------------
C
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C*    1. Initialization
C        --------------
C
C* - Initialize run parameters
C
      CALL inipar
      WRITE (UNIT = nulou,FMT = *) ' finished inipar '
      call flush(nulou)
C
C* - Initialize logical unit numbers
C
      CALL inilun
      WRITE (UNIT = nulou,FMT = *) ' finished inilun '
C
C* - Open necessary files
C
      CALL iniiof
      WRITE (UNIT = nulou,FMT = *) ' finished iniiof '
C
C* - Set up dynamic allocation for all grid-related fields
C
      CALL inidya
      WRITE (UNIT = nulou,FMT = *) ' finished iniiof '
C
C* - Initialize time information
C
      CALL initim
      WRITE (UNIT = nulou,FMT = *) ' finished iniiof '
C
C* - Initialize communication between processes
C
      CALL inicmc
      WRITE (UNIT = nulou,FMT = *) ' finished inicmc '
C
C* - Check run parameters compatibility between GCM's and coupler
C
      CALL chkpar
      WRITE (UNIT = nulou,FMT = *) ' finished chkpar '
C
C* - Initialize GCM's grids
C
      CALL inigrd
      WRITE (UNIT = nulou,FMT = *) ' finished inigrd '
C
C*    2. Time loop
C        ---------
C
C* Check if extra time step is needed
C
      imore = imaxim (nfend, nfield)
C
C* Loop on number of iterations
C
      DO 210 jt = 0, nitfn + imore
C
C* Assign local variable for iteration number
C
        iter = jt
C
C* Get time counter. 
C
        icount = iter * nstep
C
C* Update calendar date
C
        CALL updtim (iter)
C
C* Loop on number of sequential models
C
        DO 220 jm = 1, nmseq
C
C* Loop on number of fields to find active fields for current iteration
C
          ifield = 0
          DO 230 jf = 1, nfield
C
C* Get conditional logical flags for doing analysis set
C
            istart = nfinit(jf) * nfexch(jf)
            iend   = ntime + nfend(jf) * nfexch(jf)
            ifnow = nfexch(jf)
            llseqn = nseqn(jf) .EQ. jm
            lltime = mod(icount,ifnow) .EQ. 0
            llstrt = icount .GE. istart
            llend  = icount .LT. iend
C
C* Conditional test to fill up iindex array
C
            IF (llseqn .AND. lltime .AND. llstrt .AND. llend) THEN
                ifield = ifield + 1
C
C* Fill up iindex array with active fields at iteration jt
C
                iindex(ifield) = jf
            ENDIF
 230      CONTINUE
C
C* There are ifield fields to be exchanged for iteration jt
C
          IF (ifield .GT. 0) THEN
C
C* Get fields
C
              CALL getfld (iindex, ifield, iter)

C
C* Do preprocessing
C
              CALL preproc (iindex, ifield)
C
C* Do the interpolation
C
              CALL interp (iindex, ifield)
C
C* Do the nitty gritty stuff
C
              CALL cookart (iindex, ifield)

c      new bit for adding locwrite statements - by bs 12/6/2000
      do kbabs=1,8
        iloc = iindex(kbabs)
        iadrold = nadrold(iloc)
        isizold = nsizold(iloc)
        clname = cnaminp(iloc)
      WRITE (UNIT = nulou,FMT = *) ' iloc = ',iloc
      WRITE (UNIT = nulou,FMT = *) ' iadrold = ',iadrold
      WRITE (UNIT = nulou,FMT = *) ' isizold = ',isizold
      WRITE (UNIT = nulou,FMT = "(a8)") clname
        iunit = 20+kbabs
        if(jt.eq.nitfn .and. jm.eq.1)then
                    CALL locwrite
     $                    (clname, fldold(iadrold), isizold,
     $                    iunit, ifl)
        endif
        iloc = iindex(kbabs)
        iadrnew = nadrnew(iloc)
        isiznew = nsiznew(iloc)
        clname = cnaminp(iloc)
      WRITE (UNIT = nulou,FMT = *) ' iloc = ',iloc
      WRITE (UNIT = nulou,FMT = *) ' iadrnew = ',iadrnew
      WRITE (UNIT = nulou,FMT = *) ' isiznew = ',isiznew
      WRITE (UNIT = nulou,FMT = "(a8)") clname
        iunit = 40+kbabs
        if(jt.eq.nitfn .and. jm.eq.1)then
                    CALL locwrite
     $                    (clname, fldnew(iadrnew), isiznew,
     $                    iunit, ifl)
        endif
      end do
      WRITE (UNIT = nulou,FMT = *) ' doing new bit!! '
      WRITE (UNIT = nulou,FMT = *) ' jt = ',jt
      WRITE (UNIT = nulou,FMT = *) ' jm = ',jm
      WRITE (UNIT = nulou,FMT = *) ' jf = ',jf
c      end of new bit
C
C* Do postprocessing
C
              CALL postpro (iindex, ifield)
C
C* If last iteration in PIPE or SIPC case, switch sigcld handler
              IF (iter .EQ. nitfn) THEN
                  IF (cchan .EQ. 'PIPE' .OR. cchan .EQ. 'SIPC' .OR.
     $                cchan .EQ. 'GMEM') CALL modsgc
              ENDIF
C
C* Give back fields
C
              CALL givfld (iindex, ifield, iter)
          ENDIF
C
C* End of loop over the sequential models
C
 220    CONTINUE
C
C* Reset macro arrays
C
        CALL reset
C
C* End of iterative loop
C
 210  CONTINUE
C
C
C*    3. Wait until end of child processes
C        ---------------------------------
C
      CALL waitpc
C
C
C*    4. End of routine
C        --------------
C
      RETURN
      END
