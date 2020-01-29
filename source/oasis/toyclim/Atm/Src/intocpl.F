      SUBROUTINE intocpl(kt, imjm, fsol, fnsol, waflx, runoff, taux,
     $    tauy, last) 
c ======================================================================
c S. Valcke (02/99) adapted From L.Z.X Li: this subroutine provides the 
c atmospheric coupling fields to the coupler with the CLIM (PVM exchange 
c messages) technique. 
c IF last time step, writes output fields to binary files.
c ======================================================================
      IMPLICIT NONE
      INTEGER kt, imjm
c
      REAL fsol(imjm)
      REAL fnsol(imjm)
      REAL waflx(imjm)
      REAL runoff(imjm)
      REAL taux(imjm)
      REAL tauy(imjm)
      LOGICAL last
c
      INTEGER nuout
      PARAMETER (nuout = 6)
c
      INCLUDE 'clim.h'
      INCLUDE 'param_cou.h'
      INCLUDE 'inc_cpl.h'
c
      CHARACTER*8 file_name(jpmaxfld)
      INTEGER max_file
      INTEGER file_unit_max, file_unit(jpmaxfld),
     $    file_unit_field(jpmaxfld) 

      INTEGER icstep, info, jn, jf, ierror
      LOGICAL trouve
c
      INCLUDE 'oasis.h'
c
      icstep=kt 
c
      WRITE(nuout,*) ' '
      WRITE(nuout,*) 'Intocpl: sending fields to CPL, kt= ', kt
      WRITE(nuout,*) ' '

      IF (last) THEN 
c
c     -WRITE fields to binary files for coupler restart at last time step
c
c         -initialisation and files opening
c
          max_file=1
          file_unit_max=99
c         -keeps first file name
          file_name(max_file)=cl_f_writ(max_file)
c         -keeps first file unit
          file_unit(max_file)=file_unit_max
c         -decrements file unit maximum
          file_unit_max=file_unit_max-1
c         -keeps file unit for field
          file_unit_field(1)=file_unit(max_file)
c
c         -different files names counter
c
          DO jf= 2, jpflda2o1 + jpflda2o2
            trouve=.false.
            DO jn= 1, max_file
              IF (.not.trouve) THEN
                  IF (cl_f_writ(jf).EQ.file_name(jn)) THEN
c                 -keep file unit for field
                      file_unit_field(jf)=file_unit(jn)
                      trouve=.true.
                  END IF 
              END IF 
            END DO 
            IF (.not.trouve) then
c           -increment the number of different files
                max_file=max_file+1
c           -keep file name
                file_name(max_file)=cl_f_writ(jf)
c           -keep file unit for file
                file_unit(max_file)=file_unit_max
c           -keep file unit for field
                file_unit_field(jf)=file_unit(max_file)
c           -decrement unit maximum number from 99 to 98, ...
                file_unit_max=file_unit_max-1
            END IF 
          END DO 
c          
          DO jn=1, max_file 
            OPEN (file_unit(jn), FILE=file_name(jn), FORM='UNFORMATTED')
          END DO
c 
c         WRITE fields to files          
          DO jf=1, jpflda2o1 + jpflda2o2
            IF (jf.eq.1)
     $          CALL locwrite(cl_writ(jf),fnsol, imjm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.2)
     $          CALL locwrite(cl_writ(jf),fsol, imjm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.3)
     $          CALL locwrite(cl_writ(jf),waflx, imjm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.4)
     $          CALL locwrite(cl_writ(jf),runoff, imjm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.5)
     $          CALL locwrite(cl_writ(jf),taux, imjm,
     $          file_unit_field(jf),ierror, nuout) 
            IF (jf.eq.6)
     $          CALL locwrite(cl_writ(jf),tauy, imjm,
     $          file_unit_field(jf),ierror, nuout) 
            IF (jf.eq.7)
     $          CALL locwrite(cl_writ(jf),taux, imjm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.8)
     $          CALL locwrite(cl_writ(jf),tauy, imjm,
     $          file_unit_field(jf), ierror, nuout) 
          END DO 
C
C         -simulate a FLUSH
C
          DO jn=1, max_file 
            CLOSE (file_unit(jn))
          END DO 
C
C
          IF(cchan.eq.'CLIM') THEN 
C
C         -inform PVM daemon that message exchange is finished
C
              CALL CLIM_Quit (CLIM_ContPvm, info)
              IF (info .NE. CLIM_Ok) THEN
                  WRITE (6, *) 
     $                'An error occured while leaving CLIM. Error = ',
     $                info
              ENDIF
          END IF 
          RETURN    
      END IF 
C
      IF(cchan.eq.'CLIM') THEN 
C
C     -Give atmospheric fields to Oasis
C 
          DO jn=1, jpflda2o1 + jpflda2o2
C            
            IF (jn.eq.1) CALL CLIM_Export(cl_writ(jn), kt, fnsol, info)
            IF (jn.eq.2) CALL CLIM_Export(cl_writ(jn), kt, fsol, info)
            IF (jn.eq.3) CALL CLIM_Export(cl_writ(jn), kt, waflx, info)
            IF (jn.eq.4) CALL CLIM_Export(cl_writ(jn), kt, runoff, info)
            IF (jn.eq.5) CALL CLIM_Export(cl_writ(jn), kt, taux, info)
            IF (jn.eq.6) CALL CLIM_Export(cl_writ(jn), kt, tauy, info)
            IF (jn.eq.7) CALL CLIM_Export(cl_writ(jn), kt, taux, info)
            IF (jn.eq.8) CALL CLIM_Export(cl_writ(jn), kt, tauy, info)
            
            IF (info .NE. CLIM_Ok) THEN
                WRITE (nuout,*) 'STEP : Pb giving ',cl_writ(jn), ':',jn
                WRITE (nuout,*) ' at timestep = ', icstep,'kt = ',kt
                WRITE (nuout,*) 'Clim error code is = ',info
                CALL halte('STOP in intocpl ')
            ENDIF
          END DO 
      ENDIF 
C
      RETURN
      END
