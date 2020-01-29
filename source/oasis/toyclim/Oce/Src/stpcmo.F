      SUBROUTINE stpcmo(kt,im, jm, sstoc, sieoc, last)
c ======================================================================
c S. Valcke (02/99) adapted From L.Z.X Li: this subroutine provides the 
c oceanic coupling fields to the coupler with the CLIM (PVM exchange 
c messages) technique. 
c IF last time step, writes output fields to binary files.
c ======================================================================
      IMPLICIT none
      INTEGER kt, im, jm
c
      REAL sstoc(im, jm)
      REAL sieoc(im, jm)
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

      WRITE(nuout,*) ' '
      WRITE(nuout,*) 'stpcmo: sending fields to CPL with kt= ', kt
      WRITE(nuout,*) ' '

      IF (last) THEN 
c
c
c     -WRITE fields to binary files for coupler restart at last time step
c
c         -initialisation
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
          
          DO jf= 2, jpfldo2a
            trouve=.false.
            DO jn= 1, max_file
              IF (.not.trouve) THEN
                  IF (cl_f_writ(jf).EQ.file_name(jn)) THEN
c keep file unit for field
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
          DO jf=1, jpfldo2a
            IF (jf.eq.1)
     $          CALL locwrite(cl_writ(jf),sstoc, im*jm,
     $          file_unit_field(jf), ierror, nuout) 
            IF (jf.eq.2)
     $          CALL locwrite(cl_writ(jf),sieoc, im*jm,
     $          file_unit_field(jf), ierror, nuout) 
          END DO 
C
C         -simulate a FLUSH
C
          DO jn=1, max_file 
            CLOSE (file_unit(jn))
          END DO 
c
c 
c
          IF(cchan.eq.'CLIM') THEN 
c
c         -inform PVM daemon, I have finished
c
              CALL CLIM_Quit (CLIM_ContPvm, info)
              IF (info .NE. CLIM_Ok) THEN
                  WRITE (6, *) 
     $                'An error occured while leaving CLIM. Error = ',
     $                info
              ENDIF
              
          END IF 
          RETURN             
      END IF 
c
      IF(cchan.eq.'CLIM') THEN
C
C     -Give oceanic fields to Oasis
C
          DO jn=1, jpfldo2a
            
            IF (jn.eq.1) CALL CLIM_Export(cl_writ(jn), kt, sstoc, info)
            IF (jn.eq.2) CALL CLIM_Export(cl_writ(jn), kt, sieoc, info)

            IF (info .NE. CLIM_Ok) THEN
                WRITE (nuout,*) 'STEP : Pb giving ',cl_writ(jn), ':',jn
                WRITE (nuout,*) ' at timestep = ', icstep,'kt = ',kt
                WRITE (nuout,*) 'Clim error code is = ',info
                CALL halte('STOP in stpcmo ')
            ENDIF
          END DO 
      ENDIF
c 
      RETURN
      END
