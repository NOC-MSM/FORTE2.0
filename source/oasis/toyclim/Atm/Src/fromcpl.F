      SUBROUTINE fromcpl(kt, imjm, sst, gla)
c ======================================================================
c S. Valcke (02/99) adapted From L.Z.X Li: this subroutine reads the SST 
c and Sea-Ice provided by the coupler with the CLIM (PVM exchange messages)
c technique. 
c======================================================================
      IMPLICIT none
      INTEGER imjm, kt
      REAL sst(imjm)          ! sea-surface-temperature
      REAL gla(imjm)          ! sea-ice
c
      INTEGER nuout             ! listing output unit
      PARAMETER (nuout=6)
c
      INTEGER nuread, ios, iflag, icpliter
      INTEGER info, jf
c
      INCLUDE 'clim.h'
c
      INCLUDE 'oasis.h'
      INCLUDE 'param_cou.h'
c
      INCLUDE 'inc_cpl.h'
c
c
      WRITE (nuout,*) ' '
      WRITE (nuout,*) 'Fromcpl: Reading fields from CPL, kt=',kt
      WRITE (nuout,*) ' '
      CALL flush (nuout)


      IF (cchan.eq.'CLIM') THEN 

c
c     -Get interpolated oceanic fields from Oasis
c
          DO jf=1,jpfldo2a
            IF (jf.eq.1) CALL CLIM_Import (cl_read(jf) , kt, sst, info)
            IF (jf.eq.2) CALL CLIM_Import (cl_read(jf) , kt, gla, info)
            IF ( info .NE. CLIM_Ok) THEN
                WRITE(nuout,*)'Pb in reading ', cl_read(jf), jf
                WRITE(nuout,*)'Couplage kt is = ',kt
                WRITE(nuout,*)'CLIM error code is = ', info
                CALL halte('STOP in fromcpl.F')
            ENDIF
          END DO 

      ENDIF 
c
      RETURN
      END


