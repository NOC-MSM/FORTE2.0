      SUBROUTINE flx(kt, im, jm, zqsr, znsol, zemp, zrunoff)
c ======================================================================
c S. Valcke (04/99) adapted From L.Z.X Li: this subroutine reads the flux
c fields provided by the coupler with the CLIM (PVM exchange messages)
c technique. 
c======================================================================
      IMPLICIT none
c
      INTEGER kt, im, jm
c
      REAL zqsr(im, jm)
      REAL znsol(im, jm)
      REAL zemp(im, jm)
      REAL zrunoff(im, jm)
c
      INTEGER nuout, info
      PARAMETER (nuout = 6)
c
      INCLUDE 'clim.h'
c
      INCLUDE 'oasis.h'
      INCLUDE 'param_cou.h'
      INCLUDE 'inc_cpl.h'
c
      INTEGER jf 
c
      WRITE (nuout,*) ' '
      WRITE (nuout,*) 'flx: Reading fields from CPL, kt=',kt
      WRITE (nuout,*) ' '
      CALL flush (nuout)

      IF (cchan.eq.'CLIM') THEN
c
c     -Get interpolated atmospheric fields from Oasis
c
          DO jf=1,jpflda2o1
            IF (jf.eq.1) CALL CLIM_Import (cl_read(jf) , kt, zqsr, info)
            IF (jf.eq.2) CALL CLIM_Import (cl_read(jf) , kt, znsol,info)
            IF (jf.eq.3) CALL CLIM_Import (cl_read(jf) , kt, zemp, info)
            IF (jf.eq.4) CALL CLIM_Import (cl_read(jf) ,kt,zrunoff,info)
            IF ( info .NE. CLIM_Ok) THEN
                WRITE(nuout,*)'Pb in reading ', cl_read(jf), jf
                WRITE(nuout,*)'Couplage kt is = ',kt
                WRITE(nuout,*)'CLIM error code is = ', info
                CALL halte('STOP in flx.F')
            ENDIF
          END DO 

      ENDIF

      RETURN
      END
