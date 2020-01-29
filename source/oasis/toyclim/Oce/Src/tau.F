      SUBROUTINE tau(kt,im, jm, ztaux, ztauy, ztaux2, ztauy2)
c ======================================================================
c S. Valcke (04/99) adapted From L.Z.X Li: this subroutine reads the 'tau'
c fields provided by the coupler with the CLIM (PVM exchange messages)
c technique. 
c======================================================================
      IMPLICIT none
c
      INTEGER kt, im, jm
c
      REAL ztaux(im, jm)
      REAL ztauy(im, jm)
      REAL ztaux2(im, jm)
      REAL ztauy2(im, jm)
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
      WRITE (nuout,*) 'tau: Read fields from CPL, kt=', kt
      WRITE (nuout,*) ' '
      CALL flush (nuout)
c
      IF (cchan.eq.'CLIM') THEN 
c
c     -Get interpolated atmospheric fields from Oasis
c
          DO jf=1,jpflda2o2
            IF (jf.eq.1) CALL CLIM_Import (cl_read(5), kt, ztaux,info)
            IF (jf.eq.2) CALL CLIM_Import (cl_read(6), kt, ztauy, info)
            IF (jf.eq.3) CALL CLIM_Import (cl_read(7), kt, ztaux2,info)
            IF (jf.eq.4) CALL CLIM_Import (cl_read(8),kt, ztauy2,info)
            IF ( info .NE. CLIM_Ok) THEN
                WRITE(nuout,*)'Pb in reading ', cl_read(jf), jf
                WRITE(nuout,*)'Couplage kt is = ',kt
                WRITE(nuout,*)'CLIM error code is = ', info
                CALL halte('STOP in tau.F')
            ENDIF
          END DO 

      ENDIF 

      RETURN
      END
