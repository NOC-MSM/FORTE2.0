      PROGRAM atmbidon
c======================================================================
c S. Valcke (01/99) adapted from L.Z.X. Li: This PROGRAM simulates the
c atmospheric part of a coupled model using the CLIM technique for field
c exchange. The field dimensions are realistic, corresponding to a T31 
c reduced gaussian grid, but beside the exchanges the model is pratically 
c "empty" (no REAL physics nor dynamics).
c======================================================================
      INCLUDE 'param.h'
c
c Atmosphere -> Oasis coupling fields
c
      REAL fsol(imjm)        ! Solar flux
      REAL fnsol(imjm)       ! Non-solar flux
      REAL waflx(imjm)       ! water flux
      REAL runoff(imjm)      ! runoff
      REAL taux(imjm)        ! Zonal wind stress
      REAL tauy(imjm)        ! Meridional wind stress
c
c Oasis -> Atmosphere coupling fields
      REAL sst(imjm)           ! temperature de la surface de mer
      REAL glace(imjm)         ! glace de mer
c
c======================================================================
      REAL tpos(imjm)        ! factor to change initial value from position 
                                ! 1.0 in the quarter south west
                                ! 1.1                south est
                                ! 1.2                north west
                                ! 1.3                north est
      REAL zfsol(2),zfnsol(2),zwaflx(2),zrunoff(2),ztaux(2),ztauy(2)
                        ! two values alternatively given internally 
                        ! to the atmospheric fields
      INTEGER npas      ! number of timesteps for the simulation
      INTEGER nexca     ! coupling interval (in timesteps). Here it is the same
                        ! for all fields but this needs not to be the case.
      INTEGER itimestep ! timestep in seconds
      PARAMETER (npas=120)
      PARAMETER (nexca=24)
      PARAMETER (itimestep=3600)
      INTEGER itap, ji, jf, jpos
      LOGICAL last
c
      EXTERNAL inicma           ! initialize the coupling channels
      EXTERNAL intocpl          ! sends the fields to Oasis
      EXTERNAL fromcpl          ! receives the fields from Oasis
c
      DATA zfsol/30.,300./
      DATA zfnsol/-300.,400./
      DATA zwaflx/-0.6E+2,0.6E+2/
      DATA zrunoff/0.,500./
      DATA ztaux/-0.7,-0.1/
      DATA ztauy/-0.3,0.3/

c
      OPEN(6,file='atmout',form='formatted')
c
      WRITE(6,*) 'Toyclim: the atmospheric grid is  im  = ', im
      WRITE(6,*) '                               *  jm  = ', jm
      WRITE(6,*) '                            so  imjm  = ', imjm
      WRITE(6,*) 
      WRITE(6,*) 'npas, nexca, itimestep=', npas, nexca, itimestep
c
c Prepare tpos array 
c
      DO ji = 1, imjm
          jpos = 1 + ji/((imjm+1)/4)
          tpos(ji) = 1. + FLOAT(jpos)*0.1
      ENDDO
c
c Initialize coupling 
c     
      CALL inicma(npas, nexca, itimestep)
c
      DO ji = 1, imjm
        fsol  (ji) = zfsol  (1)
        fnsol (ji) = zfnsol (1)
        waflx (ji) = zwaflx (1) 
        runoff(ji) = zrunoff(1) 
        taux  (ji) = ztaux  (1) 
        tauy  (ji) = ztauy  (1) 
      ENDDO
c
      DO 99999 itap = 1, npas
c
        WRITE (6,*) ' atmosphere timestep number : ',itap
        
c
c IF coupling timestep, get oceanic fields from Oasis
c
        IF (MOD(itap,nexca).EQ.1) THEN
            CALL fromcpl(itap-1, imjm, sst, glace)
c
c PRINT some values of the oceanic fields
c
            WRITE (6,*) '    ji     sst       glace'
            DO ji = 1,imjm,imjm/16
                WRITE (6,'(i6,2f10.2)') ji,sst(ji),glace(ji) 
            ENDDO
        ENDIF
c 
c Fake timestepping
c
        DO ji = 1, imjm
          fsol  (ji) = zfsol  (mod((itap-1)/nexca,2)+1)
          fnsol (ji) = zfnsol (mod((itap-1)/nexca,2)+1)
          waflx (ji) = zwaflx (mod((itap-1)/nexca,2)+1) * tpos(ji)
          runoff(ji) = zrunoff(mod((itap-1)/nexca,2)+1) * tpos(ji)
          taux  (ji) = ztaux  (mod((itap-1)/nexca,2)+1) 
          tauy  (ji) = ztauy  (mod((itap-1)/nexca,2)+1) 
        ENDDO
c
c IF coupling timestep, give atmospheric fields to Oasis
c
        IF (MOD(itap,nexca).EQ.0) THEN
            WRITE (6,*) '    ji      fsol     fnsol     waflx    runoff
     &      taux      tauy'
c
c       PRINT some values of the atmospheric fields
c
            DO ji = 1,imjm,imjm/16
              WRITE (6,'(i6,6f10.2)') ji,fsol(ji),fnsol(ji), 
     $            waflx(ji), runoff(ji), taux(ji), tauy(ji)
            END DO
c
c       For last time step, keep files just before coupling
c
            last=.FALSE. 
            IF (itap.eq.npas) THEN
                last=.TRUE.
            END IF 
c
c       Give atmosheric fields to Oasis
c
            CALL intocpl(itap, imjm, fsol, fnsol, waflx, runoff
     $              , taux, tauy, last) 
        ENDIF
c
99999 CONTINUE
c
c
      STOP 
      END
