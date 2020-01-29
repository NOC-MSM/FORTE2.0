      PROGRAM ocebidon
c======================================================================
c S. Valcke (04/99) adapted from L.Z.X. Li: This PROGRAM simulates the
c oceanic part of a coupled model using the CLIM technique for field
c exchange. The field dimensions are realistic, corresponding to a  
c resolution of 182*152 grid points, but beside the exchanges the model  
c is pratically "empty" (no REAL physics nor dynamics).
c======================================================================
c
      INCLUDE 'param.h'
c
c Oasis -> Ocean coupling fields
      REAL zqsr(imt,jmt), znsol(imt,jmt), zemp(imt,jmt),
     &  zrunoff(imt,jmt) 
      REAL ztaux(imt,jmt), ztaux2(imt,jmt), ztauy(imt,jmt),
     &  ztauy2(imt,jmt)
c
c Ocean -> Oasis coupling fields
      REAL sstoc(imt,jmt), sieoc(imt,jmt)
c
      REAL zt(2), zi(2)
      REAL tpos(imt,jmt)          ! factor to change initial value from position 
                                ! 1.0 in the quarter south west
                                ! 1.1                south est
                                ! 1.2                north west
                                ! 1.3                north estc
c
      INTEGER npas
      INTEGER nexco
      INTEGER itimestep
      PARAMETER (npas=120)
      PARAMETER (nexco=24)
      PARAMETER (itimestep=3600)
c
      INTEGER itap, ji, jj, jpos
      LOGICAL last
c
      EXTERNAL inicmo
      EXTERNAL flx
      EXTERNAL tau
c
      DATA zt,zi/10.,20.,0.,1./
c
      OPEN(6,file='oceout',form='formatted')
c
      WRITE(6,*) 'Toyclim: the oceanic grid is  im  = ', im
      WRITE(6,*) '                           *  jm  = ', jm
      WRITE(6,*) '                        so  imjm  = ', imjm
      WRITE(6,*) 
      WRITE(6,*) 'npas, nexco, itimestep=', npas, nexco, itimestep
c
c Prepares tpos array 
c
      DO ji = 1, imt
        DO jj = 1, jmt
          jpos = 1 + ji/((imt+1)/2) + jj/((jmt+1)/2)
          tpos(ji,jj) = 1. + FLOAT(jpos)*0.1
        ENDDO
      ENDDO
c
      CALL inicmo (npas, nexco, itimestep)

      DO ji = 1, imt
        DO jj = 1, jmt
          sstoc(ji,jj) = zt(1)*tpos(ji,jj)
          sieoc(ji,jj) = zi(1)
        ENDDO
      ENDDO
c
      DO 99999 itap = 1, npas
c
        WRITE (6,*) ' ocean timestep number : ',itap
c
c IF coupling timestep, get atmospheric fields from Oasis
c
        IF (MOD(itap,nexco).EQ.1) THEN
            CALL flx(itap-1, imt, jmt, zqsr, znsol, zemp,zrunoff)
            CALL tau(itap-1, imt, jmt, ztaux, ztauy, ztaux2, ztauy2)
c
c PRINT some values of the atmospheric fields
c
            WRITE (6,*) 'ji jj ztaux ztauy'            
            DO ji = 1,imt,imt/4
              DO jj = 1,jmt,jmt/4
                WRITE (6,'(2i6,2f10.2)') ji,jj,ztaux(ji,jj),ztauy(ji,jj) 
              ENDDO
            ENDDO
        ENDIF
c 
c Fake timestepping
c
        DO ji = 1, imt
          DO jj = 1, jmt
            sstoc(ji,jj) = zt(mod((itap-1)/nexco,2)+1)*tpos(ji,jj)
            sieoc(ji,jj) = zi(mod((itap-1)/nexco,2)+1)
          ENDDO
        ENDDO
c
c IF coupling timestep, give oceanic fields to Oasis
c
        IF ((MOD(itap,nexco).EQ.0)) THEN
c
c       PRINT some values of the oceanic fields
c
            WRITE (6,*) 'ji jj sstoc  sieoc '
            DO ji = 1,imt,imt/4
              DO jj = 1,jmt,jmt/4
                WRITE (6,'(2i6,2f10.2)') ji,jj,sstoc(ji,jj),sieoc(ji,jj)
              ENDDO
            END DO
c
c      For last time step keeps files just before coupling
c
            last=.FALSE. 
            IF (itap.eq.npas) THEN
                last=.TRUE.
            END IF 
c
c       Give oceanic fields to Oasis
c
            CALL stpcmo(itap,imt, jmt, sstoc, sieoc, last)

        ENDIF
c
99999 CONTINUE
c
      STOP
      END
