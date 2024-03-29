#include "cpp_options.h"
c
c**** *inicmo*  - initialize coupled mode communication for ocean
c                 and exchange some initial information with Oasis
c
c     input:
c     -----
c       kitro  : total number of timesteps in oceanic model
c       kexco  : frequency of exchange for the fluxes (in time steps)
c       kstpo  : timestep value (in seconds)
c
c     -----------------------------------------------------------
c
      SUBROUTINE inicmo(kastp,kexch,kstep)
c
c     INCLUDE 'param.h'
#include "param.h"
c
      integer imjm
      PARAMETER ( imjm =(imt-2*nhalo)*(jmt-2*(nhalo-1)) )
      INTEGER kastp, kexch, kstep
      INTEGER iparal(3)
      INTEGER ifcpl, idt, info, imxtag, istep, jf
c
      INCLUDE 'param_cou.h'
      INCLUDE 'inc_cpl.h'
      CHARACTER*3 cljobnam      ! experiment name
      CHARACTER*6 clmodnam      ! model name
      CHARACTER*5 cloasis       ! coupler name (Oasis)
      INTEGER imess(4)
      INTEGER getpid            ! system functions
      INTEGER nuout
      PARAMETER (nuout=6)
c
      INCLUDE 'clim.h'
c
      INCLUDE 'oasis-cou.h'     ! contains the name of communication technique.
                                ! Here cchan=CLIM only is possible.
c
C     -----------------------------------------------------------
C
C*    1. Initializations
C        ---------------
C      
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' ROUTINE INICMO'
      WRITE(nuout,*) ' **************'
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' '
c
c     Define the model name
c
      clmodnam = 'toyoce'       ! as $NBMODEL in namcouple
c
c     Define the coupler name
c
      cloasis = 'Oasis'        !  as in coupler
c
c
c     Define symbolic name for fields exchanged from ocean to coupler,
c         must be the same as (1) of the field  definition in namcouple:
c
      cl_writ(1)='SOSSTSST'
      cl_writ(2)='SOZONCUR'
      cl_writ(3)='SOMERCUR'
c
c     Define files name for fields exchanged from ocean to coupler,
c         must be the same as (6) of the field  definition in namcouple:
c
c     cl_f_writ(1)='sstocean'
c     cl_f_writ(2)='sstocean'
      cl_f_writ(1)='sstatmos'
      cl_f_writ(2)='sstatmos'
      cl_f_writ(3)='sstatmos'
c
c     Define symbolic name for fields exchanged from coupler to ocean
c         must be the same as (2) of the field  definition in namcouple:
c
      cl_read(1)='SONSHLDO'     ! non solar heat flux (positif vers l'ocean)
      cl_read(2)='SOSHFLDO'     ! solar flux
      cl_read(3)='SOWAFLDO'     ! wter flux
      cl_read(4)='SORUNOFF'     ! runoff
      cl_read(5)='SOZOTAUX'     ! first zonal wind stress
      cl_read(6)='SOMETAUY'     ! first meridien wind stress
      cl_read(7)='SOZOTAUV'     ! second zonal wind stress
      cl_read(8)='SOMETAUU'     ! second meridien wind stress
c
c     Define files name for fields exchanged from coupler to ocean
c         must be the same as (7) of the field  definition in namcouple:
c
      cl_f_read(1)='flxocean'
      cl_f_read(2)='flxocean'
      cl_f_read(3)='flxocean'
      cl_f_read(4)='flxocean'
      cl_f_read(5)='flxocean'
      cl_f_read(6)='flxocean'
      cl_f_read(7)='flxocean'
      cl_f_read(8)='flxocean'
c
c     Define infos for sending to oasis
c
      imess(1) = kastp
      imess(2) = kexch
      imess(3) = kstep
      imess(4) = getpid()
c
c     Initialization and exchange of initial info in the CLIM technique
c
      IF (cchan.eq.'CLIM') THEN
c  
c     Define the experiment name :
c
          cljobnam = 'CLI'      ! as $JOBNAM in namcouple
c
c         Start the coupling 
c         (see lib/clim/src/CLIM_Init for the definition of input parameters)
c
          CALL CLIM_init ( cljobnam, clmodnam, 3, 7,
     *                 kastp, kexch, kstep,
     *                 5, 3600, 3600, info )
c
          IF (info.ne.clim_ok) THEN
              WRITE ( nuout, *) ' inicmo : pb init clim '
              WRITE ( nuout, *) ' error code is = ', info
              CALL halte('STOP in inicmo')
            ELSE
              WRITE(nuout,*) 'inicmo : init clim ok '
          ENDIF
c
c         For each coupling field, association of a port to its symbolic name
c
c         -Define the parallel decomposition associated to the port of each
c          field; here no decomposition for all ports.

          iparal ( clim_strategy ) = clim_serial
          iparal ( clim_length   ) = imjm
          iparal ( clim_offset   ) = 0
c
c         -Loop on total number of coupler-to-ocean fields
c         (see lib/clim/src/CLIM_Define for the definition of input parameters)

          DO jf=1, jpflda2o1+jpflda2o2
            CALL CLIM_Define (cl_read(jf), clim_in , clim_double, iparal
     $          , info )  
          END DO 
c
c         -Loop on total number of ocean-to-coupler fields 
c         (see lib/clim/src/CLIM_Define for the definition of input parameters)
          DO jf=1, jpfldo2a
            CALL CLIM_Define (cl_writ(jf), clim_out , clim_double,
     $          iparal, info )   
          END DO 
          WRITE(nuout,*) 'inicmo : clim_define ok '
c
c         -Join a pvm group, wait for other programs and broadcast usefull 
c          informations to Oasis and to the atmosphere 
c          (see lib/clim/src/CLIM_Start)

          CALL CLIM_Start ( imxtag, info )
          IF (info.ne.clim_ok) THEN
              WRITE ( nuout, *) 'inicmo : pb start clim '
              WRITE ( nuout, *) ' error code is = ', info
              CALL halte('stop in inicmo')
            ELSE
              WRITE ( nuout, *)  'inicmo : start clim ok '
          ENDIF
c
c         -Get initial information from Oasis
c          (see lib/clim/src/CLIM_Stepi)
          CALL CLIM_Stepi (cloasis, istep, ifcpl, idt, info)
          IF (info .NE. clim_ok) THEN
              WRITE ( UNIT = nuout, FMT = *)
     $            ' warning : problem in getting step info ',
     $            'from oasis '
              WRITE (UNIT = nuout, FMT = *)
     $            ' =======   error code number = ', info
            ELSE
              WRITE (UNIT = nuout, FMT = *)
     $            ' got step information from oasis '
          ENDIF
          WRITE ( nuout, *) ' number of tstep in oasis ', istep
          WRITE ( nuout, *) ' exchange frequency in oasis ', ifcpl
          WRITE ( nuout, *) ' length of tstep in oasis ', idt
      ENDIF
c
      RETURN
      END
