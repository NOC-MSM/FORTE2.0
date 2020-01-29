c
C****
C
C**** *INICMA*  - Initialize coupled mode communication for atmosphere
C                 and exchange some initial information with Oasis
C
C     Input:
C     -----
C       KASTP  : total number of timesteps in atmospheric model
C       KEXCH  : frequency of exchange (in time steps)
C       KSTEP  : length of timestep (in seconds)
C
C     -----------------------------------------------------------
C
      SUBROUTINE inicma(kastp,kexch,kstep)
c
      INCLUDE 'param.h'
c
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
      PARAMETER (nuout = 6)
c
      INCLUDE 'clim.h'
c
      INCLUDE 'oasis.h' ! contains the name of communication technique. Here
                        ! cchan=CLIM only is possible.
c
C     -----------------------------------------------------------
C
C*    1. Initializations
C        ---------------
C
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' ROUTINE INICMA'
      WRITE(nuout,*) ' **************'
      WRITE(nuout,*) ' '
      WRITE(nuout,*) ' '
c
c     Define the model name
c
      clmodnam = 'toyatm'       ! as in $NBMODEL in Cpl/Nam/namcouple.tmp
c
c     Define the coupler name
c
      cloasis = 'Oasis'        !  always 'Oasis' as in the coupler
c
c
c     Define symbolic name for fields exchanged from atmos to coupler,
c         must be the same as (1) of the field  definition in namcouple:
c
      cl_writ(1)='CONSFTOT'
      cl_writ(2)='COSHFTOT'
      cl_writ(3)='COWATFLU'
      cl_writ(4)='CORUNOFF'
      cl_writ(5)='COZOTAUX'
      cl_writ(6)='COMETAUY'
      cl_writ(7)='COZOTAUV'
      cl_writ(8)='COMETAUU'
c
c     Define files name for fields exchanged from atmos to coupler,
c         must be the same as (6) of the field  definition in namcouple:
c
c     cl_f_writ(1)='flxatmos'
c     cl_f_writ(2)='flxatmos'
c     cl_f_writ(3)='flxatmos'
c     cl_f_writ(4)='flxatmos'
c     cl_f_writ(5)='flxatmos'
c     cl_f_writ(6)='flxatmos'
c     cl_f_writ(7)='flxatmos'
c     cl_f_writ(8)='flxatmos'
      cl_f_writ(1)='flxocean'
      cl_f_writ(2)='flxocean'
      cl_f_writ(3)='flxocean'
      cl_f_writ(4)='flxocean'
      cl_f_writ(5)='flxocean'
      cl_f_writ(6)='flxocean'
      cl_f_writ(7)='flxocean'
      cl_f_writ(8)='flxocean'
c
c
c     Define symbolic name for fields exchanged from coupler to atmosphere,
c         must be the same as (2) of the field  definition in namcouple:
c
      cl_read(1)='SISUTESU'
      cl_read(2)='SIICECOV'
c
c     Define files names for fields exchanged from coupler to atmosphere,
c         must be the same as (7) of the field  definition in namcouple:
c
      cl_f_read(1)='sstatmos'
      cl_f_read(2)='sstatmos'
c
c     Define infos to be sent initially to oasis
c
      imess(1) = kastp      ! total number of timesteps in atmospheric model
      imess(2) = kexch      ! period of exchange (in time steps)
      imess(3) = kstep      ! length of atmospheric timestep (in seconds)
      imess(4) = getpid()   ! PID of atmospheric model
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
          CALL CLIM_Init ( cljobnam, clmodnam, 3, 7,
     *                 kastp, kexch, kstep,
     *                 5, 3600, 3600, info )
c
          IF (info.ne.CLIM_Ok) THEN
              WRITE ( nuout, *) ' inicma : pb init clim '
              WRITE ( nuout, *) ' error code is = ', info
              CALL halte('STOP in inicma')
            ELSE
              WRITE(nuout,*) 'inicma : init clim ok '
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
c         -Loop on total number of coupler-to-atmosphere fields
c         (see lib/clim/src/CLIM_Define for the definition of input parameters)
          DO jf=1, jpfldo2a
            CALL CLIM_Define (cl_read(jf), clim_in , clim_double, iparal
     $          , info )  
          END DO 
c
c         -Loop on total number of atmosphere-to-coupler fields 
c         (see lib/clim/src/CLIM_Define for the definition of input parameters)
          DO jf=1, jpflda2o1+jpflda2o2
            CALL CLIM_Define (cl_writ(jf), clim_out , clim_double,
     $          iparal, info )   
          END DO 
c
          WRITE(nuout,*) 'inicma : clim_define ok '
c
c         -Join a pvm group, wait for other programs and broadcast usefull 
c          informations to Oasis and to the ocean (see lib/clim/src/CLIM_Start)
          CALL CLIM_Start ( imxtag, info )
          IF (info.ne.clim_ok) THEN
              WRITE ( nuout, *) 'inicma : pb start clim '
              WRITE ( nuout, *) ' error code is = ', info
              CALL halte('stop in inicma')
            ELSE
              WRITE ( nuout, *)  'inicma : start clim ok '
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

      RETURN
      END
