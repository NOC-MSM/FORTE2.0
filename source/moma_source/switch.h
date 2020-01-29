c====================== include file "switch.h" =========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c
c     all time dependent decisions are made in time manager
c     "tmngr.F" and communicated elsewhere in the model via 
c     logical switches .
c
c     inputs: (set through namelist)
c
c     days    = number of days to integrate
c     init    = true if this run is starts from scratch
c               false if restarting from an archived data set.
c     nmix    = number of time steps between time step mixing
c               to damp leap frog time splitting
c     eb      = true implies euler backward mixing, false
c               implies a forward timestep.
c     acor    = (>0, 0) = (implicit, explicit) treatment of coriolis
c               term
c     tsi     = number of days between printing of time step info
c     dgnstc  = number of days between diagnostic calculations:
c     snapd   = number of days between saving an archive dataset
c     archd   = number of days between saving an archive dataset
c     restrt  = true if a restart data set is to be written
c               at the end of this run
c
c     outputs: (set in tmngr.F)
c
c     the following logical switches are set within "tmngr" every
c     time step based on the above requirements.
c
c     last    = true if this is the last timestep of a run
c     mixts   = true if this is a mixing time step
c     prntsi  = true if time step info to be printed
c     diagts  = true if diagnostics are to be printed
c     snapts  = true if this is an archive timestep
c     archts  = true if this is an archive timestep
c
c     the following switches are set within the main program
c
c     first   = true if this is the first timestep of a run
c     eots    = end of a time step. always true except for first
c               pass of an euler backward time step
c     mxpas2  = second pass of mixing timestep
# ifdef de_checkbd
c     lchkbd  = logical variable used within the barotropic
c               time loop to decide whether or not to apply
c               the del-cross-del-plus filter
# endif
c
#endif
      LOGICAL_N init, restrt, eb, first, last, mixts, eots
     &,       mxpas2, prntsi, diagts, snapts, archts
c--------------------------------------------------------
c marc 10.10.2001  new switch for streamfunction, add it to common block
c switchst
     &,       streamts
c--------------------------------------------------------
#ifdef de_checkbd
     &,       lchkbd
#endif
      common /switchl/ init, eb, restrt, first, last, mixts, eots  
     &,       mxpas2, prntsi, diagts, snapts, archts 
#ifdef de_checkbd
     &,       lchkbd
#endif
      _MOMA_REAL days,acor,tsi,dgnstc,snapd,archd
      integer nmix,idebug
      common /switchr/days,acor,tsi,dgnstc,snapd,archd
      common /switchi/nmix,idebug
c-----------------------------------------
c marc 10.10.2001
      _MOMA_REAL streamd
      common /switchst/streamts
      common /switchstr/streamd
c------------------------------------------
