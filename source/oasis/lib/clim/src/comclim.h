c
c     - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c*    =comclim.h=  CLIM 1.1 internal include file
c                  Coupling Library for Interfacing Models
c
c     - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c-----Unit number for trace file
c
      integer*4	nulprt
c
c-----Models descriptors
c
      integer*4 nmods, mynum, mytid, modtid(0:CLIM_MaxMod-1)
c
c-----Ports descriptors
c
      integer*4 nports, 
     *          myport(5+CLIM_MaxLink,CLIM_MaxPort),
     *          mydist(CLIM_ParSize,CLIM_MaxPort)
c
c
c-----Links descriptors
c
      integer*4 nlinks, 
     *          mylink(4+CLIM_ParSize,CLIM_MaxLink)
c
c-----Data encoding
c
      integer*4 mycode, ncode(0:CLIM_MaxMod-1)
c
c-----Inquiry descriptors
c
      integer*4 nbsend, nbrecv
c
c-----Time out stuff
c
      integer*4 ntiret, ntiogp, ntiout, nexit
c
c-----Timesteps descriptors
c
      integer*4 mystep, mystdt, myfcpl
      integer*4 nstep(0:CLIM_MaxMod-1),
     *          nstdt(0:CLIM_MaxMod-1),
     *          nfcpl(0:CLIM_MaxMod-1)
c
c-----Delta time between models
c
      real*8 delta(0:CLIM_MaxMod-1), delte(0:CLIM_MaxMod-1)
c
c-----Character strings
c
      character*8  cgroup
      character*12 cnaprt
      character*32 cports(CLIM_MaxPort)
      character*32 cmynam, cnames(0:CLIM_MaxMod-1)
c
c-----Commons
c
      common/CLIM_comc/cnaprt, cgroup, cports, cmynam, cnames
      common/CLIM_comd/delta,  delte
      common/CLIM_comi/nulprt, nmods,  mynum,  mytid,  modtid,
     *                 mystep, mystdt, myfcpl, nstep,  nstdt,  nfcpl,
     *                 nports, myport, nlinks, mylink, mydist,
     *                 nbsend, nbrecv, mycode, ncode,
     *                 ntiret, ntiogp, ntiout, nexit
c
