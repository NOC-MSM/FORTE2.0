#include "cpp_options.h"
      program moma
c     implicit none
c
c23456789012345678901234567890123456789012345678901234567890123456789012
c=======================================================================
c  Program MOMA,     Version %I%
c
c  Free surface ocean model for use with array processors.
c
c  Copyright 1993.  D.J. Webb,  
c                   Institute of Oceanographic Sciences,
c                   Godalming, Surrey GU8 5UB, U.K..
c  
c %Z% SCCS information - module:  %M%     
c %Z%                    version: %I%    date: %G%
c
c
c  This code is a free surface version of the Bryan-Cox-Semtner
c  code reorganised for efficient running on array processor
c  computers.  The basic finite difference scheme used in the
c  model is described in Bryan (1969), Semtner (1974) and
c  Cox (1984).  The present code follows most closely
c  the format of the GFDL Modular Ocean Model (Pacanowski et al,
c  1990), and makes use of many of the subroutines and include
c  files of that model.  More detailed information about the 
c  present code is given in Webb (1993, 1998).
c
c  The code may be freely adapted and used without charge.  For 
c  further information on the model contact the author.
c
c     Copyright 1993, 1994, 1995, 1996, 1998
c     D.J. Webb,  
c     Southampton Oceanography Centre
c     Empress Dock, Southampton SO14 3ZH, U.K..
c
c     e-mail  David.Webb@soc.soton.ac.uk 
c  
c  The main differences from the modular ocean model code are:
c
c  1.  Collection of all 'array processor' loops (i.e. loops over the
c      horizontal indices ic and jc) in subroutine step.  When an array
c      processor is used, code should be added to this subroutine to 
c      partition the horizontal index (ic, jc) ranges between the
c      different processors so that they each have a similar workload.
c  2.  Revised common block structures.  All variables are stored
c      in core.
c  3.  Removal of all 'slab' optimisation code in the program and
c      the introduction of code designed to optimise inner loop 
c      calculations using the vertical index k.
c  4.  To simplify the development and testing of the array processor
c      code the following features of the moma code were removed:
c        (a) Diagnostic calculations in routines clinic and tracer.
c        (b) Moma code options.
c      It should now be straightforward (but possibly time consuming)
c      to add the features back into the present code if they are 
c      required:
c  5.  The stream function code is removed and replaced by a free-
c      surface code.  The latter is similar to Killworth, Stainforth
c      Webb and Patterson (1989).  For efficiency, the mean horizontal
c      velocity is now used instead of horizontal transport and
c      the viscous terms are calculated in routine clinic.  
c      A leapfrog timestepping scheme is used with averaging over each
c      baroclinic timestep to prevent aliasing problems in the tropics.
c  6.  In the baroclinic momentum equation, a revised horizontal
c      advection scheme option is included.  The old scheme can still  
c      be used by specifying the flag 'oldadv'
c  7.  Near topography, in the baroclinic momentum equation, a revised
c      vertical advection scheme option is included.
c  8.  An option to precalculate the baroclinic part of the 
c      pressure filed is included using flag 'presetp'.
c  9.  Snapshot and archive functions separated.  Input 'snapd' 
c      and 'archd'.
c
c  Further updates by John Stark (2002)
c  10. Addition of netcdf archive / snapshot options
c  11. Correction of tracer / momentum stepping to conserve properties.
c  12. Addition of isopycnal mixing option.
c  13. Use of free surface height in tracer stepping.  
c
c  cpp precompiler options:
c
c     'hcomments' - include comments from '*.h' files
c     'cyclic'    - invoke cyclic boundary conditions
c     'presetp'   - precalculate the baroclinic pressure field
c                   before calling clinic
c     'simple_world' - use routine topog to construct kmt, this 
c                   generates a simplified version of the global ocean.
c     'de_checkbd'- use filter to remove checkerboarding in free
c                   surface height field.
c     'ncsa_hdf'  - produce hdf versions of archive files.
c     'halo2'     - use a double halo - for 5 point schemes like MSQ
c     'msq'       - use the modified split quick (msq) scheme for 
c                   advection. See Webb (1998)
c
c     'netcdf'    - include netcdf option for archives / snapshots
c
c    isopycnal mixing options:
c     'full_griffies'   
c     'gent_mcwilliams'
c     'gm_diag'
c     'isopycmix'
c
c     'w_write'  - Compute and store w velocity in tracer routine.
c     'de_checkbd_T' - Include filter to remove checkerboarding in tracer
c                      fields
c     'flux_write' - write out average fluxes to netcdf archive
c
c    CMIP project additions (Adam Blaker: atb299@noc.ac.uk): 
c     'tendency_write' - write out tendency terms to netcdf
c     'cmip_flx'       - write out daily fluxes to netcdf (f000*.nc)
c     'cmip_flxrd'     - read daily fluxes from netcdf
c     'c_write'        - write out speed of sound to netcdf
c     'rho_is_write'   - write out in-situ density to netcdf
c     'nflux_write'    - write out poleward heat/fw fluxes (or transports)
c     'perturbation'   - adds 0.1Sv hosing to N. Atlantic (50N-70N)
c
c     'sst_wr'         - writes out SST immediately before passing to OASIS
c     'sst_rd'         - reads SST from file immediately before passing to OASIS
c
c  hdf.F / netcdf.F need to know if real*8 or real*4 variables are being used
c
c     'REAL_8'    - set when real*8 variables are being used
c
c  the following are included for comparison with earlier code versions
c
c     'oldadv'    - use the origonal scheme for the horizontal
c                   advection velocity at velocity points.
c     'free_eb'   - use the euler backward timestepping scheme without
c                   averaging for the free surface equations.
c                   NOTE: The default is to use a leapfrog scheme
c                   with time averaging.
c     'OLD_NON_CONSERVING' - Reproduce moma v1.17 code.
c
c     'NONLIN_FREE_SURFACE' - Account for varying surface height in tracer
c                             and momentum equations
c
c  Compilation:
c
c  Set up configrc to suit run  'configure', and then 'make'
c
c  References:
c
c  Bryan, K., 1969:  A numerical method for the circulation of the
c  World Ocean.  Journal of Computational Physics, 4, 347-  .
c
c  Semtner, A.J., 1974:  A general circulation model for the 
c  World Ocean.  UCLA Department of Meteorology Technical Report
c  No. 8, 99pp.
c
c  Cox, M.D., 1984:  A primitive equation, 3-dimensional model of
c  the ocean.  GFDL Ocean Technical Report No.1, Geophysical
c  Fluid Dynamics Laboratory/NOAA, Princeton University, 
c  Princeton N.J., U.S.A..
c
c  Killworth, P.D., Stainforth, D., Webb, D.J. and Paterson, P.M.,
c  1989:  A free surface Bryan-Cox-Semtner model.  Report No. 270,  
c  Institute of Oceanographic Sciences, Wormley, U.K..
c
c  Pacanowski, R.C., Dixon, K., Rosati, A., 1990:  The GFDL Modular
c  Ocean Model 1.0.  Geophysical Fluid Dynamics Laboratory/NOAA, 
c  Princeton University, Princeton, N.J., U.S.A..  (Unpublished 
c  manuscript).
c
c  Webb, D.J., 1993:  An ocean model code for array processor
c  computers.  Internal Document No.324, Institute of Oceanographic 
c  Sciences, Wormley, U.K..
c
c  Webb, D.J., 1993:  An ocean model code for array processor
c  computers.  Computers and Geosciences, 22(5), 569-578.
c
c  Webb, D.J., 1998:  Improved advection schemes for ocean models.
c  Journal of Atmospheric and Oceanic Technology, 15, 1171-1187. 
c
c=======================================================================
c
#include "param.h"
#include "scalar.h"
#include "switch.h"
#include "coord.h"
#include "ctmngr.h"
#include "grdvar.h"
#include "levind.h"
#include "timelv.h"
#include "slabs.h"

#include "cdiag.h"
#include "chmix.h"
#include "cvbc.h"
#include "cvmix.h"
#include "frees.h"
#include "iounit.h"
#include "versno.h"
#ifdef restor98
#include "restor98.h"
#endif
#ifdef flux_adj
#include "flux_adj.h"
#endif
c=======================================================================
c Local:
      real*4 dtime 
      external dtime
      real*4 mytime
      REAL*4 utime(3)
      character*8 budget_timestamp

      integer i,j,k,ioca,ibk,isp,iept,ii,jrev,loop
      _MOMA_REAL ocnp
      _MOMA_REAL latmin,longmin
c
c     function:
      logical mom_isnan
      external mom_isnan

      namelist /coords/ dz, dxdeg, dydeg, latmin,longmin
      namelist /contrl/ init,fnflx,fnsst,fnrest,fnkmt,days,restrt,nmix 
     &,                 eb,ncon,tsi,dgnstc,snapd,archd,acor,streamd
# ifdef de_checkbd
     &,                 dchkbd
# endif
     &,                 ftrest, ftsnap, ftarch
      namelist /tsteps/ dtts, dtuv, dtbt
      namelist /eddy/   am, ah, fkpm, fkph, cdbot

#ifdef isopycmix
      namelist /isoparams/ slmx, ahisop, athkdf, ahsteep, aidif
#endif

#ifdef oasis
#include "oasis.h"
c Ocean -> Oasis coupling fields
      INTEGER npas
      INTEGER nexco
      INTEGER itimestep

      EXTERNAL inicmo
      EXTERNAL flx
      EXTERNAL tau
      EXTERNAL stpcmo

      REAL start,sync,async,counter
      REAL accts,accuv,accbt,normts,normuv,normbt
      LOGICAL doaccel, startaccel, stopaccel,accelrun
      namelist /accelist/ accelrun,start, sync, async
     $                   ,accts, accuv, accbt

#endif
      _MOMA_REAL s2dadvn(imt,jmt,nt),s2dadve(imt,jmt,nt),
     1   s2dadvw(imt,jmt,nt),s2dadvs(imt,jmt,nt)
      _MOMA_REAL s2ddifn(imt,jmt,nt),s2ddife(imt,jmt,nt),
     1   s2ddifw(imt,jmt,nt),s2ddifs(imt,jmt,nt)
      _MOMA_REAL s2dbiharmn(imt,jmt,nt),s2dbiharme(imt,jmt,nt),
     1   s2dbiharmw(imt,jmt,nt),s2dbiharms(imt,jmt,nt)
      _MOMA_REAL s2dbiharmup(imt,jmt,nt),s2ddflux(imt,jmt,nt)
      _MOMA_REAL s2dddt(imt,jmt,nt),s2dvtf(imt,jmt,nt),
     1   s2dw(imt,jmt,nt),s2dslpdif(imt,jmt,nt),s2dall(imt,jmt,nt)
      integer nsum
      common /s2ds/s2dadvn,s2dadve,s2dadvw,s2dadvs,
     1   s2ddifn,s2ddife,s2ddifw,s2ddifs,
     1   s2dbiharmn,s2dbiharme,s2dbiharmw,s2dbiharms,
     1   s2dbiharmup,s2ddflux,
     1   s2dddt,s2dvtf,s2dw,s2dslpdif,s2dall
     1   ,nsum

      nsum=0
      do m=1,nt
      do j=1,jmt
      do i=1,imt
      s2dddt(i,j,m)=c0
      s2dvtf(i,j,m)=c0
      s2dw(i,j,m)=c0
      s2dslpdif(i,j,m)=c0
      s2dadvn(i,j,m)=c0
      s2dadve(i,j,m)=c0
      s2dadvw(i,j,m)=c0
      s2dadvs(i,j,m)=c0
      s2ddifn(i,j,m)=c0
      s2ddife(i,j,m)=c0
      s2ddifw(i,j,m)=c0
      s2ddifs(i,j,m)=c0
      s2dbiharmn(i,j,m)=c0
      s2dbiharme(i,j,m)=c0
      s2dbiharmw(i,j,m)=c0
      s2dbiharms(i,j,m)=c0
      s2dbiharmup(i,j,m)=c0
      s2ddflux(i,j,m)=c0
      end do
      end do
      end do

c
c=======================================================================
c  start timing
c=======================================================================
c 
      mytime = dtime(utime)
      utime(3)=0.0
c
c=======================================================================
c  initialise constants and name
c=======================================================================
c
      pi     = c4*atan(c1)
      radian = c360/(c2*pi)
      omega  = pi/43082.0
      grav =980.6
      radius =6370.e5
c
      model(1) =' Stripped down MOM code for array processor.'
      model(2) =' Module: %M%.  Version: %I%. '//
     &              ' Date: %G%'
c
c=======================================================================
c  initialise parameters and variables
c=======================================================================
c
      dxdeg = c1                !Grid spacings
      dydeg = c1
      do k=1,km
       dz( k) = c1
      enddo
c edge of 'real' grid (not including halo area).
      latmin = c0               
      longmin = c0

c
      init = .true.
      first= .true.
      days = 2.0
      eb    = .true.
      acor  = 0.6
      nmix  = 16
      ncon  = 1
      tsi   = 2.0
      dgnstc= 2.0
      snapd = 2.0
      archd = 2.0
      restrt = .false.
      fnsst = ' '
      fnflx = ' '
      fnrest = ' '
      fnkmt  = 'ocean.kmt'
#ifdef ncsa_hdf
      ftrest = 'hdf'
      ftsnap = 'hdf'
      ftarch = 'hdf'
#else
#ifdef netcdf
      ftrest = 'ncdf'
      ftsnap = 'ncdf'
      ftarch = 'ncdf'
#else
      ftrest = 'unf'
      ftsnap = 'unf'
      ftarch = 'unf'
#endif /*netcdf*/
#endif /*ncsa_hdf*/
      iorest = 21
      iokmt = 24
c
      dtts = 10800d0
      dtuv = 10800d0
      dtbt = 100d0
c
      am    = 1.0e9 
      ah    = 2.0e7
      fkph  = 20.0
      fkpm  = 1.0
      cdbot = 0.0
#ifdef de_checkbd
      dchkbd = 0.1
#endif
c
c=======================================================================
c     begin introductory section
c     open input file and write MOM version information
c=======================================================================
c
      open (stdin,file='ocean.in')
      write (stdout,'(/2(25x,a80/)/)') model
c
c-----------------------------------------------------------------------
c     read in run parameters
c-----------------------------------------------------------------------
c
      read  (stdin, coords)
      write (stdout,coords)
      read  (stdin, contrl)
      write (stdout,contrl)
      read  (stdin, eddy)
      write (stdout,eddy)
      read  (stdin, tsteps)
      write (stdout,tsteps)
#ifdef isopycmix
      read (stdin, isoparams)
      write (stdout, isoparams)
#endif
#ifdef oasis
      read  (stdin, accelist)
      write  (stdout, accelist)
#endif
      close (unit=stdin,status='keep')

#ifdef restor98
      open (68, file='ecmwfsst-monthly.4deg',status='old')
      open (69, file='lev98sal-monthly.4deg',status='old')
      open (70, file='xstress-igcm-month',status='old')
      open (71, file='ystress-igcm-month',status='old')
      do ll=1,12
      do jj=1,45
      do ii=1,90
      read (68,*) restortemp(ii,jj,ll)
      read (69,*) restorsal(ii,jj,ll)
      read (70,*) restoruwind(ii,jj,ll)
      read (71,*) restorvwind(ii,jj,ll)
      end do
      end do
      end do
      close(68)
      close(69)
      close(70)
      close(71)
#endif
#ifdef flux_adj
      open (70, file='heat_flux_adj',status='old')
      open (71, file='water_flux_adj',status='old')
      do ll=1,12
      do jj=1,45
      do ii=1,90
      read (70,*) heat_flux(ii,jj,ll)
       heat_flux(ii,jj,ll) = heat_flux(ii,jj,ll)/4.184d4
      read (71,*) water_flux(ii,jj,ll)
       water_flux(ii,jj,ll) = 
     $              water_flux(ii,jj,ll)/-9.0102857143d6
      end do
      end do
      end do
      close(70)
      close(71)
#endif

c
      stlon = -dxdeg*nhalo + longmin
      stlat = -dydeg*nhalo + latmin

#ifdef oasis 
      normts=dtts 
      normuv=dtuv
      normbt=dtbt

      counter=1.

      doaccel=.false.
#endif

c
c-----------------------------------------------------------------------
c     calculate free surface model (barotropic) timestep
c-----------------------------------------------------------------------
c
      ntbt = max(1,nint(dtuv/dtbt))
      if(dtuv.ne.dtbt*ntbt)then
        write(stdout,'(a)') ' dtuv not integer multiple of dtbt.'
        write(stdout,*) ' dtuv is  ',dtuv
        write(stdout,*) ' dtbt was ',dtbt
        dtbt = dtuv/ntbt
        write(stdout,*) ' dtbt now set to set to:',dtbt
      endif
c
c-----------------------------------------------------------------------
c     list code options chosen by user for this model run
c-----------------------------------------------------------------------
c
      call docmnt (stdout, .false., .false., .false., .false.)
c
c-----------------------------------------------------------------------
c     set up model grids in x, y, & z
c-----------------------------------------------------------------------
c
      call grids
#ifdef de_checkbd
c
c-----------------------------------------------------------------------
c scale the checkerboard filter weighting factor (zw from grids)
c-----------------------------------------------------------------------
c
      dchkbd = dchkbd*grav*dtbt*zw(KM)
c
#endif
c
c=======================================================================
c     end introductory section
c=======================================================================
c
c-----------------------------------------------------------------------
c     set up initial pointers to timestep storage.  nc is current 
c     timestep, np is next and nm is previous timestep.  nm0, nc0
c     and np0 are the corresponding pointers for the free surface
c     model.
c-----------------------------------------------------------------------
c
      nm = 1
      nc = 2
      np = 3
      nm0 = 1
      nc0 = 2
      np0 = 3
c
c-----------------------------------------------------------------------
c     is this a start from initial conditions or a restart?
c     in either case, set the number of vertical levels on the "t" 
c     grid
c-----------------------------------------------------------------------
c
      if (init) then
        call ocn1st
      else
	print*,'This is a restart'
        call archrd
      endif

#ifdef average
c     Initialise the time averaging fields
      call av_zero
#endif
#ifdef w_write
      do j=1,jmt
        do i=1,imt
          do k=0,km
            w(k,i,j) = c0
          enddo
        enddo
      enddo
#endif
#ifdef flux_write
      do ii=1,nt
        do j=1,jmt
          do i=1,imt
            surf_t_flux(i,j,ii) = c0
            surf_n_flux(i,j,ii) = c0
          enddo
        enddo
      enddo
#endif
#ifdef cmip_flxrd
      do ii=1,nt
        do j=1,jmt
          do i=1,imt
            surfrd_t_flux(i,j,ii) = c0
          enddo
        enddo
      enddo
#endif
#ifdef conv_write
      do j=1,jmt
        do i=1,imt
          do k=1,km
            conv(k,i,j) = 0
          enddo
        enddo
      enddo
#endif
      write(6,*)days,start
      ifirstday=totday
      start=start+ifirstday
      days=days+ifirstday
      write(6,*)days,start

#ifdef oasis
      nexco=1
      itimestep=1
      npas=(days-totday)*nexco


      CALL inicmo (npas, nexco, itimestep)

      DO ji = 1, imt
        DO jj = 1, jmt
          sstoc(ji,jj) = 0.
          zscoc(ji,jj) = 0.
          mscoc(ji,jj) = 0.
        ENDDO
      ENDDO
#endif

c
c-----------------------------------------------------------------------
c     compute number of vertical levels on the "u" grid
c-----------------------------------------------------------------------
c
      do 900 i=1,imt
        kmu(i,jmt) = 0
900   continue
      do 1000 j=jcbeg,jcend
      do 990 i=icbeg,icend
        kmu(i,j) =min (kmt(i,j),kmt(i+1,j),kmt(i,j+1),kmt(i+1,j+1))

990   continue
1000  continue
      do 1100 j=1,jmt
        do i=1,nhalo
#ifdef cyclic
          kmu(i,j) = kmu(imtm2h+i,j)
          kmu(imtm1h+i,j) = kmu(nhalo+i,j)
#else
          kmu(i,j) = 0
          kmu(imtm1h+i,j) = 0
#endif
        enddo
1100  continue
      do j=jcbeg,jcend
        do i = icbeg,icend
c
c  find max of all the neighbouring points.
c
#ifdef oldadv
          kmd(i,j)  = max(kmu(i+1,j),kmu(i-1,j),kmu(i,j+1)
     &         ,kmu(i,j-1),kmu(i,j))
#else 
          kmd(i,j)  = max(kmu(i-1,j-1),kmu(i,j-1),kmu(i+1,j-1),
     &                    kmu(i-1,j  ),kmu(i,j  ),kmu(i+1,j  ),
     &                    kmu(i-1,j+1),kmu(i,j+1),kmu(i+1,j+1))
#endif
        enddo
      enddo
      do j=1,jmt
        do i=1,nhalo
#ifdef cyclic
          kmd(i,j) = kmd(imtm2h+i,j)
          kmd(imtm1h+i,j) = kmd(nhalo+i,j)
#else
          kmd(i,j) = 0
          kmd(imtm1h+i,j) = 0
#endif
        enddo
      enddo 
c
c-----------------------------------------------------------------------
c     compute area and volume of ocean ("t,s" grid boxes)
c-----------------------------------------------------------------------
c
      area   = c0
      volume = c0
      ocnp   = 0
      ioca   = 0
      do j=jcbeg,jcend
      do i=icbeg,icend
        if (kmt(i,j) .gt. 0) then
          area   = area + cst(j)*dx*dy
          volume = volume + cst(j)*dx*dy*zw(kmt(i,j))
          ocnp   = ocnp + float(kmt(i,j))
        endif
      end do
      end do
      write (stdout,301) area, volume
c
c-----------------------------------------------------------------------
c       print map of "kmt" levels. 
c-----------------------------------------------------------------------
c
c
      write (stdout,401)
      do 410 ibk=1,imt,40
        isp  = ibk
        iept = ibk + 40 - 1
        if(iept.gt.imt) iept=imt
        write (stdout,'(/, 4x, 40i3)') (ii, ii=isp,iept)
        do 400 jrev=1,jmt
          j=jmt-jrev+1
          write (stdout,'(1x,i3, 40i3)')j,(kmt(i,j),i=isp,iept)
 400    continue
 410  continue
      write(stdout,*)
c
c-----------------------------------------------------------------------
c     compute depths and reciprocal depths
c-----------------------------------------------------------------------
c
      do 1400 j=1,jmt
      do 1390 i=1,imt
        h(i,j)  = c0
        hr(i,j) = c0
        if (kmu(i,j) .ne. 0) then
          h (i,j) = zw(kmu(i,j))
          hr(i,j) = c1/zw(kmu(i,j))
        endif
1390  continue
1400  continue
c
c-----------------------------------------------------------------------
c     initialize various things
c-----------------------------------------------------------------------
c
      do 2600 j=1,jmt
      do 2590 i=1,imt
        zu(i,j)  = c0
        zv(i,j)  = c0
2590  continue
2600  continue
c
c     initialize the coriolis factor
c
      do 2660 j=1,jmt
        fcor(j) = c2*omega*sine(j)
2660  continue
c
c     initialize the diffusion factors
c
      do 2670 j=jcbeg,jcend
        bbt(j) = ah*dxr*cstr(j)*dy
        cct(j) = ah*dyr*dx*csu(j  )
        ddt(j) = ah*dyr*dx*csu(j-1)
        bbu(j) = am*dxr*csur(j)*dy
        ccu(j) = am*dyr*dx*cst(j+1)
        ddu(j) = am*dyr*dx*cst(j  )
        ggu(j) = am*(c1-tng(j)*tng(j))/(radius*radius)
        hhu(j) = am*c2*sine(j)/(radius*csu(j)*csu(j))
      
2670  continue
c
c-----------------------------------------------------------------------
c     do all consistency checks now
c-----------------------------------------------------------------------
c
      call checks
c
c-----------------------------------------------------------------------
c     start the time step loop
c-----------------------------------------------------------------------
c
      do 3400 loop=1,99999999
c     write(stdout,*) " loop = ",loop
        first= loop .eq. 1

#ifdef oasis
        if (accelrun) then

        if (abs(totday-(start+(sync*counter))).lt.1e-2) then
        startaccel=.true.
        doaccel=.true.
        counter=counter+(async/sync)+1
        end if

      if (counter.gt.1.and.abs(mod((totday-start),(sync+async) ))
     $                          .lt.1e-2)  then
        stopaccel=.true.
        doaccel=.false.
        end if

        if (startaccel) write(99,*) 'startaccel',totday
        if (stopaccel) write(99,*) 'stopaccel',totday
        startaccel=.false.
        stopaccel=.false.


        if (doaccel) then
          dtts=accts
          dtuv=accuv
          dtbt=accbt
        else
          dtts=normts
          dtuv=normuv
          dtbt=normbt
        end if

        end if

c IF coupling timestep, get atmospheric fields from Oasis
c
      IF (MOD(totday,1.).lt.1e-4) THEN
            CALL flx(nint(totday-ifirstday)*nexco, imt-2*nhalo,
     $       jmt-2*(nhalo-1), oheatlat,oswlw, owater,osensible)

            CALL tau(nint(totday-ifirstday)*nexco, imt-2*nhalo,
     $       jmt-2*(nhalo-1), oztaux, oztauy, oztaux2, oztauy2)
      ENDIF

      do i=1,imt-2*nhalo
      do j=1,jmt-2*(nhalo-1)
         heatlat(i+nhalo,j+nhalo-1)=oheatlat(i,j)
         swlw(i+nhalo,j+nhalo-1)=oswlw(i,j)
         sensible(i+nhalo,j+nhalo-1)=osensible(i,j)
         water(i+nhalo,j+nhalo-1)=owater(i,j)
         ztaux(i+nhalo,j+nhalo-1)=oztaux(i,j)
         ztauy(i+nhalo,j+nhalo-1)=oztauy(i,j)
      end do
      end do
#endif
c
c-----------------------------------------------------------------------
c       update timestep, set time dependent logical switches
c       to determine program flow for timestep itt, itt-1 and itt-2.
c       note: timestep itt and time refer to the time of the new
c       fields being calculated!
c-----------------------------------------------------------------------
c
        call tmngr (dtts)
        call step 
c
c-----------------------------------------------------------------------
c      print time step information, print diagnostics and archive 
c      as required
c-----------------------------------------------------------------------
c
        mytime = dtime(utime)
        utime(3)=utime(1)+utime(2)+utime(3)
        if (prntsi) then
          if(modulo(itt,32).eq.0) then
          write (stdout,9602) itt, stamp, ektot,
     &                      dtabs(1), dtabs(2)
          endif
          if(mom_isnan(dtabs(1)) .or.
     &         mom_isnan(dtabs(2)) .or.
     &         mom_isnan(ektot)) then
            write(stdout,*)'ERROR : Not-a-number (NaN) detected'
     &           //' - abandoning run'
            last=.true.
          endif
          call flush(stdout)
        endif
        if (diagts) call pdiag
        if (archts) call archwr
c-----------------------------------------------------
c  marc 10.10.2001 
c	if (streamts) call stream
c------------------------------------------------------
#ifdef average
c     Cumulate the time averaging fields
      call av_cumulate( dtts )
      if(snapts) then
        call av_normalise
        call snap_avg_netcdf
        call av_zero

c      units of heat budget are cm**3 s**(-1) degC
c      to convert to petawatts multiply by rho Cp and 
c      divide by 1.e15 and multipky by 1.e-6 to convert
c      cm**3 to m**3.
      write(budget_timestamp,"(i8.8)")nint(totday)
c     open(92,file="budget_diag."//budget_timestamp)
      m=1
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dddt(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dvtf(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dw(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dslpdif(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dadvn(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dadve(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dadvw(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dadvs(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2ddifn(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2ddife(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2ddifw(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2ddifs(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dbiharmn(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dbiharme(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dbiharmw(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dbiharms(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dbiharmup(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2ddflux(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     do j=1,jmt
c     do i=1,imt
c     write(92,"(e16.8)")s2dall(i,j,m)
c    1   /real(nsum)
c     end do
c     end do
c     write(92,*)nsum
c     close(92)

      nsum=0
      do m=1,nt
      do j=1,jmt
      do i=1,imt
      s2dddt(i,j,m)=c0
      s2dvtf(i,j,m)=c0
      s2dw(i,j,m)=c0
      s2dslpdif(i,j,m)=c0
      s2dadvn(i,j,m)=c0
      s2dadve(i,j,m)=c0
      s2dadvw(i,j,m)=c0
      s2dadvs(i,j,m)=c0
      s2ddifn(i,j,m)=c0
      s2ddife(i,j,m)=c0
      s2ddifw(i,j,m)=c0
      s2ddifs(i,j,m)=c0
      s2dbiharmn(i,j,m)=c0
      s2dbiharme(i,j,m)=c0
      s2dbiharmw(i,j,m)=c0
      s2dbiharms(i,j,m)=c0
      s2dbiharmup(i,j,m)=c0
      s2ddflux(i,j,m)=c0
      end do
      end do
      end do

      endif

#else
      if (snapts) call snap
#endif

c
c-----------------------------------------------------------------------
c       end timestepping loop
c-----------------------------------------------------------------------
c
#ifdef oasis

      DO ji = 1, imt
          DO jj = 1, jmt
c accumulate temperatures to form an average for passing to Oasis
          sstoc(ji,jj)=sstoc(ji,jj)+t(1,ji,jj,1,3)/(86400/dtts)
c accumulate surface currents to form an average for passing to Oasis 
c                                                (convert from cm/s to m/s )
          zscoc(ji,jj)=zscoc(ji,jj)+u(1,ji,jj,3)*.01/(86400/dtts)
          mscoc(ji,jj)=mscoc(ji,jj)+v(1,ji,jj,3)*.01/(86400/dtts)
          ENDDO
      ENDDO

c IF coupling timestep, give oceanic fields to Oasis
c
C      IF (mod(totday,1.).lt.1e-4.OR.(loop.eq.1)) THEN
C Change to avoid offset in mixing leapfrong timestep in restart runs
      IF (mod(totday,1.).lt.1e-4) THEN


c read in sst from file and replace that which is about to be passed to OASIS
c
#ifdef sst_rd
      call snap_sst_rd_netcdf
c     write(stdout,*)'sstocr1=',sstoc(90,44)
#endif /*sst_rd*/

c save out the sst which is about to be passed to OASIS
c
#ifdef sst_wr
c     write(stdout,*)'sstoc1=',sstoc(90,44)
      call snap_sst_wr_netcdf
#endif /*sst_wr*/

      do jj=1,jmt-2*(nhalo-1)
        do ji=1,imt-2*nhalo
          osstoc(ji,jj)=sstoc(ji+nhalo,jj+nhalo-1)
          ozscoc(ji,jj)=zscoc(ji+nhalo,jj+nhalo-1)
          omscoc(ji,jj)=mscoc(ji+nhalo,jj+nhalo-1)
        end do
      end do

       if (loop.eq.1) then
           CALL stpcmo(0,imt-2*nhalo, jmt-2*(nhalo-1)
     $               , osstoc, ozscoc, omscoc, archts, last)
       else
           CALL stpcmo(nint(totday-ifirstday)*nexco,imt-2*nhalo
     $   , jmt-2*(nhalo-1),osstoc ,ozscoc, omscoc, archts, last)
       end if

c
c   reset daily sst array
c
c      write(6,*) 'clearing exchange array'
       do ji=1,imt
       do jj=1,jmt
        sstoc(ji,jj)=0.
        zscoc(ji,jj)=0.
        mscoc(ji,jj)=0.
       end do
       end do

      END IF
#endif

      if (last) go to 3401
3400  continue
3401  continue
c     i=90
c     j=44
c     m=1
c     write(91,"(4e16.8)")s2dddt(i,j,m),s2dvtf(i,j,m),s2dw(i,j,m),
c    1   s2dslpdif(i,j,m),stf(1)
c     write(91,"(4e16.8)")s2dadvn(i,j,m),s2dadve(i,j,m),
c    1   s2dadvw(i,j,m),s2dadvs(i,j,m)
c     write(91,"(4e16.8)")s2ddifn(i,j,m),s2ddife(i,j,m),s2ddifw(i,j,m),
c    1   s2ddifs(i,j,m)
c     write(91,"(4e16.8)")s2dbiharmn(i,j,m),s2dbiharme(i,j,m),
c    1   s2dbiharmw(i,j,m),s2dbiharms(i,j,m)
c     write(91,"(4e16.8)")s2dbiharmup(i,j,m),s2ddflux(i,j,m)
c     s2dall(i,j,m)=s2dvtf(i,j,m)+s2dw(i,j,m)+s2dslpdif(i,j,m)+
c    1   s2dadvn(i,j,m)+s2dadve(i,j,m)+s2dadvw(i,j,m)+s2dadvs(i,j,m)+
c    1   s2ddifn(i,j,m)+s2ddife(i,j,m)+s2ddifw(i,j,m)+s2ddifs(i,j,m)+
c    1   s2dbiharmn(i,j,m)+s2dbiharme(i,j,m)+s2dbiharmw(i,j,m)+
c    1   s2dbiharms(i,j,m)+
c    1   s2dbiharmup(i,j,m)+s2ddflux(i,j,m)
c     write(91,*)" sumall = ",s2dall(i,j,m),"nsum = ",nsum
c
c-----------------------------------------------------------------------
c     documentation info at end of run
c-----------------------------------------------------------------------
c
      call docmnt (stdout, .true., .true., .true., .true.)
c
c-----------------------------------------------------------------------
c     save restart dataset at end of run if restrt is true
c-----------------------------------------------------------------------
c
      if (restrt) call archwr
c
c-----------------------------------------------------------------------
c     close all units  (stderr is not closed because it is set equal
c     to stdout in pconst.h).
c-----------------------------------------------------------------------
c
      write (stdout,9603) 
      close (unit=stdout,status='keep')
      stop
c
 301  format (//,'  Regional & Global ocean statistics:'
     &,/,'  the total ocean surface area (t grid) =',1pe15.8,'cm**2'
     &,/,'  the total ocean volume (t grid)       =',1pe15.8,'cm**3')
 401  format(/t50,'number of levels on "t,s" grid')
9451  format (/' ==== start and end indices for',a17,'====')
9461  format (' j=',i3,5x,5(2i5,10x))
9499  format (/' error => lseg too small for',a15,' indices'
     &        /'          j =',i5,'  lseg + 1 =',i8)
9602  format (1x,'ts=',i10, 1x, a32, ', ke=', 1pe13.6,
     &        ' dtemp=',1pe13.6,' dsalt=',1pe13.6)
9603  format(1x,' ==> END of model run.',//)
      end


      logical function mom_isnan( f )
c Test whether f is Not-a-number or inf
c Uses the ieee nan representation which is of the form 7fxxxxxx (hex)
c Where xxxxxx is any hex value apart from 000000. This special case is 
c for Inf.
c  +/-Nan or +/-Inf are represented as all the exponent bits =1
      implicit none
c Test value      
      _MOMA_REAL f
c local:
#ifdef REAL_8      
      real*8 fval
      integer*8 ival
c     ieee nan = 7ffxxxxxxxxxxxxx
      integer*8 ieeenan
      integer*8 i64
      parameter(i64=1)
c The following line gives a number of 7ff0000000000000 (hex),
c computed using integer*8
      parameter(ieeenan = i64*32752*65536*65536*65536)
#else 
      real*4 fval
      integer*4 ival
c     ieee nan = h7fxxxxxx
      integer*4 ieeenan
      parameter(ieeenan = 2130706432)
#endif
      equivalence(ival,fval)
       
      fval = f

      mom_isnan=(iand(ival,ieeenan).eq.ieeenan)  !Check exponent bits
      end ! function mom_isnan 
      
