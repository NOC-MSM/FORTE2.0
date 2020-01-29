#include "cpp_options.h"
      subroutine step 
      implicit none
c
c=======================================================================
c
c     step is called once per timestep. it includes all the main 
c     loops over ic and jc and calls to the main routines.
c
c=======================================================================
c
c
#include "param.h"
c
#include "scalar.h"
#include "switch.h"
#include "ctmngr.h"
#include "timelv.h"
#include "slabs.h"
#include "frees.h"
c
#include "cdiag.h"
#ifdef inline
#define inline_step
#include "grdvar.h"
#include "levind.h"
#endif

c Local:
      integer nnc,nnm,nnp,jc,ic,ntbtp0,nnc0,nnm0,nnp0,n
#ifndef free_eb
      integer m
#endif
#ifdef cyclic
      integer k
#endif

#ifdef inline
      integer ip,im,jp,jm
      _MOMA_REAL boxar,dhdt,fxa,fxb,temp1,temp2,dpdx0,dpdy0
#ifdef free_eb
     &     ,fac1,fac2,ustar,vstar
#else
     &     ,fx
#endif
#ifdef de_checkbd
     &     ,delplus,delcross
      integer m_n,m_s,m_e,m_w,m_ne,m_se,m_sw,m_nw
#endif
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

c 
c-----------------------------------------------------------------------
c    update pointers for new value of itt.  
c    nnp, nnc and nnm are not changed during a timestep
c    np, nc, nm may be modified during a forward or euler backward 
c    timestep.
c-----------------------------------------------------------------------
c
      nnc = np
      nnm = nc
      nnp = nm
      np  = nnp
      nc  = nnc
      nm  = nnm
c
c-----------------------------------------------------------------------
c       adjust various quantities for normal/mixing timesteps
c-----------------------------------------------------------------------
c
      mxpas2 = .false.
      eots   = .true.
      if (mixts) then
        if (eb) eots = .false.
        nm = nnc
        c2dtts = dtts
        c2dtuv = dtuv
      else
        c2dtts = c2*dtts
        c2dtuv = c2*dtuv
      endif
#ifdef NONLIN_FREE_SURFACE
c
c New code added from occam source for free surface in addv
c
c--------------------------------------------------------------- 
c   store free surface height at end of previous baroclinic      
c   timestep for use in tracer                                   
c---------------------------------------------------------------
      do jc = 1,jmt                                             
       do ic = 1,imt                                            
        if (mixts) then                                         
          h00(ic,jc,1) = h0(ic,jc,np0)                          
        else                                                    
          h00(ic,jc,1) = h00(ic,jc,2)                           
        endif                                                   
        h00(ic,jc,2) = h0(ic,jc,np0)                            
       enddo
      enddo
#endif
c
c-----------------------------------------------------------------------
c       return here for second pass of euler backward timestep
c-----------------------------------------------------------------------
c
 100  continue
#ifdef presetp
c
c-----------------------------------------------------------------------
c  
c     precalculate the baroclinic part of the pressure field 
c     for use by subroutine clinic
c
c-----------------------------------------------------------------------
c  
      do jc=1,jmt
       do ic=1,imt
        call setp (ic,jc)
       enddo
      enddo
#endif
c
c-----------------------------------------------------------------------
c      main baroclinic timestep loop over grid cells, from south 
c      to north and from west to east
c      1. set vertical boundary conditions (surface & bottom)
c      2. calculate internal mode velocities
c      3. calculate tracers
c-----------------------------------------------------------------------
c

      nsum=nsum+1
c     write(91,*) " in step.F ",nsum

#ifdef cmip_flxrd
      IF (mod(totday,1.).lt.1e-4) THEN
      write(91,*) " calling snap_flx_read ",nsum
      call snap_flxrd_netcdf
      endif
#endif

C$OMP PARALLEL DO PRIVATE( ic , jc ) , SCHEDULE(DYNAMIC, jmt/4)
      do jc=jcbeg,jcend
       do ic=icbeg,icend
        call setvbc (ic,jc)
        call clinic (ic,jc)
#ifdef isopycmix
c       compute isopycnal diffusive fluxes through east, north,
c       and bottom faces of T cells.
        call isopyc (ic,jc)
#endif
        call tracer (ic,jc)
       enddo
      enddo

C$OMP END PARALLEL DO

#ifdef cmip_flx
      IF (mod(totday,1.).lt.1e-4) THEN
      write(91,*) " calling snap_flx ",nsum
      call snap_flx_netcdf
      endif
#endif

c
c-----------------------------------------------------------------------
c      run free surface model (except during the second part
c      of a baroclinic euler backwards timestep).
c      first initialise pointers 
c-----------------------------------------------------------------------
c
      if(.not.mxpas2)then
        ntbtp0 = 0
        c2dtbt = dtbt
#ifdef free_eb
        ntbt2 = ntbt
#else
        ntbt2 = 2*ntbt
        do jc=jcbeg,jcend
         do ic=icbeg,icend
          do m=1,3
          freeav(m,ic,jc)=c0
         enddo
        enddo
       enddo
#endif
        do 600 itbt=1,ntbt2
          nnc0 = np0
          nnm0 = nc0
          nnp0 = nm0
          np0  = nnp0
          nc0  = nnc0
#ifdef free_eb
          nm0 = nnc0
#else
          if(itbt.eq.1)then
            nm0 = nnc0
          else
            nm0 = nnm0
          endif
#endif
c
c-----------------------------------------------------------------------
c      On the first barotropic timestep or if "free_eb" is set
c      use euler backward. this requires two passes.
c-----------------------------------------------------------------------
c
          do 500 itbtp=ntbtp0,1
c
c-----------------------------------------------------------------------
c      main free surface model loop to carry out a
c      partial timestep for each model point
c-----------------------------------------------------------------------
c
C$OMP PARALLEL DO PRIVATE( ic , jc ) , SCHEDULE(DYNAMIC, jmt/4)
          do jc=jcbeg,jcend
           do ic=icbeg,icend
#ifdef inline
#include "frees.F"
#else
            call frees(ic,jc)
#endif
           enddo
          enddo
C$OMP END PARALLEL DO
c
c-----------------------------------------------------------------------
c  set boundary conditions for the free surface model
c-----------------------------------------------------------------------
c
#ifdef cyclic
          do jc=jcbeg,jcend
           do ic=1,nhalo
            h0(ic,jc,np0) = h0(imtm2h+ic,jc,np0)
            u0(ic,jc,np0) = u0(imtm2h+ic,jc,np0)
            v0(ic,jc,np0) = v0(imtm2h+ic,jc,np0)
           end do
          end do
          do jc=jcbeg,jcend
           do ic=1,nhalo
            h0(imtm1h+ic,jc,np0) = h0(nhalo+ic,jc,np0)
            u0(imtm1h+ic,jc,np0) = u0(nhalo+ic,jc,np0)
            v0(imtm1h+ic,jc,np0) = v0(nhalo+ic,jc,np0)
           enddo
          enddo
#endif
c
c-----------------------------------------------------------------------
c     reset pointers at end of first pass of the free surface model
c-----------------------------------------------------------------------
c
#ifdef free_eb
          if(itbtp.eq.0)then
            nc0 = nnp0
            np0 = nnm0
          else
            nc0 = nnc0
            nm0 = nnp0
          endif
#else
          if(itbt.eq.1)then
            if(itbtp.eq.0)then
              nc0 = nnp0
              np0 = nnm0
            else
c jods: These next two lines do not seem to be needed, in fact they seem 
c to permute the time steps wrongly.
              nc0 = nnc0
              nm0 = nnp0
              ntbtp0 = 1
              c2dtbt = c2*dtbt
            endif
          endif
#endif

 500     continue               !End of itbtp loop
 600    continue
      endif
c
c-----------------------------------------------------------------------
c  end of free surface model
c  now add barotropic velocities to baroclinic velocities
c-----------------------------------------------------------------------
c
      do jc=jcbeg,jcend
       do ic=icbeg,icend
        call addv(ic,jc)
       enddo
      enddo
c     
c-----------------------------------------------------------------------
c  set boundary conditions for the baroclinic model
c-----------------------------------------------------------------------
c
#ifdef cyclic
          do jc=jcbeg,jcend
            do ic=1,nhalo
              do k=1,km
                u(k,ic,jc,np) = u(k,imtm2h+ic,jc,np)
                v(k,ic,jc,np) = v(k,imtm2h+ic,jc,np)
                u(k,imtm1h+ic,jc,np) = u(k,nhalo+ic,jc,np)
                v(k,imtm1h+ic,jc,np) = v(k,nhalo+ic,jc,np)
                do n=1,nt
                  t(k,ic,jc,n,np) = t(k,imtm2h+ic,jc,n,np)
                  t(k,imtm1h+ic,jc,n,np) = t(k,nhalo+ic,jc,n,np)
                end do
              enddo
            enddo
          enddo
#endif
c
c-----------------------------------------------------------------------
c     if this is the end of the first pass of an euler backward
c     timestep then set the pointers for the second pass.
c-----------------------------------------------------------------------
c
      if (mixts.and.eb)then
        eots   = .true.
        nc = nnp   !nm   
        np = nnm   !nc   and nm is still nc.
        mixts  = .false.
        mxpas2 = .true.
        go to 100
      endif
c
c-----------------------------------------------------------------------
c  collect timestep statistics
c-----------------------------------------------------------------------
c
      if(prntsi.and.eots) then
        ektot = c0
        do n=1,nt
         tddt(n)  = c0
         dtabs(n) = c0
         tvar(n)  = c0
        enddo
        do jc=jcbeg,jcend
         do ic=icbeg,icend
          call diag(ic,jc)
         enddo
        enddo
        ektot = ektot/volume
        do n=1,nt
         tddt(n)  = tddt(n)/volume
         dtabs(n) = dtabs(n)/volume
         tvar(n)  = tvar(n)/volume
        enddo
      endif
c
c-----------------------------------------------------------------------
c       if this is the end of either a forward or euler backward
c       timestep then reset the pointers
c-----------------------------------------------------------------------
c
      if(mxpas2)then
        nc = nnc
        nm = nnp
      endif
      if(mixts)then
        nm = nnm
      endif
c
      return
      end
