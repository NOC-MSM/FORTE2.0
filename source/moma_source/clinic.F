#include "cpp_options.h"
      subroutine clinic(ic,jc)
      implicit none
c
c=======================================================================
c
c     clinic computes internal mode velocities at time level t+1
c     for a vertical column of k cells with i and j values equal to
c     ic and jc.
c
c=======================================================================
c
c
#include "param.h"
c
#include "scalar.h"
#include "switch.h"
#include "coord.h"
#include "grdvar.h"
#include "levind.h"
#include "timelv.h"
#include "slabs.h"
#include "frees.h"
c
#include "chmix.h"
#include "cvbc.h"
#include "cvmix.h"
c
c In...
      integer ic,jc
c Local 
      integer im,ip,jm,jp,kmc,k,lcor
      _MOMA_REAL boxa, boxar,dxp5,dyp5
      _MOMA_REAL fvjc,fuic,fxa,fxb,temp1,temp2
      _MOMA_REAL ucor,urest,vcor,vrest,fx,detmr,t1,t2,sumu,sumv !,zwr
      
      _MOMA_REAL hn,hs,he,hw,dzk,dzkr,zwkmc,zwkr
#ifdef msq
      integer ipp,imm,jpp,jmm
#endif

      kmc=kmu(ic,jc)
      if(kmc.eq.0)return
      jp = jc+1
      jm = jc-1
      ip = ic+1
      im = ic-1
c
      boxa = dx*csu(jc)*dy
      boxar = c1/boxa
c
c=======================================================================
c  calculate horizontal and vertical advective fluxes
c=======================================================================
c
      dxp5 = p5*dx
      dyp5 = p5*dy

#ifdef msq
c
c=======================================================================
c    set up masks for the modified split-quick scheme.
c=======================================================================
c
      jpp = jp+1
      jmm = jm-1
      ipp = ip+1
      imm = im-1
c
      do k=1,kmc
        rmsknn(k) = c0
        rmsknd(k) = c0
        if(k.le.kmu(ic,jp)) then
          rmsknn(k) = r12
          rmsknd(k) = r16    
        endif
        rmskee(k) = c0
        rmsked(k) = c0
        if(k.le.kmu(ip,jc)) then
          rmskee(k) = r12
          rmsked(k) = r16    
        endif
        rmskss(k) = c0
        rmsksd(k) = c0
        if(k.le.kmu(ic,jm)) then
          rmskss(k) = r12
          rmsksd(k) = r16    
        endif
        rmskww(k) = c0
        rmskwd(k) = c0
        if(k.le.kmu(im,jc)) then
          rmskww(k) = r12
          rmskwd(k) = r16    
        endif
      enddo
#endif /*msq*/

c     jods: Deal with top box first
#ifndef NONLIN_FREE_SURFACE
      hn = dz(1)
      hs = dz(1)
      he = dz(1)
      hw = dz(1)
#else /* NONLIN_FREE_SURFACE */   
      hn = dz(1) + p5*(h00(ic,jp,2)+h00(ip,jp,2)) 
      hs = dz(1) + p5*(h00(ic,jc,2)+h00(ip,jc,2)) 
      he = dz(1) + p5*(h00(ip,jp,2)+h00(ip,jc,2)) 
      hw = dz(1) + p5*(h00(ic,jc,2)+h00(ic,jp,2)) 
#endif /* NONLIN_FREE_SURFACE */

#ifdef oldadv
      fvn(1) = (v(1,ic,jc,nc)+v(1,ic,jp,nc))*p5*dx*cst(jp)*hn
      fvs(1) = (v(1,ic,jc,nc)+v(1,ic,jm,nc))*p5*dx*cst(jc)*hs
      fue(1) = (u(1,ic,jc,nc)+u(1,ip,jc,nc))*p5*dy*he
      fuw(1) = (u(1,ic,jc,nc)+u(1,im,jc,nc))*p5*dy*hw    
#else
      fvjc   = csu(jc)*
     &         (v(1,im,jc,nc)+c2*v(1,ic,jc,nc)+v(1,ip,jc,nc))
      fuic   = (u(1,ic,jp,nc)+c2*u(1,ic,jc,nc)+u(1,ic,jm,nc))
c
      fvn(1) = hn*dx*p125*( fvjc + csu(jp)*
     &         (v(1,im,jp,nc)+c2*v(1,ic,jp,nc)+v(1,ip,jp,nc)))
      fvs(1) = hs*dx*p125*( fvjc + csu(jm)*
     &         (v(1,im,jm,nc)+c2*v(1,ic,jm,nc)+v(1,ip,jm,nc)))
c
      fue(1) = he*dy*p125*( fuic +
     &         (u(1,ip,jp,nc)+c2*u(1,ip,jc,nc)+u(1,ip,jm,nc)))
      fuw(1) = hw*dy*p125*( fuic +
     &         (u(1,im,jp,nc)+c2*u(1,im,jc,nc)+u(1,im,jm,nc)))
#endif
      fw(0)  = (fuw(1)-fue(1)+fvs(1)-fvn(1))

c     Deal with sub-surface boxes
      do 100 k=2,kmd(ic,jc)
#ifdef oldadv
c
c  original horizontal advection scheme for momentum
c
      fvn(k) = (v(k,ic,jc,nc)+v(k,ic,jp,nc))*p5*dx*cst(jp)*dz(k)
      fvs(k) = (v(k,ic,jc,nc)+v(k,ic,jm,nc))*p5*dx*cst(jc)*dz(k)
      fue(k) = (u(k,ic,jc,nc)+u(k,ip,jc,nc))*p5*dy*dz(k)
      fuw(k) = (u(k,ic,jc,nc)+u(k,im,jc,nc))*p5*dy*dz(k)
#else
c
c  new horizontal advection scheme for momentum
c
      fvjc   = csu(jc)*
     &         (v(k,im,jc,nc)+c2*v(k,ic,jc,nc)+v(k,ip,jc,nc))
      fuic   = (u(k,ic,jp,nc)+c2*u(k,ic,jc,nc)+u(k,ic,jm,nc))
c
      fvn(k) = dz(k)*dx*p125*( fvjc + csu(jp)*
     &         (v(k,im,jp,nc)+c2*v(k,ic,jp,nc)+v(k,ip,jp,nc)))
      fvs(k) = dz(k)*dx*p125*( fvjc + csu(jm)*
     &         (v(k,im,jm,nc)+c2*v(k,ic,jm,nc)+v(k,ip,jm,nc)))
c
      fue(k) = dz(k)*dy*p125*( fuic +
     &         (u(k,ip,jp,nc)+c2*u(k,ip,jc,nc)+u(k,ip,jm,nc)))
      fuw(k) = dz(k)*dy*p125*( fuic +
     &         (u(k,im,jp,nc)+c2*u(k,im,jc,nc)+u(k,im,jm,nc)))
#endif
c
c  new vertical boundary conditions on w at u points
c
      fw(k-1)  = (fuw(k)-fue(k)+fvs(k)-fvn(k))
 100  continue
c
c=======================================================================
c    integrate up from the bottom level
c=======================================================================
c
      fw(kmd(ic,jc))=c0
      do 210 k=kmd(ic,jc),1,-1
        fw(k-1) = fw(k-1)+fw(k)
 210  continue
#ifdef presetp
c
c-----------------------------------------------------------------------
c         compute hydrostatic pressure gradient (baroclinic part)
c-----------------------------------------------------------------------
c
      fxa = dx2r*csur(jc)
      fxb = dy2r
      do 300 k=1,kmc
        temp1  = p(k,ip,jp) - p(k,ic,jc)
        temp2  = p(k,ic,jp) - p(k,ip,jc)
        dpdx(k) = (temp1-temp2)*fxa
        dpdy(k) = (temp1+temp2)*fxb
 300  continue
#else
c
c-----------------------------------------------------------------------
c     compute density on surrounding 't' points
c-----------------------------------------------------------------------
c
      call state (t(1,ic,jc,1,nc), t(1,ic,jc,2,nc), rhoo, 
     &            kmt(ic,jc) ,0)
      call state (t(1,ip,jc,1,nc), t(1,ip,jc,2,nc), rhpo, 
     &            kmt(ip,jc) ,0)
      call state (t(1,ip,jp,1,nc), t(1,ip,jp,2,nc), rhpp, 
     &            kmt(ip,jp) ,0)
      call state (t(1,ic,jp,1,nc), t(1,ic,jp,2,nc), rhop, 
     &            kmt(ic,jp) ,0)
c
c-----------------------------------------------------------------------
c         compute hydrostatic pressure gradient (baroclinic part)
c-----------------------------------------------------------------------
c
c    1. compute it at the first level
c
      fxa = grav*dzw(0)*dx2r*csur(jc)
      fxb = grav*dzw(0)*dy2r
      temp1  = rhpp(1) - rhoo(1)
      temp2  = rhop(1) - rhpo(1)
      dpdx(1) = (temp1-temp2)*fxa
      dpdy(1) = (temp1+temp2)*fxb
c
c    2. compute the change in pressure gradient between levels
c
      fxa = grav*dx4r*csur(jc)
      fxb = grav*dy4r
      do 300 k=2,kmc
        temp1 = rhpp(k)+rhpp(k-1)- (rhoo(k)+rhoo(k-1))
        temp2 = rhop(k)+rhop(k-1)- (rhpo(k)+rhpo(k-1))
        dpdx(k) = (temp1-temp2)*fxa*dzw(k-1)
        dpdy(k) = (temp1+temp2)*fxb*dzw(k-1)
 300  continue
c
c=======================================================================
c         integrate downward from the first level
c=======================================================================
c
      do 400 k=2,kmc
        dpdx(k) = dpdx(k-1) + dpdx(k)
        dpdy(k) = dpdy(k-1) + dpdy(k)
 400  continue
#endif
c
c-----------------------------------------------------------------------
c         calculate quantities for the computation of
c         vertical diffusion and advection of momentum
c         - positive for upwards flux of momentum
c-----------------------------------------------------------------------
c
      do 500 k=1,kmc-1
        vmf(k,1) = fkpm*(u(k+1,ic,jc,nm) - u(k  ,ic,jc,nm))
     &                     *dzwr(k)*dx*csu(jc)*dy
        vmf(k,2) = fkpm*(v(k+1,ic,jc,nm) - v(k  ,ic,jc,nm))
     &                     *dzwr(k)*dx*csu(jc)*dy
        fwb1(k) = fw(k)*(u(k,ic,jc,nc) + u(k+1,ic,jc,nc))*p5
        fwb2(k) = fw(k)*(v(k,ic,jc,nc) + v(k+1,ic,jc,nc))*p5
c      enhanced vertical viscosity at the equator
c     if(jc.ge.43 .and. jc.le.46)vmf(k,1)=2.*vmf(k,1)
c     if(jc.ge.43 .and. jc.le.46)vmf(k,2)=2.*vmf(k,2)
c---
 500  continue
c
c-----------------------------------------------------------------------
c     set k=0 vertical diffusion to reflect surface wind stress
c     set level "kmc" for bottom drag condition.
c     set k=kmc elements of vertical advection array to zero
c     vertical momentum transfer.  Set k=0 to represent advection
c     loss to region above the free surface.
c     smf is the surface stress acting on the ocean
c     bmf is the bottom stress acting on the ocean floor
c-----------------------------------------------------------------------
c
      vmf(0,1)  = -smf(1)*boxa
      vmf(0,2)  = -smf(2)*boxa
      vmf(kmc,1)= -bmf(1)*boxa
      vmf(kmc,2)= -bmf(2)*boxa
#ifdef OLD_NON_CONSERVING
      fwb1(0)   = fw(0)*u(1,ic,jc,nc)
      fwb2(0)   = fw(0)*v(1,ic,jc,nc)
#else
      fwb1(0)   = c0            !jods fw(0)*u(1,ic,jc,nc)
      fwb2(0)   = c0            !jods fw(0)*v(1,ic,jc,nc)
#endif
      fwb1(kmc) = c0
      fwb2(kmc) = c0
c
c-----------------------------------------------------------------------
c       set up index lcor for the coriolis term:
c       use tau timestep for explicit trtmnt ... or
c       use tau-1 timestep for implicit treatment with remainder of
c       term to be added later
c-----------------------------------------------------------------------
c
          if (acor .eq. c0) then
            lcor = nc
          else
            lcor = nm
          endif
c
c-----------------------------------------------------------------------
c       initialise vertical mean forcing array elements:
c-----------------------------------------------------------------------
c
      zu(ic,jc) = c0
      zv(ic,jc) = c0
c
c=======================================================================
c  calculate thickness of top box at beginning of timestep              
c=======================================================================
c                                                                       
#ifndef NONLIN_FREE_SURFACE
      hn = dz(1)
      hs = dz(1)
      he = dz(1)
      hw = dz(1)
      dzk   = dz(1)
      dzkr  = dzr(1)
      zwkmc = zw(kmc)
      zwkr  = c1/zwkmc
#else /* NONLIN_FREE_SURFACE */   
      hn = dz(1) + p5*(h00(ic,jp,1)+h00(ip,jp,1))                   
      hs = dz(1) + p5*(h00(ic,jc,1)+h00(ip,jc,1))                   
      he = dz(1) + p5*(h00(ip,jp,1)+h00(ip,jc,1))                   
      hw = dz(1) + p5*(h00(ic,jc,1)+h00(ic,jp,1))                   
      dzk   = dz(1)+p25*(                                           
     &        h00(ic,jp,1)+h00(ip,jp,1)+h00(ip,jc,1)+h00(ic,jc,1))   
      dzkr  = c1/dzk                                                 
      zwkmc = zw(kmc)+p25*(                                          
     &        h00(ic,jp,1)+h00(ip,jp,1)+h00(ip,jc,1)+h00(ic,jc,1))   
      zwkr  = c1/zwkmc                                               
#endif /* NONLIN_FREE_SURFACE */
c
c=======================================================================
c    main calculation to timestep baroclinic velocity
c=======================================================================
c
c jods : Level 1 first, to account for FSH
c
c u-momentum equation.  separate coriolis term and the rest.
c
      ucor  = fcor(jc)*v(1,ic,jc,lcor)
      urest = -dpdx(1)
#ifdef msq
c  u-advection                                                      
     &    +(fuw(1)*( (u(1,im, jc,nc) + u(1,ic,jc,nc))*(p5+rmskww(1))
     &             - (u(1,imm,jc,nc) + u(1,ip,jc,nc))*rmskww(1) )    
     &    - fue(1)*( (u(1,ip, jc,nc) + u(1,ic,jc,nc))*(p5+rmskee(1)) 
     &             - (u(1,ipp,jc,nc) + u(1,im,jc,nc))*rmskee(1) )    
c  v-advection                                                       
     &    + fvs(1)*( (u(1,ic,jm, nc) + u(1,ic,jc,nc))*(p5+rmskss(1)) 
     &             - (u(1,ic,jmm,nc) + u(1,ic,jp,nc))*rmskss(1) )    
     &    - fvn(1)*( (u(1,ic,jp, nc) + u(1,ic,jc,nc))*(p5+rmsknn(1)) 
     &             - (u(1,ic,jpp,nc) + u(1,ic,jm,nc))*rmsknn(1) )    
c  velocity dependent biharmonic difusion                             
     &    - abs(fuw(1))*(u(1,imm,jc,nm) - u(1,ip ,jc,nm)              
     &             - c3*(u(1,im, jc,nm) - u(1,ic ,jc,nm)))*rmskwd(1)  
     &    + abs(fue(1))*(u(1,im, jc,nm) - u(1,ipp,jc,nm)              
     &             - c3*(u(1,ic, jc,nm) - u(1,ip, jc,nm)))*rmsked(1)  
     &    - abs(fvs(1))*(u(1,ic,jmm,nm) - u(1,ic, jp,nm)              
     &             - c3*(u(1,ic, jm,nm) - u(1,ic, jc,nm)))*rmsksd(1)  
     &    + abs(fvn(1))*(u(1,ic, jm,nm) - u(1,ic,jpp,nm)              
     &             - c3*(u(1,ic, jc,nm) - u(1,ic, jp,nm)))*rmsknd(1)  
#else /*msq*/
c  u-advection
     & +(fuw(1)*(u(1,im,jc,nc) + u(1,ic,jc,nc))*p5
     & - fue(1)*(u(1,ip,jc,nc) + u(1,ic,jc,nc))*p5
c  v-advection
     & + fvs(1)*(u(1,ic,jm,nc) + u(1,ic,jc,nc))*p5
     & - fvn(1)*(u(1,ic,jp,nc) + u(1,ic,jc,nc))*p5
#endif /*msq*/
c  w-advection
     & + fwb1(1  ) - fwb1(1-1)
c  x diffusion  
     & + bbu(jc)*(u(1,ip,jc,nm)-u(1,ic,jc,nm))*he
     & + bbu(jc)*(u(1,im,jc,nm)-u(1,ic,jc,nm))*hw
c  y diffusion
     & + ccu(jc)*(u(1,ic,jp,nm)-u(1,ic,jc,nm))*hn
     & + ddu(jc)*(u(1,ic,jm,nm)-u(1,ic,jc,nm))*hs
c  z diffusion  
     &  + vmf(1  ,1) - vmf(0,1) )*boxar*dzkr
c
c v-momentum equation.  separate coriolis term and the rest
c
      vcor  = -fcor(jc)*u(1,ic,jc,lcor)
      vrest = -dpdy(1)
#ifdef msq
c  u-advection                                                           !  + freshwater_flux
     &    +(fuw(1)*( (v(1,im,jc,nc)  + v(1,ic,jc,nc))*(p5+rmskww(1))     !  + freshwater_flux
     &             - (v(1,imm,jc,nc) + v(1,ip,jc,nc))*rmskww(1) )        !  + freshwater_flux
     &    - fue(1)*( (v(1,ip,jc,nc)  + v(1,ic,jc,nc))*(p5+rmskee(1))     !  + freshwater_flux
     &             - (v(1,ipp,jc,nc) + v(1,im,jc,nc))*rmskee(1) )        !  + freshwater_flux
c  v-advection                                                           !  + freshwater_flux
     &    + fvs(1)*( (v(1,ic,jm,nc)  + v(1,ic,jc,nc))*(p5+rmskss(1))     !  + freshwater_flux
     &             - (v(1,ic,jmm,nc) + v(1,ic,jp,nc))*rmskss(1) )        !  + freshwater_flux
     &    - fvn(1)*( (v(1,ic,jp,nc)  + v(1,ic,jc,nc))*(p5+rmsknn(1))     !  + freshwater_flux
     &             - (v(1,ic,jpp,nc) + v(1,ic,jm,nc))*rmsknn(1) )        !  + freshwater_flux
c  velocity dependent biharmonic difusion                                !  + freshwater_flux
     &    - abs(fuw(1))*(v(1,imm,jc,nm) - v(1,ip, jc,nm)                 !  + freshwater_flux
     &             - c3*(v(1,im, jc,nm) - v(1,ic, jc,nm)))*rmskwd(1)     !  + freshwater_flux
     &    + abs(fue(1))*(v(1,im, jc,nm) - v(1,ipp,jc,nm)                 !  + freshwater_flux
     &             - c3*(v(1,ic, jc,nm) - v(1,ip, jc,nm)))*rmsked(1)     !  + freshwater_flux
     &    - abs(fvs(1))*(v(1,ic,jmm,nm) - v(1,ic, jp,nm)                 !  + freshwater_flux
     &             - c3*(v(1,ic, jm,nm) - v(1,ic, jc,nm)))*rmsksd(1)     !  + freshwater_flux
     &    + abs(fvn(1))*(v(1,ic, jm,nm) - v(1,ic,jpp,nm)                 !  + freshwater_flux
     &             - c3*(v(1,ic, jc,nm) - v(1,ic, jp,nm)))*rmsknd(1)     !  + freshwater_flux
#else /*msq*/
c  u-advection
     & +(fuw(1)*(v(1,im,jc,nc) + v(1,ic,jc,nc))*p5
     & - fue(1)*(v(1,ip,jc,nc) + v(1,ic,jc,nc))*p5
c  v-advection
     & + fvs(1)*(v(1,ic,jm,nc) + v(1,ic,jc,nc))*p5
     & - fvn(1)*(v(1,ic,jp,nc) + v(1,ic,jc,nc))*p5
#endif /*msq*/
c  w-advection
     & + fwb2(1  ) - fwb2(1-1)
c  x diffusion  
     & + bbu(jc)*(v(1,ip,jc,nm)-v(1,ic,jc,nm))*he
     & + bbu(jc)*(v(1,im,jc,nm)-v(1,ic,jc,nm))*hw
c  y diffusion
     & + ccu(jc)*(v(1,ic,jp,nm)-v(1,ic,jc,nm))*hn
     & + ddu(jc)*(v(1,ic,jm,nm)-v(1,ic,jc,nm))*hs 
c  z diffusion
     & + vmf(1  ,2) - vmf(0 ,2) )*boxar*dzkr
c
c***********************************************************************
c  the corrections to the advection and diffusion terms that
c  allow for the curvature of the grid near the poles have been
c  left out of this version of the model
c***********************************************************************
c
c-----------------------------------------------------------------------
c     calculate vertically averaged forcing (without coriolis)
c     and total force at each level (with coriolis)
c-----------------------------------------------------------------------
c
      fx = dzk
      zu(ic,jc) = zu(ic,jc) + urest*fx
      zv(ic,jc) = zv(ic,jc) + vrest*fx
      u(1,ic,jc,np) = ucor + urest
      v(1,ic,jc,np) = vcor + vrest
c                                                                        
c----------------------------------------------------------------------- 
c   sub-surface boxes                                    
c----------------------------------------------------------------------- 
c                                                                    
      do 600 k=2,kmc
c
c u-momentum equation.  separate coriolis term and the rest.
c
      ucor  = fcor(jc)*v(k,ic,jc,lcor)
      urest = -dpdx(k)
#ifdef msq
c  u-advection
     &      +(fuw(k)*( (u(k,im, jc,nc) + u(k,ic,jc,nc))*(p5+rmskww(k))
     &               - (u(k,imm,jc,nc) + u(k,ip,jc,nc))*rmskww(k) )
     &      - fue(k)*( (u(k,ip, jc,nc) + u(k,ic,jc,nc))*(p5+rmskee(k))
     &               - (u(k,ipp,jc,nc) + u(k,im,jc,nc))*rmskee(k) )
c  v-advection
     &      + fvs(k)*( (u(k,ic,jm, nc) + u(k,ic,jc,nc))*(p5+rmskss(k))
     &               - (u(k,ic,jmm,nc) + u(k,ic,jp,nc))*rmskss(k) )
     &      - fvn(k)*( (u(k,ic,jp, nc) + u(k,ic,jc,nc))*(p5+rmsknn(k))
     &               - (u(k,ic,jpp,nc) + u(k,ic,jm,nc))*rmsknn(k) )
c  velocity dependent biharmonic difusion
     &      - abs(fuw(k))*(u(k,imm,jc,nm) - u(k,ip, jc,nm)
     &               - c3*(u(k,im, jc,nm) - u(k,ic, jc,nm)))*rmskwd(k)
     &      + abs(fue(k))*(u(k,im, jc,nm) - u(k,ipp,jc,nm)
     &               - c3*(u(k,ic, jc,nm) - u(k,ip, jc,nm)))*rmsked(k)
     &      - abs(fvs(k))*(u(k,ic,jmm,nm) - u(k,ic, jp,nm)
     &               - c3*(u(k,ic, jm,nm) - u(k,ic, jc,nm)))*rmsksd(k)
     &      + abs(fvn(k))*(u(k,ic, jm,nm) - u(k,ic,jpp,nm)
     &               - c3*(u(k,ic, jc,nm) - u(k,ic, jp,nm)))*rmsknd(k)
#else /*msq*/
c  u-advection
     & +(fuw(k)*(u(k,im,jc,nc) + u(k,ic,jc,nc))*p5
     & - fue(k)*(u(k,ip,jc,nc) + u(k,ic,jc,nc))*p5
c  v-advection
     & + fvs(k)*(u(k,ic,jm,nc) + u(k,ic,jc,nc))*p5
     & - fvn(k)*(u(k,ic,jp,nc) + u(k,ic,jc,nc))*p5
#endif /*msq*/
c  w-advection
     & + fwb1(k  ) - fwb1(k-1)
c  x diffusion  
     & +(bbu(jc)*(u(k,ip,jc,nm)-u(k,ic,jc,nm))
     & + bbu(jc)*(u(k,im,jc,nm)-u(k,ic,jc,nm))
c  y diffusion
     & + ccu(jc)*(u(k,ic,jp,nm)-u(k,ic,jc,nm))
     & + ddu(jc)*(u(k,ic,jm,nm)-u(k,ic,jc,nm)) )
     & *dz(k)
c  z diffusion  
     &  + vmf(k  ,1) - vmf(k-1,1) )*boxar*dzr(k)
c
c v-momentum equation.  separate coriolis term and the rest
c
      vcor  = -fcor(jc)*u(k,ic,jc,lcor)
      vrest = -dpdy(k)
#ifdef msq
c  u-advection
     &      +(fuw(k)*( (v(k,im, jc,nc) + v(k,ic,jc,nc))*(p5+rmskww(k))
     &               - (v(k,imm,jc,nc) + v(k,ip,jc,nc))*rmskww(k) )
     &      - fue(k)*( (v(k,ip, jc,nc) + v(k,ic,jc,nc))*(p5+rmskee(k))
     &               - (v(k,ipp,jc,nc) + v(k,im,jc,nc))*rmskee(k) )
c  v-advection
     &      + fvs(k)*( (v(k,ic, jm,nc) + v(k,ic,jc,nc))*(p5+rmskss(k))
     &               - (v(k,ic,jmm,nc) + v(k,ic,jp,nc))*rmskss(k) )
     &      - fvn(k)*( (v(k,ic, jp,nc) + v(k,ic,jc,nc))*(p5+rmsknn(k))
     &               - (v(k,ic,jpp,nc) + v(k,ic,jm,nc))*rmsknn(k) )
c  velocity dependent biharmonic diffusion
     &      - abs(fuw(k))*(v(k,imm,jc,nm) - v(k,ip, jc,nm)
     &               - c3*(v(k,im, jc,nm) - v(k,ic, jc,nm)))*rmskwd(k)
     &      + abs(fue(k))*(v(k,im, jc,nm) - v(k,ipp,jc,nm)
     &               - c3*(v(k,ic, jc,nm) - v(k,ip, jc,nm)))*rmsked(k)
     &      - abs(fvs(k))*(v(k,ic,jmm,nm) - v(k,ic, jp,nm)
     &               - c3*(v(k,ic, jm,nm) - v(k,ic, jc,nm)))*rmsksd(k)
     &      + abs(fvn(k))*(v(k,ic, jm,nm) - v(k,ic,jpp,nm)
     &               - c3*(v(k,ic, jc,nm) - v(k,ic, jp,nm)))*rmsknd(k)
#else /*msq*/
c  u-advection
     & +(fuw(k)*(v(k,im,jc,nc) + v(k,ic,jc,nc))*p5
     & - fue(k)*(v(k,ip,jc,nc) + v(k,ic,jc,nc))*p5
c  v-advection
     & + fvs(k)*(v(k,ic,jm,nc) + v(k,ic,jc,nc))*p5
     & - fvn(k)*(v(k,ic,jp,nc) + v(k,ic,jc,nc))*p5
#endif /*msq*/
c  w-advection
     & + fwb2(k  ) - fwb2(k-1)
c  x diffusion  
     & +(bbu(jc)*(v(k,ip,jc,nm)-v(k,ic,jc,nm))
     & + bbu(jc)*(v(k,im,jc,nm)-v(k,ic,jc,nm))
c  y diffusion
     & + ccu(jc)*(v(k,ic,jp,nm)-v(k,ic,jc,nm))
     & + ddu(jc)*(v(k,ic,jm,nm)-v(k,ic,jc,nm)) )
     & *dz(k)
c  z diffusion
     & + vmf(k  ,2) - vmf(k-1,2) )*boxar*dzr(k)
c
c***********************************************************************
c  the corrections to the advection and diffusion terms that
c  allow for the curvature of the grid near the poles have been
c  left out of this version of the model
c***********************************************************************
c
c-----------------------------------------------------------------------
c     calculate vertically averaged forcing (without coriolis)
c     and total force at each level (with coriolis)
c-----------------------------------------------------------------------
c
      fx = dz(k)
      zu(ic,jc) = zu(ic,jc) + urest*fx
      zv(ic,jc) = zv(ic,jc) + vrest*fx
      u(k,ic,jc,np) = ucor + urest
      v(k,ic,jc,np) = vcor + vrest
 600  continue
c
c-----------------------------------------------------------------------
c     calculate vertical mean force 
c-----------------------------------------------------------------------
c
      zu(ic,jc) = zu(ic,jc)*zwkr
      zv(ic,jc) = zv(ic,jc)*zwkr
c
c-----------------------------------------------------------------------
c     timestep baroclinic terms
c-----------------------------------------------------------------------
c
      if(acor.eq.c0)then
        do 700 k=1,kmc
          u(k,ic,jc,np) = u(k,ic,jc,nm) + c2dtuv*u(k,ic,jc,np) 
          v(k,ic,jc,np) = v(k,ic,jc,nm) + c2dtuv*v(k,ic,jc,np) 
 700    continue
      else
        fx    = c2dtuv*acor*fcor(jc)
        detmr = c1/(c1 + fx*fx)
        do 750 k=1,kmc
          t1 = (u(k,ic,jc,np)+fx*v(k,ic,jc,np))*detmr
          t2 = (v(k,ic,jc,np)-fx*u(k,ic,jc,np))*detmr
          u(k,ic,jc,np) = u(k,ic,jc,nm) + c2dtuv*t1 
          v(k,ic,jc,np) = v(k,ic,jc,nm) + c2dtuv*t2
 750    continue
      endif
c
c-----------------------------------------------------------------------
c     remove vertical mean velocity
c-----------------------------------------------------------------------
c
c jods      zwr = c1/zw(kmc)
      k=1
      sumu = u(1,ic,jc,np)*dzk
      sumv = v(1,ic,jc,np)*dzk
      do k=2,kmc
       sumu = sumu + u(k,ic,jc,np)*dz(k)
       sumv = sumv + v(k,ic,jc,np)*dz(k)
      enddo
      sumu = sumu*zwkr
      sumv = sumv*zwkr
      do 900 k=1,kmc
      u(k,ic,jc,np) = u(k,ic,jc,np) - sumu
      v(k,ic,jc,np) = v(k,ic,jc,np) - sumv
 900  continue

      return
      end
