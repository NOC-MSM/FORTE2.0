#include "cpp_options.h"
      subroutine tracer(ic,jc)
      implicit none
c
c=======================================================================
c
c     tracer computes tracer quantities for time level t+1
c
c=======================================================================
c
c jods : split depth loop into surface and rest
c
#include "param.h"
c
#include "scalar.h"
#include "ctmngr.h"
#include "grdvar.h"
#include "levind.h"
#include "timelv.h"
#include "slabs.h"
c
#include "chmix.h"
#include "cvbc.h"
#include "cvmix.h"
#include "frees.h"
#include "average.h"
#include "coord.h"
c Arguments:
      integer ic,jc
c Local:
      integer kmc,jp,jm,ip,im,jpp,jmm,ipp,imm,k,m
      _MOMA_REAL boxa,boxar,dxp5,dyp5,hn,hs,he,hw
     & , dzh1r,rh,rtmp,fkpsurf

#ifdef c_write
      REAL z,s,tc,tt,t2,t3
#endif /*c_write*/

#ifdef OLD_NON_CONSERVING
      integer n,ks
#endif /*OLD_NON_CONSERVING*/

#ifdef de_checkbd_T
      integer m_n,m_s,m_e,m_w,m_ne,m_se,m_sw,m_nw
      _MOMA_REAL dFlux, dplus, dCross
#endif /*de_checkbd_T*/

#ifdef tendency_write
      _MOMA_REAL temp_col(kmt(ic,jc))
#endif /*tendency_write*/

c
c=======================================================================
c    outer loops over i and j indices
c=======================================================================
c
      kmc= kmt(ic,jc)
      if(kmc.eq.0)return

      jp = jc+1
      jm = jc-1
      ip = ic+1
      im = ic-1
      jpp = jp+1
      jmm = jm-1
      ipp = ip+1
      imm = im-1
c
      boxa = dx*cst(jc)*dy
      boxar = c1/boxa
#ifndef adiabatic_msq
c
c=======================================================================
c    set up masks, equal to 0 on sub-seabottom points.
c=======================================================================
c
      do 110 k=1,kmc
#ifndef msq
        rmskn (k)=c0
        rmske (k)=c0
        rmsks (k)=c0
        rmskw (k)=c0
        if(k.le.kmt(ic,jp))rmskn(k)=c1
        if(k.le.kmt(ip,jc))rmske(k)=c1
        if(k.le.kmt(ic,jm))rmsks(k)=c1
        if(k.le.kmt(im,jc))rmskw(k)=c1
#else /*ndef msq*/
        rmskn (k)=c0
        rmsknn(k)=c0
        rmsknd(k)=c0
        if(k.le.kmt(ic,jp))then
          rmskn (k)=c1
          if(k.le.kmt(ic,jpp).and.k.le.kmt(ic,jm))then
            rmsknn(k)=r12
            rmsknd(k)=r16
          endif
        endif
c
        rmske (k)=c0
        rmskee(k)=c0
        rmsked(k)=c0
        if(k.le.kmt(ip,jc))then
          rmske (k)=c1
          if(k.le.kmt(ipp,jc).and.k.le.kmt(im,jc))then
            rmskee(k)=r12
            rmsked(k)=r16
          endif
        endif
c
        rmsks (k)=c0
        rmskss(k)=c0
        rmsksd(k)=c0
        if(k.le.kmt(ic,jm))then
          rmsks (k)=c1
          if(k.le.kmt(ic,jmm).and.k.le.kmt(ic,jp))then
            rmskss(k)=r12
            rmsksd(k)=r16
          endif
        endif
c
        rmskw (k)=c0
        rmskww(k)=c0
        rmskwd(k)=c0
        if(k.le.kmt(im,jc))then
          rmskw (k)=c1
          if(k.le.kmt(imm,jc).and.k.le.kmt(ip,jc))then
            rmskww(k)=r12
            rmskwd(k)=r16
          endif
        endif
#endif /*ndef msq*/
 110  continue
#endif /* ndef adiabatic_msq */

#ifdef griffies_p5
c-----------------------------------------------------------------------
c halve diffusive flux at top & bottom to ensure exact adiabaticity
c-----------------------------------------------------------------------
         rmskw(1)=p5*rmskw(1)
         rmske(1)=p5*rmske(1)
         rmsks(1)=p5*rmsks(1)
         rmskn(1)=p5*rmskn(1)

         k=min(kmc,kmt(im,jc))
         rmskw(k)=p5*rmskw(k)
         k=min(kmc,kmt(ip,jc))
         rmske(k)=p5*rmske(k)
         k=min(kmc,kmt(ic,jm))
         rmsks(k)=p5*rmsks(k)
         k=min(kmc,kmt(ic,jp))
         rmskn(k)=p5*rmskn(k)
#endif /*griffies_p5*/

#ifdef adiabatic_msq_p5
c-----------------------------------------------------------------------
c halve diffusive flux at top & bottom to ensure exact adiabaticity
c-----------------------------------------------------------------------
      do m=1,nt
         do i=0,1
            bihar_flux_e(1,i,m)=p5*bihar_flux_e(1,i,m)
            end do
         do j=0,1
            bihar_flux_n(1,j,m)=p5*bihar_flux_n(1,j,m)
            end do

         k=max(min(kmc,kmt(im,jc)),1)
         bihar_flux_e(k,0,m)=p5*bihar_flux_e(k,0,m)
         k=max(min(kmc,kmt(ip,jc)),1)
         bihar_flux_e(k,1,m)=p5*bihar_flux_e(k,1,m)
         k=max(min(kmc,kmt(ic,jm)),1)
         bihar_flux_n(k,0,m)=p5*bihar_flux_n(k,0,m)
         k=max(min(kmc,kmt(ic,jp)),1)
         bihar_flux_n(k,1,m)=p5*bihar_flux_n(k,1,m)
         end do
#endif /*adiabatic_msq_p5*/

c
c=======================================================================
c  calculate horizontal and vertical advective fluxes
c=======================================================================
c
      dxp5 = p5*dx
      dyp5 = p5*dy
#ifdef NONLIN_FREE_SURFACE
c
c adjust depth of top box to include free surface height at timestep nc
c
      hn = dz(1) + p5*(h00(ic,jc,2)+h00(ic,jp,2))
      hs = dz(1) + p5*(h00(ic,jc,2)+h00(ic,jm,2))
      he = dz(1) + p5*(h00(ic,jc,2)+h00(ip,jc,2))
      hw = dz(1) + p5*(h00(ic,jc,2)+h00(im,jc,2))
      dzh1r = c1/(dz(1)+h00(ic,jc,1))
#else /*NONLIN_FREE_SURFACE*/
      hn = dz(1)
      hs = dz(1)
      he = dz(1)
      hw = dz(1)
      dzh1r = dzr(1)
#endif /*NONLIN_FREE_SURFACE*/

#ifdef de_checkbd_T
c
c=======================================================================
c   calculate de-checkerboard  mask
c=======================================================================
c
      m_N  = min(1,kmt(ic,jp))
      m_S  = min(1,kmt(ic,jm))
      m_E  = min(1,kmt(ip,jc))
      m_W  = min(1,kmt(im,jc))
      m_NE = min(1,kmu(ic,jc))
      m_SE = min(1,kmu(ic,jm))
      m_SW = min(1,kmu(im,jm))
      m_NW = min(1,kmu(im,jc))
#endif /*de_checkbd_T*/

#ifndef adiabatic_msq
      fvn(1) = (v(1,ic,jc,nc)+v(1,im,jc,nc))*dxp5*csu(jc)*hn
      fvs(1) = (v(1,ic,jm,nc)+v(1,im,jm,nc))*dxp5*csu(jm)*hs
      fue(1) = (u(1,ic,jc,nc)+u(1,ic,jm,nc))*dyp5*he
      fuw(1) = (u(1,im,jc,nc)+u(1,im,jm,nc))*dyp5*hw
#endif /*ndef adiabatic_msq*/
      fw(0)  = (fuw(1)-fue(1)+fvs(1)-fvn(1))

      do 160 k=2,kmc
#ifndef adiabatic_msq
        fvn(k) = (v(k,ic,jc,nc)+v(k,im,jc,nc))*dxp5*csu(jc)*dz(k)
        fvs(k) = (v(k,ic,jm,nc)+v(k,im,jm,nc))*dxp5*csu(jm)*dz(k)
        fue(k) = (u(k,ic,jc,nc)+u(k,ic,jm,nc))*dyp5*dz(k)
        fuw(k) = (u(k,im,jc,nc)+u(k,im,jm,nc))*dyp5*dz(k)
#endif /*ndef adiabatic_msq*/
        fw(k-1)  = (fuw(k)-fue(k)+fvs(k)-fvn(k))
 160  continue
c
c=======================================================================
c    integrate up from the lowest layer
c    fw is positive upwards
c=======================================================================
c
      fw(kmc)=c0
      do 180 k=kmc-1,0,-1
        fw(k)=fw(k)+fw(k+1)
 180  continue

#ifdef w_write
c-----------------------------------------------------------------
c calculate and store vertical velocities
c-----------------------------------------------------------------

      do k=0,kmc
          w(k,ic,jc) = fw(k)*boxar
      end do
#endif /*w_write*/




c=======================================================================
c    set vertical diffusivities vdc: we use fkph
c for diapycnal mixing in isopycmix option add K33
C set variable diffusion in CGS units: f(T) at surface to 2.0 at bottom
C fkpsurf is a function from 0.4 to fkph depending on stability
C When Tsurf=Tbottom, fkph=fkpsurf; when Tsurf>>Tbottom fkph=0.2
C All in CGS. God what is wrong with oceanographers. I mean seriously
c=======================================================================

      fkpsurf = 0.25+(fkph-0.25)*
     &           exp(-((0.10*(t(1,ic,jc,1,nc)-t(kmc,ic,jc,1,nc)))**2))
C Account for when Tsurf < Tbottom
C      fkpsurf = min(fkpsurf,fkph)
      if(t(1,ic,jc,1,nc).lt.t(kmc,ic,jc,1,nc))fkpsurf = fkph
      do k = 1,kmc-1
#ifdef isopycmix
C         vdc(k) = fkph + K33(k)
C replace fkph with variable
         vdc(k) = fkpsurf+2.0E-6*(5000.e2/ZW(kmc-1))*(2.0-fkpsurf)*ZW(k)
     &            + K33(k)

#else /*isopycmix*/
         vdc(k) = fkph
#endif /*isopycmix*/
      end do

c
c=======================================================================
c    main calculation loop to timestep tracers
c=======================================================================
c
      do 900 m=1,nt
c
#ifdef de_checkbd_T
c
c=======================================================================
c   calculate de-checkerboard  terms
c=======================================================================
c
        dplus  = m_E*(h0(ip,jc,np0) - h0(ic,jc,np0))*cst(jc)
     &             *(t(1,ip,jc,m,nc)+t(1,ic,jc,m,nc))
     &          +m_W*(h0(im,jc,np0) - h0(ic,jc,np0))*cst(jc)
     &             *(t(1,im,jc,m,nc)+t(1,ic,jc,m,nc))
     &          +m_N*(h0(ic,jp,np0) - h0(ic,jc,np0))*csu(jc)
     &             *(t(1,ic,jp,m,nc)+t(1,ic,jc,m,nc))
     &          +m_S*(h0(ic,jm,np0) - h0(ic,jc,np0))*csu(jm)
     &             *(t(1,ic,jm,m,nc)+t(1,ic,jc,m,nc))
        dcross = m_NE*(h0(ip,jp,np0) - h0(ic,jc,np0))*csu(jc)
     &              *(t(1,ip,jp,m,nc)+t(1,ic,jc,m,nc))
     &          +m_SW*(h0(im,jm,np0) - h0(ic,jc,np0))*csu(jm)
     &              *(t(1,im,jm,m,nc)+t(1,ic,jc,m,nc))
     &          +m_NW*(h0(im,jp,np0) - h0(ic,jc,np0))*csu(jc)
     &              *(t(1,im,jp,m,nc)+t(1,ic,jc,m,nc))
     &          +m_SE*(h0(ip,jm,np0) - h0(ic,jc,np0))*csu(jm)
     &              *(t(1,ip,jm,m,nc)+t(1,ic,jc,m,nc))
        dflux = dchkbd*(dplus-0.5*dcross)*0.5*(2.0*dtbt/dtts)

#endif /*de_checkbd_T*/
c
c=======================================================================
c  calculate vertical fluxes - positive upwards
c=======================================================================
c
      do 930 k=1,kmc-1
        vtf(k) = vdc(k)*(t(k+1,ic,jc,m,nm)-t(k  ,ic,jc,m,nm))
     &                 *dzwr(k)*boxa
        T_up(k)  = (t(k,ic,jc,m,nc)+t(k+1,ic,jc,m,nc))*p5
 930  continue

#ifdef msq
c
c=======================================================================
c  calculate msq correction to advective flux
c=======================================================================
c
c-----------------------------------------------------------------------
c  save T_up : tracer values advected up at bottom of box
c note also vert msq diffusive corr'n now added to vtf ..
c-----------------------------------------------------------------------
c *** Set Diff_msqUp to 0 - George suggests since not used in OCCAM ***
c     if(kmc.ge.4)then
c       Diff_msqUp(0) = c0
c       Diff_msqUp(1) = c0
c       Diff_msqUp(kmc-1) = c0
c       Diff_msqUp(kmc) = c0
c       do k=2,kmc-2
c         T_up(k) = T_up(k)
c    &            - r12*(t(k+2,ic,jc,m,nc)-t(k+1,ic,jc,m,nc)
c    &                           -t(k,ic,jc,m,nc)+t(k-1,ic,jc,m,nc))
c         Diff_msqUp(k) =
c    &              abs(fw(k))*r16*(t(k-1,ic,jc,m,nm)-t(k+2,ic,jc,m,nm)
c    &                       -c3*(t(k  ,ic,jc,m,nm)-t(k+1,ic,jc,m,nm)))
c       end do
c     else
         do k=0,kmc
            Diff_msqUp(k) = c0
         enddo
c     endif
#endif /*msq*/

      do k=1,kmc-1
         fwb1(k) = T_up(k)*fw(k)
      end do

c
c=======================================================================
c  surface and bottom advective and diffusive boundary conditions
c  stf is downwards flux into the ocean
c=======================================================================
c
      vtf(0)   = -stf(m)*boxa ! = (c0)
      vtf(kmc) = btf(m)*boxa
#ifdef OLD_NON_CONSERVING
      fwb1(0)  = fw(0) * t(1,ic,jc,m,nc)
#else /*OLD_NON_CONSERVING*/
      fwb1(0)  = c0 !fw(0) * t(1,ic,jc,m,nc)
#endif /*OLD_NON_CONSERVING*/
      fwb1(kmc)= c0

c
c=======================================================================
c  main timestepping calculation
c=======================================================================
c
c

#ifdef rho_is_write
          call state(t(1,ic,jc,1,nc),t(1,ic,jc,2,nc),rhoo,kmc,0)
          do k=1,kmc
            rho_is(k,ic,jc) = (rhoo(k)*1000.+ref_dens(k)+1000.)
          enddo
#endif /*rho_is_write*/


#ifdef c_write
c-----------------------------------------------------------------
c calculate and store sound velocities
c-----------------------------------------------------------------
      do k=1,kmc
            z=zt(k)/100000.
            tc=t(k,ic,jc,1,nc)
            tt=tc/10.
            t2 = tt*tt
            t3 = t2*tt
            s=t(k,ic,jc,2,nc)
            cs(k,ic,jc) = 1449.05 + 45.7*tt - 5.21*t2 + 0.23*t3
     &             +(1.333-0.126*tt+0.009*t2)*(s*1000.)
     &             + z*(16.3 + 0.18*z)
      enddo
#endif /*c_write*/


c---Output poleward flux terms (for cmip project)
#ifdef nflux_write

#ifndef msq
      if(m.eq.2)then
        tracerbit=(1-(t(1,ic,jm,m,nc)+t(1,ic,jc,m,nc)))
      else
        tracerbit=(t(1,ic,jm,m,nc)+t(1,ic,jc,m,nc))
      endif
#else /*ndef msq*/
c---Use freshwater (1-S) instead of salinity (S)
c---Note: Diffusive terms do not need to be changed
      if(m.eq.2)then
c Note: 1 - S = 1 - ( S(model units) + (35/1000) )
        tracerbit=(1-(((t(1,ic,jm,m,nc)+t(1,ic,jc,m,nc))*
     &   (p5+rmskss(1))-(t(1,ic,jmm,m,nc)+t(1,ic,jp,m,nc))*
     &   rmskss(1))+.035))
      else
        tracerbit=((t(1,ic,jm,m,nc)+t(1,ic,jc,m,nc))*
     &   (p5+rmskss(1))-(t(1,ic,jmm,m,nc)+t(1,ic,jp,m,nc))*rmskss(1))
      endif
#endif /*ndef msq*/

         nflux(1,ic,jc,m) = c2dtts*(fvs(1)*tracerbit

#ifdef adiabatic_msq
     &   - bihar_flux_n(1,0,m)
#else /*.not. adiabatic_msq*/
     &   - abs(fvs(1))*(t(1,ic,jmm,m,nm) - t(1,ic,jp ,m,nm)
     &   - c3*(t(1,ic,jm ,m,nm) - t(1,ic,jc ,m,nm)))*rmsksd(1)
#endif /*adiabatic_msq*/

#ifdef full_griffies
c   isopycnal y-diffusion
     &      - diff_fn(1,-1,m)*tmask(1,0,-1)*dx*dz(1))
#else /*.not. full_griffies*/
c  y diffusion
     & + ddt(jc)*(t(1,ic,jm,m,nm)-t(1,ic,jc,m,nm))*rmsks(1)*hs)
#endif /*full_griffies*/

#endif /*nflux_write*/


c---Output tendency terms (for cmip project)
#ifdef tendency_write

c---Tendency of tracers due to advection
      tend_adv(1,ic,jc,m)=c2dtts*boxar*dzh1r*(

#ifndef msq
c  u-advection
     &      + (fuw(1)*(t(1,im,jc,m,nc) + t(1,ic,jc,m,nc))
     &      - fue(1)*(t(1,ip,jc,m,nc) + t(1,ic,jc,m,nc))
c  v-advection
     &      + fvs(1)*(t(1,ic,jm,m,nc) + t(1,ic,jc,m,nc))
     &      - fvn(1)*(t(1,ic,jp,m,nc) + t(1,ic,jc,m,nc)))*p5
#else /*ndef msq*/
c  msq u-advection
     & + fuw(1)*((t(1,im,jc,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskww(1))
     &         - (t(1,imm,jc,m,nc) + t(1,ip,jc,m,nc))*rmskww(1))
     & - fue(1)*((t(1,ip,jc,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskee(1))
     &         - (t(1,ipp,jc,m,nc) + t(1,im,jc,m,nc))*rmskee(1))
c  msq v-advection
     & + fvs(1)*((t(1,ic,jm,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskss(1))
     &         - (t(1,ic,jmm,m,nc) + t(1,ic,jp,m,nc))*rmskss(1))
     & - fvn(1)*((t(1,ic,jp,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmsknn(1))
     &         - (t(1,ic,jpp,m,nc) + t(1,ic,jm,m,nc))*rmsknn(1))
#endif /*ndef msq*/
c  w-advection
     & + fwb1(1  )
     & - fwb1(0  ))

c---Tendency of tracers due to horizontal diffusion
      tend_hdiff(1,ic,jc,m)=c2dtts*boxar*dzh1r*(

#ifdef msq
#ifdef adiabatic_msq
c note bihar_flux_e,_n are *negative* of fluxes ...
     & + bihar_flux_e(1,1,m)
     & - bihar_flux_e(1,0,m)
     & + bihar_flux_n(1,1,m)
     & - bihar_flux_n(1,0,m)
#else /*.not. adiabatic_msq*/
c  velocity dependent bi-harmonic diffusion
     & - abs(fuw(1))*(t(1,imm,jc,m,nm) - t(1,ip ,jc,m,nm)
     &          - c3*(t(1,im ,jc,m,nm) - t(1,ic ,jc,m,nm)))*rmskwd(1)
     & - abs(fue(1))*(t(1,ipp,jc,m,nm) - t(1,im ,jc,m,nm)
     &          - c3*(t(1,ip ,jc,m,nm) - t(1,ic ,jc,m,nm)))*rmsked(1)
     & - abs(fvs(1))*(t(1,ic,jmm,m,nm) - t(1,ic,jp ,m,nm)
     &          - c3*(t(1,ic,jm ,m,nm) - t(1,ic,jc ,m,nm)))*rmsksd(1)
     & - abs(fvn(1))*(t(1,ic,jpp,m,nm) - t(1,ic,jm ,m,nm)
     &          - c3*(t(1,ic,jp ,m,nm) - t(1,ic,jc ,m,nm)))*rmsknd(1)
#endif /*adiabatic_msq*/
c .... vertical biharmonic flux is now treated separately ..
     & - Diff_msqUp(0)
     & + Diff_msqUp(1)
#endif /*msq*/
#ifdef full_griffies
c ...........................................................................
c note that diff_fe,fn,fbiso are *westward*, southward, *downward*
c  (i.e. -opposite of) diff've fluxes *per unit area corrected by coslat*
c MOM3 p464 ...
c ...........................................................................
c NOTE: the dz(1) here is not really fully consistent - should be reviewed later...
c   isopycnal x-diffusion
     &      + diff_fe(1, 0,m)*tmask(1, 1,0)*dy*dz(1)
     &      - diff_fe(1,-1,m)*tmask(1,-1,0)*dy*dz(1)
c   isopycnal y-diffusion
     &      + diff_fn(1, 0,m)*tmask(1,0, 1)*dx*dz(1)
     &      - diff_fn(1,-1,m)*tmask(1,0,-1)*dx*dz(1))

C Put additional equatorial horizontal mixing into top layer
c     x diffusion
     &      + bbt(jc)*(t(1,ip,jc,m,nm)-t(1,ic,jc,m,nm))*rmske(1)*he*
     &       (2.0+38.0*exp(-(phit(jc)/0.07)**4))
     &      + bbt(jc)*(t(1,im,jc,m,nm)-t(1,ic,jc,m,nm))*rmskw(1)*hw*
     &       (2.0+38.0*exp(-(phit(jc)/0.07)**4))
c  y diffusion
     & + cct(jc)*(t(1,ic,jp,m,nm)-t(1,ic,jc,m,nm))*rmskn(1)*hn*
     &   (2.0+38.0*exp(-(phi(jc)/0.07)**4))
     & + ddt(jc)*(t(1,ic,jm,m,nm)-t(1,ic,jc,m,nm))*rmsks(1)*hs*
     &   (2.0+38.0*exp(-(phi(jm)/0.07)**4))                    )
C End of additional equatorial horizontal mixing into top layer

#else /*.not. full_griffies*/
c     x diffusion
     &      + bbt(jc)*(t(1,ip,jc,m,nm)-t(1,ic,jc,m,nm))*rmske(1)*he
     &      + bbt(jc)*(t(1,im,jc,m,nm)-t(1,ic,jc,m,nm))*rmskw(1)*hw
c  y diffusion
     & + cct(jc)*(t(1,ic,jp,m,nm)-t(1,ic,jc,m,nm))*rmskn(1)*hn
     & + ddt(jc)*(t(1,ic,jm,m,nm)-t(1,ic,jc,m,nm))*rmsks(1)*hs
#endif /*full_griffies*/


c---Tendency of tracers due to vertical diffusion
      tend_vdiff(1,ic,jc,m)=c2dtts*boxar*dzh1r*(

c  z diffusion
#ifdef isopycmix
c fraction aidif of diffusion is done implicitly
     &  + vtf(1) *(c1-aidif)
     & - vtf(1-1)*(c1-aidif)
c    slope z-diffusion terms
     & - diff_fbiso(1,m)*boxa
     & + diff_fbiso(0,m)*boxa
#else /*.not. isopycmix*/
     & + vtf(1)
     & - vtf(0)
#endif /*isopycmix*/
#ifdef de_checkbd_T
     &      +dflux
#endif /*de_checkbd_T*/
     &      )

c---Sum tendencies and calculate new temperature
       t(1,ic,jc,m,np) = t(1,ic,jc,m,nm)
     &      + tend_adv(1,ic,jc,m) + tend_hdiff(1,ic,jc,m)
     &      + tend_vdiff(1,ic,jc,m)

c---end of tendency_write
#else /*.not. tendency_write*/

       t(1,ic,jc,m,np) = t(1,ic,jc,m,nm)
     &      + c2dtts*boxar*dzh1r*(
#ifndef msq
c  u-advection
     &      + (fuw(1)*(t(1,im,jc,m,nc) + t(1,ic,jc,m,nc))
     &      - fue(1)*(t(1,ip,jc,m,nc) + t(1,ic,jc,m,nc))
c  v-advection
     &      + fvs(1)*(t(1,ic,jm,m,nc) + t(1,ic,jc,m,nc))
     &      - fvn(1)*(t(1,ic,jp,m,nc) + t(1,ic,jc,m,nc)))*p5
#else /*ndef msq*/
c  msq u-advection
     & + fuw(1)*((t(1,im,jc,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskww(1))
     &         - (t(1,imm,jc,m,nc) + t(1,ip,jc,m,nc))*rmskww(1))
     & - fue(1)*((t(1,ip,jc,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskee(1))
     &         - (t(1,ipp,jc,m,nc) + t(1,im,jc,m,nc))*rmskee(1))
c  msq v-advection
     & + fvs(1)*((t(1,ic,jm,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmskss(1))
     &         - (t(1,ic,jmm,m,nc) + t(1,ic,jp,m,nc))*rmskss(1))
     & - fvn(1)*((t(1,ic,jp,m,nc)  + t(1,ic,jc,m,nc))*(p5+rmsknn(1))
     &         - (t(1,ic,jpp,m,nc) + t(1,ic,jm,m,nc))*rmsknn(1))
#endif /*ndef msq*/
c  w-advection
     & + fwb1(1  )
     & - fwb1(0  )
#ifdef msq
#ifdef adiabatic_msq
c note bihar_flux_e,_n are *negative* of fluxes ...
     & + bihar_flux_e(1,1,m)
     & - bihar_flux_e(1,0,m)
     & + bihar_flux_n(1,1,m)
     & - bihar_flux_n(1,0,m)
#else /*.not. adiabatic_msq*/
c  velocity dependent bi-harmonic diffusion
     & - abs(fuw(1))*(t(1,imm,jc,m,nm) - t(1,ip ,jc,m,nm)
     &          - c3*(t(1,im ,jc,m,nm) - t(1,ic ,jc,m,nm)))*rmskwd(1)
     & - abs(fue(1))*(t(1,ipp,jc,m,nm) - t(1,im ,jc,m,nm)
     &          - c3*(t(1,ip ,jc,m,nm) - t(1,ic ,jc,m,nm)))*rmsked(1)
     & - abs(fvs(1))*(t(1,ic,jmm,m,nm) - t(1,ic,jp ,m,nm)
     &          - c3*(t(1,ic,jm ,m,nm) - t(1,ic,jc ,m,nm)))*rmsksd(1)
     & - abs(fvn(1))*(t(1,ic,jpp,m,nm) - t(1,ic,jm ,m,nm)
     &          - c3*(t(1,ic,jp ,m,nm) - t(1,ic,jc ,m,nm)))*rmsknd(1)
#endif /*adiabatic_msq*/
c .... vertical biharmonic flux is now treated separately ..
     & - Diff_msqUp(0)
     & + Diff_msqUp(1)
#endif /*msq*/
#ifdef full_griffies
c ...........................................................................
c note that diff_fe,fn,fbiso are *westward*, southward, *downward*
c  (i.e. -opposite of) diff've fluxes *per unit area corrected by coslat*
c MOM3 p464 ...
c ...........................................................................
c NOTE: the dz(1) here is not really fully consistent - should be reviewed later...
c   isopycnal x-diffusion
     &      + diff_fe(1, 0,m)*tmask(1, 1,0)*dy*dz(1)
     &      - diff_fe(1,-1,m)*tmask(1,-1,0)*dy*dz(1)
c   isopycnal y-diffusion
     &      + diff_fn(1, 0,m)*tmask(1,0, 1)*dx*dz(1)
     &      - diff_fn(1,-1,m)*tmask(1,0,-1)*dx*dz(1)

C Put additional equatorial horizontal mixing into top layer
c     x diffusion
     &      + bbt(jc)*(t(1,ip,jc,m,nm)-t(1,ic,jc,m,nm))*rmske(1)*he*
     &   (2.0+38.0*exp(-(phit(jc)/0.07)**4))
     &      + bbt(jc)*(t(1,im,jc,m,nm)-t(1,ic,jc,m,nm))*rmskw(1)*hw*
     &   (2.0+38.0*exp(-(phit(jc)/0.07)**4))
c  y diffusion
     & + cct(jc)*(t(1,ic,jp,m,nm)-t(1,ic,jc,m,nm))*rmskn(1)*hn*
     &   (2.0+38.0*exp(-(phi(jc)/0.07)**4))
     & + ddt(jc)*(t(1,ic,jm,m,nm)-t(1,ic,jc,m,nm))*rmsks(1)*hs*
     &   (2.0+38.0*exp(-(phi(jm)/0.07)**4))
C End of additional equatorial horizontal mixing into top layer

#else /*.not. full_griffies*/
c     x diffusion
     &      + bbt(jc)*(t(1,ip,jc,m,nm)-t(1,ic,jc,m,nm))*rmske(1)*he
     &      + bbt(jc)*(t(1,im,jc,m,nm)-t(1,ic,jc,m,nm))*rmskw(1)*hw
c  y diffusion
     & + cct(jc)*(t(1,ic,jp,m,nm)-t(1,ic,jc,m,nm))*rmskn(1)*hn
     & + ddt(jc)*(t(1,ic,jm,m,nm)-t(1,ic,jc,m,nm))*rmsks(1)*hs
#endif /*full_griffies*/
c  z diffusion
#ifdef isopycmix
c fraction aidif of diffusion is done implicitly
     &  + vtf(1) *(c1-aidif)
     & - vtf(1-1)*(c1-aidif)
c    slope z-diffusion terms
     & - diff_fbiso(1,m)*boxa
     & + diff_fbiso(0,m)*boxa
#else /*.not. isopycmix*/
     & + vtf(1)
     & - vtf(0)
#endif /*isopycmix*/
#ifdef de_checkbd_T
     &      +dflux
#endif /*de_checkbd_T*/
     &      )

#endif /*tendency_write*/




      do 960 k=2,kmc


c---Output poleward flux terms (for cmip project)
#ifdef nflux_write

#ifndef msq
      if(m.eq.2)then
        tracerbit=(1-(t(k,ic,jm,m,nc)+t(k,ic,jc,m,nc)))
      else
        tracerbit=(t(k,ic,jm,m,nc)+t(k,ic,jc,m,nc))
      endif
#else /*ndef msq*/
c---Use freshwater (1-S) instead of salinity (S)
c---Note: Diffusive terms do not need to be changed

      if(m.eq.2)then
        tracerbit=(1-((t(k,ic,jm,m,nc)+t(k,ic,jc,m,nc))*
     &   (p5+rmskss(k))-(t(k,ic,jmm,m,nc)+t(k,ic,jp,m,nc))*
     &   rmskss(k)+.035))
      else
        tracerbit=((t(k,ic,jm,m,nc)+t(k,ic,jc,m,nc))*
     &   (p5+rmskss(k))-(t(k,ic,jmm,m,nc)+t(k,ic,jp,m,nc))*rmskss(k))
      endif
#endif /*ndef msq*/

        nflux(k,ic,jc,m) = c2dtts*(fvs(k)*tracerbit

#ifdef adiabatic_msq
     &   - bihar_flux_n(k,0,m)
#else /*.not. adiabatic_msq*/
     & - abs(fvs(k))*(t(k,ic,jmm,m,nm) - t(k,ic,jp ,m,nm)
     & - c3*(t(k,ic,jm ,m,nm) - t(k,ic,jc ,m,nm)))*rmsksd(k)
#endif /*adiabatic_msq*/

#ifdef full_griffies
c   isopycnal y-diffusion
     &      - diff_fn(k,-1,m)*tmask(k,0,-1)*dx*dz(k))
#else /*.not. full_griffies*/
c  y diffusion
     & + ddt(jc)*(t(k,ic,jm,m,nm)-t(k,ic,jc,m,nm))*rmsks(k)*dz(k))
#endif /*full_griffies*/

#endif /*nflux_write*/


c---Output tendency terms (for cmip project)
#ifdef tendency_write

c---Tendency of tracers due to advection
      tend_adv(k,ic,jc,m)=c2dtts*boxar*dzr(k)*(

#ifndef msq
c  u-advection
     &      + (fuw(k)*(t(k,im,jc,m,nc) + t(k,ic,jc,m,nc))
     &      - fue(k)*(t(k,ip,jc,m,nc) + t(k,ic,jc,m,nc))
c  v-advection
     &      + fvs(k)*(t(k,ic,jm,m,nc) + t(k,ic,jc,m,nc))
     &      - fvn(k)*(t(k,ic,jp,m,nc) + t(k,ic,jc,m,nc)))*p5
#else /*ndef msq*/
c  msq u-advection
     & + fuw(k)*((t(k,im,jc,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskww(k))
     &         - (t(k,imm,jc,m,nc) + t(k,ip,jc,m,nc))*rmskww(k))
     & - fue(k)*((t(k,ip,jc,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskee(k))
     &         - (t(k,ipp,jc,m,nc) + t(k,im,jc,m,nc))*rmskee(k))
c  msq v-advection
     & + fvs(k)*((t(k,ic,jm,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskss(k))
     &         - (t(k,ic,jmm,m,nc) + t(k,ic,jp,m,nc))*rmskss(k))
     & - fvn(k)*((t(k,ic,jp,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmsknn(k))
     &         - (t(k,ic,jpp,m,nc) + t(k,ic,jm,m,nc))*rmsknn(k))
#endif /*ndef msq*/
c  w-advection
     & + fwb1(k  ) -fwb1(k-1))

c---Tendency of tracers due to horizontal diffusion
      tend_hdiff(k,ic,jc,m)=c2dtts*boxar*dzr(k)*(

#ifdef msq
#ifdef adiabatic_msq
c note bihar_flux_e,_n are *negative* of fluxes ...
     & + bihar_flux_e(k,1,m)
     & - bihar_flux_e(k,0,m)
     & + bihar_flux_n(k,1,m)
     & - bihar_flux_n(k,0,m)
#else /*.not. adiabatic_msq*/
c  velocity dependent bi-harmonic diffusion
     & - abs(fuw(k))*(t(k,imm,jc,m,nm) - t(k,ip ,jc,m,nm)
     &          - c3*(t(k,im ,jc,m,nm) - t(k,ic ,jc,m,nm)))*rmskwd(k)
     & - abs(fue(k))*(t(k,ipp,jc,m,nm) - t(k,im ,jc,m,nm)
     &          - c3*(t(k,ip ,jc,m,nm) - t(k,ic ,jc,m,nm)))*rmsked(k)
     & - abs(fvs(k))*(t(k,ic,jmm,m,nm) - t(k,ic,jp ,m,nm)
     &          - c3*(t(k,ic,jm ,m,nm) - t(k,ic,jc ,m,nm)))*rmsksd(k)
     & - abs(fvn(k))*(t(k,ic,jpp,m,nm) - t(k,ic,jm ,m,nm)
     &          - c3*(t(k,ic,jp ,m,nm) - t(k,ic,jc ,m,nm)))*rmsknd(k)
#endif /*adiabatic_msq*/
c .... vertical biharmonic flux is now treated separately ..
     & + Diff_msqUp(k) - Diff_msqUp(k-1)
#endif /*msq*/
#ifdef full_griffies
c ...........................................................................
c note that diff_fe,fn,fbiso are *westward*, southward, *downward*
c  (i.e. -opposite of) diff've fluxes *per unit area corrected by coslat*
c MOM3 p464 ...
c ...........................................................................
c   isopycnal x-diffusion
     &      + ((diff_fe(k, 0,m)*tmask(k, 1,0)
     &      - diff_fe(k,-1,m)*tmask(k,-1,0))*dy
c   isopycnal y-diffusion
     &      + (diff_fn(k, 0,m)*tmask(k,0, 1)
     &      - diff_fn(k,-1,m)*tmask(k,0,-1))*dx)*dz(k))
#else /*.not. full_griffies*/
c     x diffusion
     &      + (bbt(jc)*(t(k,ip,jc,m,nm)-t(k,ic,jc,m,nm))*rmske(k)
     &      + bbt(jc)*(t(k,im,jc,m,nm)-t(k,ic,jc,m,nm))*rmskw(k)
c  y diffusion
     & + cct(jc)*(t(k,ic,jp,m,nm)-t(k,ic,jc,m,nm))*rmskn(k)
     & + ddt(jc)*(t(k,ic,jm,m,nm)-t(k,ic,jc,m,nm))*rmsks(k))*dz(k))
#endif /*full_griffies*/


c     if(k.eq.kmc)then
c---Tendency of tracers due to vertical diffusion
c     tend_vdiff(k,ic,jc,m)=c2dtts*boxar*dzr(k)*(

c  z diffusion
cifdef isopycmix
c fraction aidif of diffusion is done implicitly
c    &  + (vtf(k) - vtf(k-1)) *(c1-aidif)
c    no slope z-diffusion terms
c    &  - (diff_fbiso(k,m)-diff_fbiso(k-1,m))  *boxa
celse /*  .not. isopycmix */
c    & + vtf(k) - vtf(k-1)
cendif /* isopycmix */
c    & )
c     else
c---Tendency of tracers due to vertical diffusion
      tend_vdiff(k,ic,jc,m)=c2dtts*boxar*dzr(k)*(

c  z diffusion
#ifdef isopycmix
c fraction aidif of diffusion is done implicitly
     &  + (vtf(k) - vtf(k-1)) *(c1-aidif)
c    slope z-diffusion terms
     &  - (diff_fbiso(k,m)-diff_fbiso(k-1,m))  *boxa
#else /*.not. isopycmix*/
     & + vtf(k) - vtf(k-1)
#endif /*isopycmix*/
     & )
c     endif

c---Sum tendencies and calculate new temperature
       t(k,ic,jc,m,np) = t(k,ic,jc,m,nm)
     &      + tend_adv(k,ic,jc,m) + tend_hdiff(k,ic,jc,m)
     &      + tend_vdiff(k,ic,jc,m)

c---end of tendency_write
#else /*tendency_write*/

       t(k,ic,jc,m,np) = t(k,ic,jc,m,nm)
     &      + c2dtts*boxar*dzr(k)*(
#ifndef msq
c  u-advection
     &      + (fuw(k)*(t(k,im,jc,m,nc) + t(k,ic,jc,m,nc))
     &      - fue(k)*(t(k,ip,jc,m,nc) + t(k,ic,jc,m,nc))
c  v-advection
     &      + fvs(k)*(t(k,ic,jm,m,nc) + t(k,ic,jc,m,nc))
     &      - fvn(k)*(t(k,ic,jp,m,nc) + t(k,ic,jc,m,nc)))*p5
#else /*ndef msq*/
c  msq u-advection
     & + fuw(k)*((t(k,im,jc,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskww(k))
     &         - (t(k,imm,jc,m,nc) + t(k,ip,jc,m,nc))*rmskww(k))
     & - fue(k)*((t(k,ip,jc,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskee(k))
     &         - (t(k,ipp,jc,m,nc) + t(k,im,jc,m,nc))*rmskee(k))
c  msq v-advection
     & + fvs(k)*((t(k,ic,jm,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmskss(k))
     &         - (t(k,ic,jmm,m,nc) + t(k,ic,jp,m,nc))*rmskss(k))
     & - fvn(k)*((t(k,ic,jp,m,nc)  + t(k,ic,jc,m,nc))*(p5+rmsknn(k))
     &         - (t(k,ic,jpp,m,nc) + t(k,ic,jm,m,nc))*rmsknn(k))
#endif /*ndef msq*/
c  w-advection
     & + fwb1(k  ) -fwb1(k-1)

#ifdef msq
#ifdef adiabatic_msq
c note bihar_flux_e,_n are *negative* of fluxes ...
     & + bihar_flux_e(k,1,m)
     & - bihar_flux_e(k,0,m)
     & + bihar_flux_n(k,1,m)
     & - bihar_flux_n(k,0,m)
#else /*.not. adiabatic_msq*/
c  velocity dependent bi-harmonic diffusion
     & - abs(fuw(k))*(t(k,imm,jc,m,nm) - t(k,ip ,jc,m,nm)
     &          - c3*(t(k,im ,jc,m,nm) - t(k,ic ,jc,m,nm)))*rmskwd(k)
     & - abs(fue(k))*(t(k,ipp,jc,m,nm) - t(k,im ,jc,m,nm)
     &          - c3*(t(k,ip ,jc,m,nm) - t(k,ic ,jc,m,nm)))*rmsked(k)
     & - abs(fvs(k))*(t(k,ic,jmm,m,nm) - t(k,ic,jp ,m,nm)
     &          - c3*(t(k,ic,jm ,m,nm) - t(k,ic,jc ,m,nm)))*rmsksd(k)
     & - abs(fvn(k))*(t(k,ic,jpp,m,nm) - t(k,ic,jm ,m,nm)
     &          - c3*(t(k,ic,jp ,m,nm) - t(k,ic,jc ,m,nm)))*rmsknd(k)
#endif /*adiabatic_msq*/
c .... vertical biharmonic flux is now treated separately ..
     & + Diff_msqUp(k) - Diff_msqUp(k-1)
#endif /*msq*/
#ifdef full_griffies
c ...........................................................................
c note that diff_fe,fn,fbiso are *westward*, southward, *downward*
c  (i.e. -opposite of) diff've fluxes *per unit area corrected by coslat*
c MOM3 p464 ...
c ...........................................................................
c   isopycnal x-diffusion
     &      + ((diff_fe(k, 0,m)*tmask(k, 1,0)
     &      - diff_fe(k,-1,m)*tmask(k,-1,0))*dy
c   isopycnal y-diffusion
     &      + (diff_fn(k, 0,m)*tmask(k,0, 1)
     &      - diff_fn(k,-1,m)*tmask(k,0,-1))*dx)*dz(k)
#else /*.not. full_griffies*/
c     x diffusion
     &      + (bbt(jc)*(t(k,ip,jc,m,nm)-t(k,ic,jc,m,nm))*rmske(k)
     &      + bbt(jc)*(t(k,im,jc,m,nm)-t(k,ic,jc,m,nm))*rmskw(k)
c  y diffusion
     & + cct(jc)*(t(k,ic,jp,m,nm)-t(k,ic,jc,m,nm))*rmskn(k)
     & + ddt(jc)*(t(k,ic,jm,m,nm)-t(k,ic,jc,m,nm))*rmsks(k))*dz(k)
#endif /*full_griffies*/
c  z diffusion
#ifdef isopycmix
c fraction aidif of diffusion is done implicitly
     &  + (vtf(k) - vtf(k-1)) *(c1-aidif)
c    slope z-diffusion terms
     &  - (diff_fbiso(k,m)-diff_fbiso(k-1,m))  *boxa
#else /*.not. isopycmix*/
     & + vtf(k) - vtf(k-1)
#endif /*isopycmix*/
     & )

#endif /*tendency_write*/

 960  continue  !end of k loop


#ifdef isopycmix

#ifdef tendency_write
      do k=1,kmc
      temp_col(k) = t(k,ic,jc,m,np)
      end do
#endif /*tendency_write*/

c     solve the vertical diffusion equation implicitly
      call invtri (t(1,ic,jc,m,np), stf(m),btf(m), vdc(1),
     &     c2dtts, ic,jc, kmc )

#ifdef tendency_write
      do k=1,kmc
      tend_vdiff(k,ic,jc,m) = tend_vdiff(k,ic,jc,m)
     &                        + t(k,ic,jc,m,np) - temp_col(k)
      end do
#endif /*tendency_write*/

#endif /*isopycmix*/

 900  continue  !end of tracer loop



#ifdef OLD_NON_CONSERVING
c
c=======================================================================
c   convectively adjust water column if gravitaionally unstable.
c=======================================================================
c
      if(kmc.gt.1.and.ncon.gt.0)then
        do 1070 n=1,ncon
        do 1070 ks=1,2
        call state(t(1,ic,jc,1,np),t(1,ic,jc,2,np),temp(1),
     &              kmc, ks)
        do 1070 m=1,nt
        do 1070 k=ks,kmc-1,2
        if(temp(k).gt.temp(k+1))then
          t(k,ic,jc,m,np) = (dz(k)*t(k,ic,jc,m,np)
     &                    +  dz(k+1)*t(k+1,ic,jc,m,np))*dzw2r(k)
          t(k+1,ic,jc,m,np) = t(k,ic,jc,m,np)
        endif
 1070   continue
      endif
#endif /*OLD_NON_CONSERVING*/

      return
      end
