#include "cpp_options.h"
      subroutine setvbc (ic, jc)
c
c=======================================================================
c
c     compute vertical boundary conditions for clinic & tracer
c
c=======================================================================
c
#include "param.h"
#include "cvbc.h"
#include "grdvar.h"
#include "scalar.h"
#include "slabs.h"
#include "timelv.h"
#include "levind.h"
#include "ctmngr.h"
c
#include "coord.h"
c jods: for h00
#include "frees.h"

#ifdef oasis
#include "oasis.h"
      real ep
#endif

c Local:
      integer ic,jc,m,kz
      real gamma,uvmag,dzh1
c-----------------------------------------------------------------
c     get test case surface temperatures, salinities and wind 
c  
c   stresses (they vary with latitude).  test assumes nt=2,
c     (potential temperature and salinity)
c-----------------------------------------------------------------------
c
c     
c if using oasis, feed in fluxes here
#ifdef oasis

         snf(1) = 1.d-4*(heatlat(ic, jc) + swlw(ic, jc)
     $            + sensible(ic, jc) )/4.2d0


#ifdef cmip_flxrd
         snf(1) = surfrd_t_flux(ic,jc,1)
#endif


         ep=water(ic,jc)

c--- Hosing hosing code
#ifdef hosing
        if(ic.ge.156.and.ic.le.179.and.jc.eq.71)then
        ep=ep+1.22
        endif
c 24 pts
        if(ic.ge.155.and.ic.le.177.and.jc.eq.72)then
        ep=ep+1.22
        endif
c 23 pts
        if(ic.ge.153.and.ic.le.178.and.jc.eq.73)then
        ep=ep+1.22
        endif
c 26 pts
        if(ic.ge.153.and.ic.le.182.and.jc.eq.74)then
        ep=ep+1.22
        endif
c 28 pts
        if(ic.ge.151.and.ic.le.182.and.jc.eq.75)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.5.and.jc.eq.75)then
        ep=ep+1.22
        endif
c 35 pts
        if(ic.ge.151.and.ic.le.158.and.jc.eq.76)then
        ep=ep+1.22
        endif
        if(ic.ge.162.and.ic.le.182.and.jc.eq.76)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.5.and.jc.eq.76)then
        ep=ep+1.22
        endif
c 32 pts
        if(ic.ge.151.and.ic.le.157.and.jc.eq.77)then
        ep=ep+1.22
        endif
        if(ic.ge.162.and.ic.le.182.and.jc.eq.77)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.6.and.jc.eq.77)then
        ep=ep+1.22
        endif
c 32 pts
        if(ic.ge.152.and.ic.le.156.and.jc.eq.78)then
        ep=ep+1.22
        endif
        if(ic.ge.164.and.ic.le.182.and.jc.eq.78)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.8.and.jc.eq.78)then
        ep=ep+1.22
        endif
c 30 pts
        if(ic.ge.151.and.ic.le.156.and.jc.eq.79)then
        ep=ep+1.22
        endif
        if(ic.ge.167.and.ic.le.182.and.jc.eq.79)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.9.and.jc.eq.79)then
        ep=ep+1.22
        endif
c 29 pts
        if(ic.ge.147.and.ic.le.156.and.jc.eq.80)then
        ep=ep+1.22
        endif
        if(ic.ge.171.and.ic.le.182.and.jc.eq.80)then
        ep=ep+1.22
        endif
        if(ic.ge.1.and.ic.le.11.and.jc.eq.80)then
        ep=ep+1.22
        endif
c 29 pts
#endif /*hosing*/

        ep=ep*0.1/86400.

c       if(ic.eq.154.and.jc.eq.70)then
c       write(81,*)'ep = ',ep
c       endif

        snf(2) = -1*( 35*ep * 1d-03 )


#ifdef cmip_flxrd
         snf(2) = surfrd_t_flux(ic,jc,2)
#endif /*cmip_flxrd*/

#ifdef oasis_ice
c the old sea-ice parametrisation - don't use
         if (t(1,ic,jc,1,nm).lt.1.96) then
         stf(1) = 0
         end if
#endif /*oasis_ice*/

#endif

#ifdef oasis_infl
      stf(1)=snf(1)
      stf(2)=snf(2)

#ifdef flux_adj
c flux_adjustments - experimental
      call fluxadjust(stf,ic-nhalo, jc-(nhalo-1), totday)
#endif /*flux_adj*/

#else /*oasis_infl*/

c if not oasis, either restore to monthly levitus/ecmwf/igcm winds (bcest2)
c or annual average zonal levitus (bcest)


#ifdef restor98
      call bcest2(stf, smf, ic-nhalo, jc-(nhalo-1), totday)
#else /*restor98*/
      call bcest(stf,smf, ic, jc)
#endif /*restor98*/
     
     
c  convert salinity to model units.
      stf(2) = (stf(2)-35.0)*0.001
c
c-----------------------------------------------------------------------
c     set no flux condition for tracer at  bottom.
c     relax to "bcest" values with 40 day timescale at top
c-----------------------------------------------------------------------
c
      gamma = secday/40.0


c jods:
#ifdef NONLIN_FREE_SURFACE
      dzh1 = dz(1)+h00(ic,jc,1)
#else /*NONLIN_FREE_SURFACE*/
      dzh1 = dz(1)
#endif /*NONLIN_FREE_SURFACE*/
#ifdef zero_fluxes
      do 100 m=1,nt
        stf(m) = c0
        btf(m) = c0
100   continue
#else /*zero_fluxes*/
      do 100 m=1,nt
        stf(m) = gamma*(stf(m)-t(1,ic,jc,m,nm))*dzh1
        btf(m) = c0
100   continue
#endif /*zero_fluxes*/

#endif
c end of oasis-or-restore #if


c     jods:
c      stf(1) = c0
c      stf(2) = c0
c      smf(1) = c0
c      smf(2) = c0
c
c-----------------------------------------------------------------------
c     set bottom stresses.  bmf is stress acting on the ocean floor
c-----------------------------------------------------------------------
c
      kz = kmu(ic,jc)
      if (kz .ne. 0) then
        uvmag    = sqrt(u(kz,ic,jc,nm)**2 +v(kz,ic,jc,nm)**2)
        bmf(1) = cdbot*u(kz,ic,jc,nm)*uvmag
        bmf(2) = cdbot*v(kz,ic,jc,nm)*uvmag
      else
        bmf(1) = c0
        bmf(2) = c0
      endif
c
c if using oasis, use supplied wind stresses at the top instead
#ifdef oasis

c-----------------------------------------------------------------------
c     set surface stresses.  smf is stress acting on the ocean surface
c      this bit is new, added by B. Sinha 20/11/2000
c-----------------------------------------------------------------------
c
      kz = kmu(ic,jc)
      if (kz .ne. 0) then

         smf(1) = 10.d0*ztaux(ic, jc)
         smf(2) = 10.d0*ztauy(ic, jc)

#ifdef cmip_flxrd
c        smf(1) = surfrd_m_flux(ic,jc,1)
c        smf(2) = surfrd_m_flux(ic,jc,2)
#endif /*cmip_flxrd*/

#ifdef oasis_ice
         if (t(1,ic,jc,1,nm).lt.1.96) then
           smf(1) = c0
           smf(2) = c0
         end if
#endif /*oasis_ice*/
      else
        smf(1) = c0
        smf(2) = c0
      endif
#endif /*oasis_ice*/



c
#ifdef flux_write
c     Store surface flux in array:
      do m=1,nt
        surf_t_flux(ic,jc,m) = stf(m)
        surf_m_flux(ic,jc,m) = smf(m)
#ifdef oasis
        surf_n_flux(ic,jc,m) = snf(m)
#endif
      enddo
#endif
      return
      end
