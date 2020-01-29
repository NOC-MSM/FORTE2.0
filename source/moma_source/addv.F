#include "cpp_options.h"
      subroutine addv( ic, jc)
      implicit none
c
c=======================================================================
c
c  add barotropic to baroclinic velocities
c
c=======================================================================
c
#include "param.h"
c
#include "frees.h"
#include "levind.h"
#include "timelv.h"
#include "slabs.h"
#include "grdvar.h"
#include "coord.h"
c <!-- jods
#include "scalar.h"
c jods -->

c     Arguments:
      integer ic,jc
c Local:
      integer kmc,ip,jp,k
      _MOMA_REAL ubar,vbar,sumu,sumv,zwkmc,zwkr,dzh1,dzhr1

#ifndef OLD_NON_CONSERVING          
      integer m,n,ks
# ifdef NONLIN_FREE_SURFACE
      _MOMA_REAL dz1
# endif    
#endif

c=*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*=
c     N.B. For inlining purposes, line numbers are of the form 2**
c=*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*= =*=
c
      if (kmu(ic,jc) .gt. 0) then
       ubar=u0(ic,jc,np0)
       vbar=v0(ic,jc,np0)
c
c=================================================================
c
c  remove vertical mean velocity using updated height of top box
c  and add barotropic to baroclinic velocities
c
c=================================================================
c
c (Assumes that limit_h0 is not defined as well as variable_bbox)
       kmc=kmu(ic,jc)
       ip=ic+1
       jp=jc+1

#ifndef NONLIN_FREE_SURFACE
       zwkmc = zw(kmc)
       zwkr  = c1/zwkmc
       dzh1   = dz(1)
       dzhr1  = c1/dz(1)
#else /* NONLIN_FREE_SURFACE */
       zwkmc = zw(kmc)+p25*
     &      (h0(ic,jp,np0)+h0(ip,jp,np0)+h0(ip,jc,np0)+h0(ic,jc,np0))
       zwkr = c1/zwkmc
       dzh1 = dz(1)+p25*
     &      (h0(ic,jp,np0)+h0(ip,jp,np0)+h0(ip,jc,np0)+h0(ic,jc,np0))
#endif /* NONLIN_FREE_SURFACE */

c     Get mean velocity
       sumu = u(1,ic,jc,np)*dzh1
       sumv = v(1,ic,jc,np)*dzh1
       do 200 k=2,kmc
        sumu = sumu + u(k,ic,jc,np)*dz(k)
        sumv = sumv + v(k,ic,jc,np)*dz(k)
 200   continue
       sumu = sumu*zwkr
       sumv = sumv*zwkr
       do 220 k=1,kmc
        u(k,ic,jc,np) = u(k,ic,jc,np) - sumu + ubar
        v(k,ic,jc,np) = v(k,ic,jc,np) - sumv + vbar
 220   continue
c     
      endif
c
#ifdef NONLIN_FREE_SURFACE
c     
c=================================================================
c   correct tracers with new depth for top box                    
c=================================================================
c    
       dzhr1 = c1/(dz(1)+h0(ic,jc,np0)) 
       do 240 m = 1,nt      
        t(1,ic,jc,m,np) = t(1,ic,jc,m,np)*(dz(1)+h00(ic,jc,1))*dzhr1 
 240   continue 

#endif /*  NONLIN_FREE_SURFACE */

#ifndef OLD_NON_CONSERVING             
c
c=======================================================================
c   convectively adjust water column if gravitationally unstable.
c=======================================================================
c Moved from TRACER to here
c
      kmc= kmt(ic,jc)
      if(kmc.gt.1.and.ncon.gt.0)then
#ifdef conv_write
        do k=1,kmc
          conv(k,ic,jc) = 0
        enddo
#endif
       do  n=1,ncon
        do  ks=1,2
         call state(t(1,ic,jc,1,np),t(1,ic,jc,2,np),temp(1),
     &        kmc, ks)
         do  m=1,nt
          do  k=ks,kmc-1,2
           if(temp(k).gt.temp(k+1))then
#ifdef conv_write
             if( m.eq.1) then
               conv(k,ic,jc) = conv(k,ic,jc)+1
               conv(k+1,ic,jc) = conv(k+1,ic,jc)+1
             endif
#endif
#ifdef NONLIN_FREE_SURFACE
            if (k .eq. 1) then  
             dz1 = dz(1)+h0(ic,jc,np0) 
             t(k,ic,jc,m,np) = (dz1*t(k,ic,jc,m,np) + 
     &            dz(k+1)*t(k+1,ic,jc,m,np))/(dz1+dz(k+1)) 
             t(k+1,ic,jc,m,np) = t(k,ic,jc,m,np) 
            else                
#endif
             t(k,ic,jc,m,np) = (dz(k)*t(k,ic,jc,m,np) +
     &            dz(k+1)*t(k+1,ic,jc,m,np))*dzsr(k)
             t(k+1,ic,jc,m,np) = t(k,ic,jc,m,np)
#ifdef NONLIN_FREE_SURFACE
            endif               
#endif
           endif
          enddo                 !260 k
         enddo                  !260 m
        enddo                   !260 ks
       enddo                    !260 n
      endif
#endif /* OLD_NON_CONSERVING */
      return
      end
      
