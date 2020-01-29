#include "cpp_options.h"
#ifndef inline_step
c only include the header if this file is being compiled stand alone
      subroutine frees(ic,jc)
      implicit none
c Arguments:      
      integer ic,jc
#endif
#if defined inline_step || !defined inline
c include this bit if not inline 
#ifndef inline_step
c
c=======================================================================
c
c  Solution of the standard shallow water equations.
c
c  This is an improvement of "Killworth, P.D., Stainforth, D.,
c  Webb, D.J. and Paterson, S.M. (1989).  A free surface
c  Bryan-Cox-Semtner model.  Institute of Oceanographic
c  Sciences, Report No. 270.  Wormley, Godlaming, U.K.. 184pp."
c
c  Later modified to use a leapfrog and time smoothing scheme,
c  suggested by Semtner, to prevent instabilities near the equator
c  due to aliasing of barotropic waves with periods equal to the
c  baroclinic timestep. 
c
c=======================================================================
c
c-----------------------------------------------------------------------
c  define global data
c-----------------------------------------------------------------------
c
#include "param.h"
c
#include "chmix.h"
#include "ctmngr.h"
#include "frees.h"
#include "grdvar.h"
#include "levind.h"
#include "scalar.h"
#include "switch.h"
#include "timelv.h"
c Local:
      integer ip,im,jp,jm
      _MOMA_REAL boxar,dhdt,fxa,fxb,temp1,temp2
#ifdef free_eb
      _MOMA_REAL fac1,fac2,ustar,vstar
#else
      _MOMA_REAL fx
#endif
      _MOMA_REAL dpdx0,dpdy0
#ifdef de_checkbd
      integer m_n,m_s,m_e,m_w,m_ne,m_se,m_sw,m_nw
      _MOMA_REAL delplus,delcross
#endif

#endif /*ifndef inline_step*/
c
c
c-----------------------------------------------------------------------
c  set indices
c-----------------------------------------------------------------------
c
      ip=ic+1
      im=ic-1
      jp=jc+1
      jm=jc-1
c
c-----------------------------------------------------------------------
c  timestep height field.
c-----------------------------------------------------------------------
c
      if(kmt(ic,jc).ne.0)then
        boxar = dxr*dyr*cstr(jc)
#ifdef de_checkbd
c
c Calculate masks for the del-cross and del-plus operators
c on selected time steps for free-surface calculation
c
       lchkbd = itbt.eq.2
       if (lchkbd) then
         m_N  = min(1,kmt(ic,jp))
         m_S  = min(1,kmt(ic,jm))
         m_E  = min(1,kmt(ip,jc))
         m_W  = min(1,kmt(im,jc))
         m_NE = min(1,kmu(ic,jc))
         m_SE = min(1,kmu(ic,jm))
         m_SW = min(1,kmu(im,jm))
         m_NW = min(1,kmu(im,jc))
c
c Calculate the del-plus operator (x2 for inclusion within existing 
c terms of the dhdt expression)
c
#ifdef OLD_NON_CONSERVING
c     jods
c Calculate the del-plus operator (x2 for inclusion within existing 
c terms of the dhdt expression)
         delplus = c2*(m_E*(h0(ip,jc,nm0) - h0(ic,jc,nm0))
     &                -m_W*(h0(ic,jc,nm0) - h0(im,jc,nm0))
     &                +m_N*(h0(ic,jp,nm0) - h0(ic,jc,nm0))
     &                -m_S*(h0(ic,jc,nm0) - h0(ic,jm,nm0)))
c
c Calculate the del-cross operator (x2 for inclusion within existing
c terms of the dhdt expression)
         delcross = ( m_NE*(h0(ip,jp,nm0) - h0(ic,jc,nm0))
     &               -m_SW*(h0(ic,jc,nm0) - h0(im,jm,nm0))
     &               +m_NW*(h0(im,jp,nm0) - h0(ic,jc,nm0))
     &               -m_SE*(h0(ic,jc,nm0) - h0(ip,jm,nm0)))
#else /*OLD_NON_CONSERVING */ 
c occam Version:
c Calculate the del-plus operator (x2 for inclusion within existing 
c terms of the dhdt expression)
       delplus = c2*(m_E*(h0(ip,jc,nm0) - h0(ic,jc,nm0))*cst(jc)    
     &              +m_W*(h0(im,jc,nm0) - h0(ic,jc,nm0))*cst(jc)    
     &              +m_N*(h0(ic,jp,nm0) - h0(ic,jc,nm0))*csu(jc)    
     &              +m_S*(h0(ic,jm,nm0) - h0(ic,jc,nm0))*csu(jm))   
c Calculate the del-cross operator (x2 for inclusion within existing
c terms of the dhdt expression)
       delcross = ( m_NE*(h0(ip,jp,nm0) - h0(ic,jc,nm0))*csu(jc)    
     &             +m_SW*(h0(im,jm,nm0) - h0(ic,jc,nm0))*csu(jm)    
     &             +m_NW*(h0(im,jp,nm0) - h0(ic,jc,nm0))*csu(jc)    
     &             +m_SE*(h0(ip,jm,nm0) - h0(ic,jc,nm0))*csu(jm))   
#endif /* OLD_NON_CONSERVING */
       endif
c
c Need a constant of the form: 
c    WGHT = alpha*grav*Hmax*dtbt/(dphi*dlambda*radius*radius*cos(phi)) 
c    i.e.WGHT = alpha*grav*Hmax*dtbt*boxar = dchkbd*boxar
c
# endif /*de_checkbd*/
#ifndef NONLIN_FREE_SURFACE
c jods : old version:
        dhdt  = (dy*(u0(im,jm,nc0)*h(im,jm)+u0(im,jc,nc0)*h(im,jc))
     &           -dy*(u0(ic,jm,nc0)*h(ic,jm)+u0(ic,jc,nc0)*h(ic,jc))
     & +dx*(csu(jm)*(v0(im,jm,nc0)*h(im,jm)+v0(ic,jm,nc0)*h(ic,jm))
     &     -csu(jc)*(v0(im,jc,nc0)*h(im,jc)+v0(ic,jc,nc0)*h(ic,jc))
     &     ))*p5*boxar
#else
c jods : Version new, from occam...
      dhdt =
     & (dy*((u0(im,jm,nc0)*(h(im,jm)+p5*(h0(ic,jc,nc0)+h0(im,jc,nc0)))  
     &      +u0(im,jc,nc0)*(h(im,jc)+p5*(h0(ic,jc,nc0)+h0(im,jc,nc0)))) 
     &     -(u0(ic,jm,nc0)*(h(ic,jm)+p5*(h0(ic,jc,nc0)+h0(ip,jc,nc0)))  
     &      +u0(ic,jc,nc0)*(h(ic,jc)+p5*(h0(ic,jc,nc0)+h0(ip,jc,nc0)))))
     &  +dx*(csu(jm)*                                                   
     &      (v0(im,jm,nc0)*(h(im,jm)+p5*(h0(ic,jc,nc0)+h0(ic,jm,nc0)))  
     &      +v0(ic,jm,nc0)*(h(ic,jm)+p5*(h0(ic,jc,nc0)+h0(ic,jm,nc0)))) 
     &      -csu(jc)*                                                   
     &      (v0(im,jc,nc0)*(h(im,jc)+p5*(h0(ic,jc,nc0)+h0(ic,jp,nc0)))  
     &      +v0(ic,jc,nc0)*(h(ic,jc)+p5*(h0(ic,jc,nc0)+h0(ic,jp,nc0)))))
     & )*p5*boxar
#endif

# ifdef de_checkbd
       if(lchkbd) then
        dhdt=dhdt+dchkbd*(delplus-delcross)*p5*boxar
       endif
# endif
c
        h0(ic,jc,np0) = h0(ic,jc,nm0) + c2dtbt*dhdt

#ifndef free_eb
c
c-----------------------------------------------------------------------
c  Calculate time averages (except for first pass of first timestep
c                           when itbtp equals zero)
c-----------------------------------------------------------------------
c
        if(itbtp.ne.0)then
          freeav(1,ic,jc) = freeav(1,ic,jc) + h0(ic,jc,np0)
          if(itbt.eq.ntbt2)then
            fx = p5/dble(ntbt)
            h0(ic,jc,np0) = freeav(1,ic,jc)*fx
          endif
        endif
#endif
      endif
c
c-----------------------------------------------------------------------
c  timestep velocity fields using semi-implicit scheme
c  for the coriolis term.
c-----------------------------------------------------------------------
c
      if(kmu(ic,jc).ne.0)then
c
c-----------------------------------------------------------------------
c    compute pressure gradients
c-----------------------------------------------------------------------
c
        fxa = grav*dx2r*csur(jc)
        fxb = grav*dy2r
        temp1 = h0(ip,jp,nc0) - h0(ic,jc,nc0)
        temp2 = h0(ic,jp,nc0) - h0(ip,jc,nc0)
        dpdx0  = (temp1-temp2)*fxa
        dpdy0  = (temp1+temp2)*fxb
#ifdef free_eb
c
c-----------------------------------------------------------------------
c  euler backward scheme treats coriolis term implicitly
c-----------------------------------------------------------------------
c
        fac1=fcor(jc)*c2dtbt*p5
        fac2=c1/(c1+fac1*fac1)
        ustar = u0(ic,jc,nm0) + c2dtbt*(zu(ic,jc) - dpdx0)
     &                        + fac1*v0(ic,jc,nm0)
        vstar = v0(ic,jc,nm0) + c2dtbt*(zv(ic,jc) - dpdy0)
     &                        - fac1*u0(ic,jc,nm0)
        u0(ic,jc,np0) = (ustar + fac1*vstar)*fac2
        v0(ic,jc,np0) = (vstar - fac1*ustar)*fac2
#else
c
c-----------------------------------------------------------------------
c  leapfrog scheme (explicit matsuno/euler-backward
c                   on first mixing timestep)
c-----------------------------------------------------------------------
c
        u0(ic,jc,np0) = u0(ic,jc,nm0) + c2dtbt*(zu(ic,jc) - dpdx0
     &                        + fcor(jc)*v0(ic,jc,nc0) )
        v0(ic,jc,np0) = v0(ic,jc,nm0) + c2dtbt*(zv(ic,jc) - dpdy0
     &                        - fcor(jc)*u0(ic,jc,nc0) )
c
c-----------------------------------------------------------------------
c  Calculate time averages (except for first pass of matsuno/
c                           euler-backward when itbtp equals zero)
c-----------------------------------------------------------------------
c
        if(itbtp.ne.0)then
          freeav(2,ic,jc) = freeav(2,ic,jc) + u0(ic,jc,np0)
          freeav(3,ic,jc) = freeav(3,ic,jc) + v0(ic,jc,np0)
          if(itbt.eq.ntbt2)then
            fx = p5/dble(ntbt)
            u0(ic,jc,np0) = freeav(2,ic,jc)*fx
            v0(ic,jc,np0) = freeav(3,ic,jc)*fx
          endif
        endif
#endif
      endif
c
c defined inline1 || !defined inline :
#endif

#ifndef inline_step
      return
      end
c inline_step
#endif
