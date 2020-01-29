#include "cpp_options.h"
      subroutine invtri (z, topbc, botbc, dcb, tdt,  ic,jc,kmc)
      implicit none
c
c
c
c=======================================================================
c     solve the vertical diffusion equation implicitly using the
c     method of inverting a tridiagonal matrix as described in
c     Numerical Recipies in Fortran, The art of Scientific Computing,
c     Second Edition, Press, Teukolsky, Vetterling, Flannery, 1992
c     pages 42,43.
c     this routine assums that the variables are defined at grid points
c     and the top and bottom b.c. are flux conditions.
c
c     inputs:
c     z         = right hand side terms
c     topbc     = top boundary condition
c     botbc     = bottom boundary condition
c     dcb       = vertical mixing coeff at base of cell
c     tdt       = 2 * timestep
c
c     outputs:
c     z         = returned solution
c
c     author:   Anand Gnanadesikan    a1g@gfdl.gov
c=======================================================================
c
#include "param.h"
#include "grdvar.h"
#include "chmix.h"
#include "levind.h"
#include "frees.h"
      _MOMA_REAL z(km), topbc,botbc,tdt,dcb(km)
      integer ic,jc,kmc
#ifdef isopycmix
c      kmc=kmt(ic,jc)
c
      _MOMA_REAL a(km),b(km)
     & , c(0:km),e(km),f(0:km)   !Not used ?: ,d(km)
     & , bet,eps,factu,factl,dzk1

      common /invtri_com/ a,b,c,e,f
C$OMP THREADPRIVATE( /invtri_com/ )      

      integer k,km1
c     integer kp1  !notused?
c
c       dzur(k) = c1/(dzw(k-1)*dz(k))
c       dzlr(k) = c1/(dzw(k)*dz(k)) 
c

#ifdef NONLIN_FREE_SURFACE
      dzk1 = dz(1) + h00(ic,jc,1)
#else
      dzk1 = dz(1)
#endif
      eps = 1.e-30

c This routine will only be called if kmc > 0
c
      km1 = 1
      factu = dzur(1)*tdt*aidif
      factl = (c1/(dzw(1)*dzk1))*tdt*aidif
      a(1)  = c0
      c(1) = -dcb(1)*factl
      b(1)  = c1 - a(1) - c(1)
      f(1) = z(1)
      
      do 20 k=2,kmc
        km1 = max(1,k-1)
        factu = dzur(k)*tdt*aidif
        factl = dzlr(k)*tdt*aidif
        a(k) = -dcb(km1)*factu   
        c(k) = -dcb(k)*factl
        f(k) = z(k)
        b(k) = c1 - a(k) - c(k)
 20   continue

      c(kmc) = c0
      b(kmc) = c1 - a(kmc) - c(kmc)
c     
c     top and bottom b.c.
c     
      f(1)  = z(1) + topbc*tdt*aidif/dzk1
      k=max(2,kmc)
      f(k)   = z(k) - botbc*tdt*dzr(k)*aidif
c     
c     decomposition and forward substitution
c     
      bet = c1/(b(1) + eps)
      z(1) = f(1)*bet
      do 40 k=2,kmc
        e(k) = c(k-1)*bet
        bet = c1/(b(k) - a(k)*e(k) + eps)
        z(k) = (f(k) - a(k)*z(k-1))*bet
 40   continue
c     
c     back substitution
c     
      do 60 k=kmc-1,1,-1
        z(k) = z(k) - e(k+1)*z(k+1)
 60   continue

      return
#endif /* isopycmix */
      end
