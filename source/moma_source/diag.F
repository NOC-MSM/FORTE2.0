#include "cpp_options.h"
      subroutine diag( ic, jc)
      implicit none
c
c=============================================================
c  collect timestep statistics and (if diagts is set) statistics
c  for diagnostic timestep.
c  note 1. this routine sums arrays, it may not vectorise well
c       2. diagnostic timestep option not implemented yet
c=============================================================
c
c-----------------------------------------------------------------
c         accumulate kinetic energy 
c-----------------------------------------------------------------
c
c
#include "param.h"
c
#include "scalar.h"
#include "switch.h"
#include "grdvar.h"
#include "levind.h"
#include "timelv.h"
#include "slabs.h"
c
#include "cdiag.h"
c
c In...
      integer ic,jc
c Local:
      integer ku,kt,k,m
      _MOMA_REAL boxa,boxv
      ku = kmu(ic,jc)
      kt = kmt(ic,jc)
      if(ku.ne.0)then
        boxa = dx*csu(jc)*dy
        do 100 k=1,ku 
        boxv  = boxa*dz(k)
        ektot = ektot + (u(k,ic,jc,nc)**2 + v(k,ic,jc,nc)**2)
     &                  *p5*boxv
 100    continue
      endif
c
c-----------------------------------------------------------------
c         accumulate tracer change statistics 
c-----------------------------------------------------------------
c
      if(kt.ne.0)then
        boxa = cst(jc)*dy*dx/c2dtts
        do m=1,nt
         do k=1,kt
          boxv    = boxa*dz(k)
          tddt(m)= tddt(m)
     &         + (t(k,ic,jc,m,np)-t(k,ic,jc,m,nm))*boxv
          dtabs(m)= dtabs(m)
     &         + abs(t(k,ic,jc,m,np)-t(k,ic,jc,m,nm))*boxv
          tvar(m) = tvar(m)+(t(k,ic,jc,m,np)**2
     &         -t(k,ic,jc,m,nm)**2)*boxv
         ENDDO
        ENDDO     
       endif
      return
      end
