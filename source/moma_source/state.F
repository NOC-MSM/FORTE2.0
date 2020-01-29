#include "cpp_options.h"
      subroutine state (t, s, rho, kk, ind)
      implicit none
c
c=======================================================================
c
c     state computes one column of normalized densities from the
c     surface to level kk by using a 3rd order polynomial fit to
c     the equation of state.
c
c     note.. for precision purposes, there is a depth dependent
c     constant subtracted from the density returned by this
c     routine.
c     so.. this routine should be used only for calculating  
c     horizontal gradients of density or vertical stability
c     with adjacent levels referred to the same reference level.
c
c     inputs:
c
c*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
c*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
c*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
c
c     t = the input column of temperatures (degrees C)
c
c*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
c*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~
c
c     s = the input column of salinities (units: (ppt-35)/1000)
c     ind = 0 for normal density calculations
c         = 1 for comparing levels 1 to 2, 3 to 4, etc.
c         = 2 for comparing levels 2 to 3, 4 to 5, etc.
c           (if ind = 1 or 2, the coefficients for the lower
c            of the 2 levels are used)
c     kk = bottom level to be calculated
c
c     note: model salinities are equal to (S-35.0)/1000.0 where
c     S is the standard oceanographic salinity.  This convention,
c     which dates from the early Bryan and Cox model, increases 
c     the precision by about 5 binary bits.  This can be 
c     significant in density gradient calculations on 32-bit
c     computers.  The convention is also the one used by the
c     mom/gfdl program used to generate the include file  
c     'dncoef.h'.
c
c     output:
c
c     rho = density with depth level dependent offset 
c           (units: g/cc minus constant)
c
c=======================================================================
c
c
#include "param.h"
c Arguments:      
      _MOMA_REAL t(km),  s(km),  rho(km)
      integer kk,ind
c Local:

      _MOMA_REAL to(km), so(km), c(km,9),
     &          to1(km),so1(km),co1(km,9),
     &          to2(km),so2(km),co2(km,9)
c     'in' has been promoted to an array because it forces it to remain
c     threadprivate for OMP.
      integer in
      common /state_comi/ in
      common /state_comr/ to,so,c,to1,so1,co1,to2,so2,co2
C$OMP THREADPRIVATE( /state_comi/,/state_comr/)
      integer n
      integer k,l,k1,k2
      _MOMA_REAL tq,sq
#include "dncoef.h"
      data in/1/

c
c  on first entry set up coefficients for stability calculations
c
      if(in.eq.1)then
        in=-1
        do k=1,km-1,2
         to1(k)  = to(k+1)
         to1(k+1)= to(k+1)
         so1(k)  = so(k+1)
         so1(k+1)= so(k+1)
         do l=1,9
          co1(k,l)  = c(k+1,l)
          co1(k+1,l)= c(k+1,l)
         enddo
        enddo
        do k=2,km-1,2
         to2(k)  = to(k+1)
         to2(k+1)= to(k+1)
         so2(k)  = so(k+1)
         so2(k+1)= so(k+1)
         do  l=1,9
          co2(k,l)  = c(k+1,l)
          co2(k+1,l)= c(k+1,l)
         enddo
        enddo
      endif
c
      do k=1,km
       rho(k)=1e30
      enddo
c
c  test for errors
c
      if(ind.lt.0.or.ind.gt.2.or.kk.lt.0.or.kk.gt.km)then
        write (stderr,99)ind,kk
        stop '1 state'
c
c  normal entry
c
      elseif(ind.eq.0)then
        do k=1,kk
         tq=t(k)-to(k)
         sq=s(k)-so(k)
         rho(k) = (c(k,1) + (c(k,4) + c(k,7)*sq)*sq +
     &        (c(k,3) + c(k,8)*sq + c(k,6)*tq)*tq)*tq +
     &        (c(k,2) + (c(k,5) + c(k,9)*sq)*sq)*sq
        enddo
c
c  compare 1 with 2, 3 with 4, etc.  density w.r.t bottom level
c
      elseif(ind.eq.1.and.kk.gt.1)then
        do k1=0,kk-2,2
         do  k2=1,2
          k=k1+k2
          tq=t(k)-to1(k)
          sq=s(k)-so1(k)
          rho(k) = (co1(k,1) + (co1(k,4)    + co1(k,7)*sq)*sq
     &         + (co1(k,3) +  co1(k,8)*sq + co1(k,6)*tq)*tq)*tq
     &         + (co1(k,2) + (co1(k,5)    + co1(k,9)*sq)*sq)*sq
         enddo
        enddo
c
c  compare 2 with 3, 4 with 5 etc
c
      elseif(ind.eq.2.and.kk.gt.2)then
        do k1=1,kk-2,2
         do k2=1,2
          k=k1+k2
          tq=t(k)-to2(k)
          sq=s(k)-so2(k)
          rho(k) = (co2(k,1) + (co2(k,4)    + co2(k,7)*sq)*sq
     &         + (co2(k,3) +  co2(k,8)*sq + co2(k,6)*tq)*tq)*tq
     &         + (co2(k,2) + (co2(k,5)    + co2(k,9)*sq)*sq)*sq
         enddo
        enddo
      endif
c
      return
   99 format(/,' Routine state. Variables "ind" or "kk" out',
     &       ' of range',/,' ind =',i10,' kk =',i10,/,
     &       ' Program stopping ... ')
      end
