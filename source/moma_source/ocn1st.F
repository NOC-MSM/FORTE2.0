#include "cpp_options.h"
      subroutine ocn1st
      implicit none
c
c=======================================================================
c
c  initialize quantites when model first started
c
c=======================================================================
c
#include "param.h"
#include "coord.h"
#include "ctmngr.h"
#include "cvbc.h"
#include "frees.h"
#include "grdvar.h"
#include "iounit.h"
#include "levind.h"
#include "scalar.h"
#include "slabs.h"
#include "switch.h"
c External function:
      _MOMA_REAL theta0
      external theta0
c Local:
      _MOMA_REAL sconst
      _MOMA_REAL thetaread(imt,jmt,km),saltread(imt,jmt,km)
      integer i,j,k,n
c
c-----------------------------------------------------------------------
c     generate topography over "t" points
c-----------------------------------------------------------------------
c
#ifdef simple_world
      call topog
#else
      call read_kmt
#endif
c
c-----------------------------------------------------------------------
c     initialize t,s,u,v.
c          velocities set to zero
c          temperature and salinity set to -2.0 and 45.0 over land
c                 and to theta0(latitude,depth) and 34.9 at sea
c-----------------------------------------------------------------------
c

      write(6,*) " hello there ! ",imt,jmt,km
      open(99,file="levitemp.dat")
      open(98,file="levisalt.dat")
      do k=1,15
c     do j=1,43
      do j=1,88
      do i=1,180
      read(99,*)thetaread(i+2,j+1,k)
      read(98,*)saltread(i+2,j+1,k)
      end do
      thetaread(1,j+1,k)=thetaread(181,j+1,k)
      thetaread(2,j+1,k)=thetaread(182,j+1,k)
      thetaread(183,j+1,k)=thetaread(3,j+1,k)
      thetaread(184,j+1,k)=thetaread(4,j+1,k)
      saltread(1,j+1,k)=saltread(181,j+1,k)
      saltread(2,j+1,k)=saltread(182,j+1,k)
      saltread(183,j+1,k)=saltread(3,j+1,k)
      saltread(184,j+1,k)=saltread(4,j+1,k)
      end do
      do i=1,184
      thetaread(i,1,k)=thetaread(i,2,k)
      thetaread(i,90,k)=thetaread(i,89,k)
      saltread(i,1,k)=saltread(i,2,k)
      saltread(i,90,k)=saltread(i,89,k)
      end do
      end do
      close(98)
      close(99)
      sconst=0.0349
      do n=1,3
       do j=1,jmt
        do i=1,imt
         do k=1,km
          u(k,i,j,n)=c0
          v(k,i,j,n)=c0
          if(k.gt.kmt(i,j))then
           t(k,i,j,1,n)=-2.0
           t(k,i,j,2,n)=0.01
          else
c          t(k,i,j,1,n)= theta0 (yt(j), zt(k))
c          t(k,i,j,2,n)= sconst-0.035
           t(k,i,j,1,n)= thetaread(i,j,k)
           t(k,i,j,2,n)= (saltread(i,j,k)-35.)/1000.
c          if(abs(thetaread(i,j,k)).gt.1.e9)t(k,i,j,1,n)=2.
c          if(abs(thetaread(i,j,k)).gt.1.e2)write(6,*)i,j,k,"!",
c    1   thetaread(i,j,k)
c          if(abs(saltread(i,j,k)).gt.1.e9)t(k,i,j,2,n)=0.
          endif
c     if(k.eq.1 .and. n.eq.1)write(6,*)t(k,i,j,1,n)
         enddo
        enddo
       enddo
      enddo
c
c-----------------------------------------------------------------------
c     initialize free surface model
c-----------------------------------------------------------------------
c
      do n=1,3
       do j=1,jmt
        do i=1,imt
         h0(i,j,n) = c0
         u0(i,j,n) = c0
         v0(i,j,n) = c0
        enddo
       enddo
      enddo

c
c-----------------------------------------------------------------------
c     initialize controls
c-----------------------------------------------------------------------
c
      itt   = 0
      totsec = c0
      totday = c0
      years  = c0
c
      return
      end
