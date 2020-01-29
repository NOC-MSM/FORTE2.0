#include "cpp_options.h"
      subroutine stream
      implicit none
c-------------------------------------------------------
c Subroutine for calculating meridional streamfunction
c
c marc 11.10.2001
c-------------------------------------------------------

c#include "iounit.h"
c#include "pconst.h"
#include "param.h"
#include "ctmngr.h"
#include "frees.h"
#include "iounit.h"
#include "levind.h"
#include "slabs.h"
#include "switch.h"
#include "timelv.h"
#include "grdvar.h"
#include "coord.h"
c <--jods - for time steps
#include "scalar.h"
c -->
      _MOMA_REAL psi(km,jmt),phim(km,jmt),maxs,boxvolume
     & , totheat,totvolume,totsal
     & , mint1,maxt1,mint2,maxt2,minh,maxh,minu,maxu,minv,maxv
     & , cflmax
      integer i,j,k
      _MOMA_REAL VERYHIGH,VERYLOW
      parameter(VERYHIGH=1.d31,VERYLOW= -1.d31)
#ifdef w_write
      _MOMA_REAL minw,maxw,cflwmax
#endif
      integer ioINFO, ioMINMAX
      parameter(ioINFO = 10, ioMINMAX = 20)

      if (first) then
       open(ioINFO,file='stream_info.dat',form='formatted')       
       open(ioMINMAX,file='stream_minmax.dat',form='formatted')
      endif

      if (streamts) then
       do k=1,km
        do j=1,jmt
         phim(k,j)=0.
         psi(k,j)=0.
        enddo
       enddo
c
c     First integrate zonally
c
       do j=jcbeg,jcend
         do i=icbeg,icend
           do k=1,km
             phim(k,j)=(v(k,i,j,np)+v(k,i-1,j,np))
     &            *csu(j)*dx*p5*dz(k) + phim(k,j)
           enddo
         enddo
       enddo

c     Integrate over depth and find maximum
       maxs= VERYLOW
       do j=jcbeg,jcend
        psi(km,j) = -phim(km,j)
        do k=km-1,1,-1
         psi(k,j) = psi(k+1,j) - phim(k,j)
         maxs=max(psi(k,j),maxs)
        enddo
       enddo

c     convert Maximum to Sv
       maxs = maxs*1.e-12

c------------------------------------------------------------------------
c     Marc 10-12-2001 Total heat.
c     jods 25-02-02   And salinity
c     And total volume (mass)
c
c------------------------------------------------------------------------
       mint1 = VERYHIGH
       maxt1 = VERYLOW
       mint2 = VERYHIGH
       maxt2 = VERYLOW
       minh =  VERYHIGH
       maxh =  VERYLOW
       minu =  VERYHIGH
       maxu =  VERYLOW
       minv =  VERYHIGH
       maxv =  VERYLOW
#ifdef w_write
       minw =  VERYHIGH
       maxw =  VERYLOW
#endif
       
       totheat   = c0 
       totsal    = c0
       totvolume = c0
c     Free surface component:
       do j=jcbeg,jcend
        do i=icbeg,icend
         if(kmt(i,j).gt.0) then
          boxvolume=dx*dy*cst(j)*h0(i,j,np0)
#ifdef NONLIN_FREE_SURFACE
          totheat  =totheat   + boxvolume*t(1,i,j,1,np)
          totsal   =totsal    + boxvolume*t(1,i,j,2,np)
#endif
          totvolume=totvolume + boxvolume
          minh = min(h0(i,j,np0),minh)
          maxh = max(h0(i,j,np0),maxh)
         endif 
        enddo
       enddo
       do j=jcbeg,jcend
        do i=icbeg,icend
         do k=1,kmt(i,j)
          boxvolume   =dx*dy*cst(j)*dz(k)
          totheat  =totheat   + boxvolume*t(k,i,j,1,np)
          totsal   =totsal    + boxvolume*t(k,i,j,2,np)
c          totvolume=totvolume + boxvolume
          mint2 = min(t(k,i,j,2,np),mint2)
          maxt2 = max(t(k,i,j,2,np),maxt2)
          mint1 = min(t(k,i,j,1,np),mint1)
          maxt1 = max(t(k,i,j,1,np),maxt1)
         enddo
        enddo
       enddo

c Velocities & cfl
       do j=jcbeg,jcend
        do i=icbeg,icend
         do k=1,kmu(i,j)
          minu  = min(u(k,i,j,np),minu)
          maxu  = max(u(k,i,j,np),maxu)
          minv  = min(v(k,i,j,np),minv)
          maxv  = max(v(k,i,j,np),maxv)
#ifdef w_write
          minw =  min(w(k,i,j),minw)
          maxw =  max(w(k,i,j),maxw)
#endif
         enddo
        enddo
       enddo

c Approx cfl for speed - max velocity / spacing
       cflmax = max(maxu,-minu,maxv,-minv)
     &      *max(dtts,dtuv,dtbt)/min(dx,dy)
#ifdef w_write
       cflwmax = max(maxw,-minw)
     &      *max(dtts,dtuv,dtbt)*dzr(1)
#endif 

       write(ioINFO,1010) totday , maxs, 
     &      totheat,totsal,totvolume
c     &      ,t(15,15,15,1,np),t(1,15,15,1,np),
c     &      t(15,15,15,2,np),t(1,15,15,2,np),
c     &      u(15,15,15,np),u(1,15,15,np),
c     &      v(15,15,15,np),v(1,15,15,np)
       write(ioMINMAX,1010) totday,mint1,maxt1,mint2,maxt2,minh,maxh
     &      ,minu,maxu,minv,maxv,cflmax
#ifdef w_write
     &      ,minw,maxw,cflwmax
#endif
c       call flush(ioINFO)
c       call flush(ioMINMAX)
      endif !if streamts

      if(last) then
        close(ioINFO)
        close(ioMINMAX)
      endif

 1010 format(100e25.16)      
      return
      end



