#include "cpp_options.h"
      subroutine grids
      implicit none
c=======================================================================
c
c     calculate all grid related items
c
c=======================================================================
c
#include "param.h"
#include "scalar.h"
#include "coord.h"
#include "grdvar.h"
c Local VAriables:
      integer i,j,k
      _MOMA_REAL sumdy
c
c-----------------------------------------------------------------------
c     set latitudes & longitudes of "t" & "u,v" grid points (in degrees)
c-----------------------------------------------------------------------
c
      do i=1,imt
       xt(i) = stlon + (dble(i)-p5)*dxdeg
       xu(i) = stlon + dble(i)*dxdeg
      enddo
c
      do j=1,jmt
       yt(j) = stlat + (dble(j)-p5)*dydeg
       yu(j) = stlat + dble(j)*dydeg
      enddo
c
c     convert to centimeters
c
      dy = dydeg*radius/radian
      dx = dxdeg*radius/radian
c
c-----------------------------------------------------------------------
c     compute auxiliary arrays
c-----------------------------------------------------------------------
c
      do k=1,km
       dzr(k)  = c1/dz(k)
       c2dz(k) = c2*dz(k)
       dz2r(k) = c1/c2dz(k)
c     <!-- jods    
       dz4r(k) = c1/(c4*dz(k))
       if (k .lt. km) then                                              
        dzsr(k)=c1/(dz(k)+dz(k+1))                                     
       endif                                                            
c     jods -->
      enddo
c     
      dzw(0) = p5*dz(1)
      zw(1)  = dz(1)
      do k=2,km
       dzw(k-1) = p5*(dz(k-1)+dz(k))
       zw(k)    = zw(k-1)+dz(k)
      enddo
c     
      dzw(km)   = p5*dz(km)
      dzwr(km)  = c1/dzw(km)
      dzw2r(km) = p5/dzw(km)
      zt(1)     = dzw(0)     
c
      do k=1,km
       dzwr(k-1)  = c1/dzw(k-1)
       dzw2r(k-1) = p5/dzw(k-1)
       zt(k+1)  = zt(k) + dzw(k)
c jods:
c     dzur(k)= upper diffusion grid factor = 1.0/(dzw(k-1)*dz(k))
c     dzlr(k)= lower diffusion grid factor = 1.0/(dzw(k)*dz(k))
       dzur(k) = c1/(dzw(k-1)*dz(k))
       dzlr(k) = c1/(dzw(k)*dz(k)) 
c end jods
      enddo
c
      dxr = c1/dx
      dx2r = p5/dx
      dx4r = p25/dx
c
      dyr = c1/dy
      dy2r = p5/dy
      dy4r = p25/dy
c
      phi(1)   = yu(1)/radian
      phit(1)  = phi(1) - p5*dydeg/radian
      sumdy    = phi(1)
c
      do j=1,jmt
       if (j .ne. jmt) sumdy = sumdy + dydeg/radian
       if (j .ne. jmt) phi(j+1) = sumdy
       if (j .ne. 1) phit(j) = p5*(phi(j-1)+phi(j))
       cst(j)  = cos(phit(j))
       csu(j)  = cos(phi (j))
       sine(j) = sin(phi(j))
       cstr(j) = c1/cst(j)
       csur(j) = c1/csu(j)
       tng(j)  = sine(j)/csu(j)
       tanra(j)= tng(j)/radius
      enddo
c
c-----------------------------------------------------------------------
c     print grid related arrays
c-----------------------------------------------------------------------
c
c
      write (stdout,9101)
      write (stdout,9001) dz
      write (stdout,9102)
      write (stdout,9001) dzw
      write (stdout,9103)
      write (stdout,9001) zw
      write (stdout,9104)
      write (stdout,9001) zt
      write (stdout,9105)
      write (stdout,9001) yt
      write (stdout,9106)
      write (stdout,9001) yu
      write (stdout,9107)
      write (stdout,9001) xt
      write (stdout,9108)
      write (stdout,9001) xu
c
      return
c
9001  format (1x,10g13.5)
9101  format (/,' "t" grid box thickness (cm): "dz(k) k=1..km"')
9102  format (/,' "w" grid box thickness (cm): ',
     &        '"dzw(k) k=0..km"')
9103  format (/,' depth to "t" box bottom (cm): "zw(k) k=1..km"')
9104  format (/,' depth to "t" & "u,v" grid points (cm): "zt(k) ',
     &        'k=1..kmp1"')
9105  format (/,' latitude of t,s points (deg): "yt(j) j=1,jmt"')
9106  format (/,' latitude of u,v points (deg): "yu(j) j=1,jmt"')
9107  format (/,' longitude of t,s points (deg): "xt(i) i=1,imt"')
9108  format (/,' longitude of u,v points (deg): "xu(i) i=1,imt"')
      end
