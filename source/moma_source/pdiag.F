#include "cpp_options.h"
      subroutine pdiag
      implicit none
c
c=======================================================================
c     write diagnostics
c=======================================================================
c
#include "param.h"
#include "coord.h"
#include "ctmngr.h"
#include "frees.h"
#include "timelv.h"
#include "slabs.h"
c Local VAriables:
      _MOMA_REAL ptemp(imt,km),fx,scl
      character trname(nt)*12
      integer i,k,jlat,jj,is,ie,js,je,ks,ke,m
c
c=======================================================================
c based on MOM common "cprnts.h" - only used by this routine
c
      integer nlatpr
      parameter (nlatpr=10)
c
      _MOMA_REAL prlat(nlatpr), prslon(nlatpr), prelon(nlatpr)
     &,         prsdpt(nlatpr), predpt(nlatpr)
     & ,      slatxy, elatxy, slonxy, elonxy
c
c     default starting & ending coordinates for matrix prints during
c     diagnostic time steps (lats & lons in degrees, depths in cm)
c     see "cprnts.h"
c
      data prlat /-60.0, 0.0, 27.0, 55.0, 6*100.0/
      data prslon /180.0, 180.0, 180.0, 180.0, 6*0.0/
      data prelon /250.0, 250.0, 250.0, 250.0, 6*0.0/
      data prsdpt /nlatpr*0.0/
      data predpt /nlatpr*6000.e2/
      data slatxy, elatxy, slonxy, elonxy /-90.0, 90.0, 3.0, 357.0/
c=======================================================================
      data trname/'temperature ','salinity    '/
c
c-----------------------------------------------------------------------
c       print the solution at specified latitudes
c-----------------------------------------------------------------------
c
      do 700 jlat=1,nlatpr
      jj = indp (prlat(jlat), yt, jmt)
      if (prlat(jlat) .le. yt(jmt)) then
        if (jlat .eq. 1) write(stdout,9000)
        is = indp (prslon(jlat), xt, imt)
        ie = indp (prelon(jlat), xt, imt)
        ks = indp (prsdpt(jlat), zt, km)
        ke = indp (predpt(jlat), zt, km)
        fx = 1.0e-2
        do 680 m=1,nt
         scl = c1
         if (m .eq. 2) scl=1.e-3
         do k=1,km
          do i=1,imt
           ptemp(i,k) = t(k,i,jj,m,nc)
          enddo
         enddo
         write (stdout,9100) trname(m), itt
     &        ,    jj, yt(jj), xt(is), xt(ie), fx*zt(ks), fx*zt(ke), scl
         call matrix (ptemp(1,1), imt, is, ie, ks, ke, scl)
 680    continue
c
        do k=1,km
         do i=1,imt
          ptemp(i,k) = u(k,i,jj,nc)
         enddo
        enddo

        scl = c1
        write (stdout,9100) 'u velocity', itt
     &,   jj, yt(jj), xt(is), xt(ie), fx*zt(ks), fx*zt(ke), scl
        call matrix (ptemp(1,1), imt, is, ie, ks, ke, scl)
c
        do k=1,km
         do i=1,imt
          ptemp(i,k) = v(k,i,jj,nc)
         enddo
        enddo
        
        scl = c1
        write (stdout,9100) 'v velocity', itt
     &,   jj, yt(jj), xt(is), xt(ie), fx*zt(ks), fx*zt(ke), scl
        call matrix (ptemp(1,1), imt, is, ie, ks, ke, scl)
      endif
700   continue
c
c------------------------------------------------------------------------
c       print the free surface model solution
c------------------------------------------------------------------------
c
      scl = c100
      is = indp (slonxy, xt, imt)
      ie = indp (elonxy, xt, imt)
      js = indp (slatxy, yt, jmt)
      je = indp (elatxy, yt, jmt)
      write (stdout,8000) ' surface height (m)'
     &, itt, xt(is), xt(ie), yt(js), yt(je), scl
      call matrix (h0(1,1,np0), imt, is, ie, -js, -je, scl)
c
      scl = c1
      write (stdout,8000) ' u velocity (cm/s)'
     &, itt, xt(is), xt(ie), yt(js), yt(je), scl
      call matrix (u0(1,1,np0), imt, is, ie, -js, -je, scl)
c
      write (stdout,8000) ' v velocity (cm/s)'
     &, itt, xt(is), xt(ie), yt(js), yt(je), scl
      call matrix (v0(1,1,np0), imt, is, ie, -js, -je, scl)
c
      return
c
8000  format(1x,a27,1x,'ts=',i7
     &,', lon:',f6.2,' ==> ',f6.2,',   lat:',f6.2,' ==> ',f6.2
     &,', scaling=',1pg10.3)
9000  format(/' Zonal section printouts at specified latitudes:'/)
9100  format(1x,a27,1x,'ts=',i10,1x,',j=',i3,', lat=',f6.2
     &,', lon:',f6.2,' ==> ',f6.2,', depth(m):',f6.1,' ==> ',f6.1
     &,', scaling=',1pg10.3)
      end
