#include "cpp_options.h"
      subroutine restio
      implicit none
c
c=======================================================================
c     read and write restart file
c     NOTE - this version only saves and restores the current
c     timestep.  The model must therefore be restarted with
c     a forward or euler backward timestep.
c=======================================================================
c
#include "param.h"
#include "ctmngr.h"
#include "frees.h"
#include "iounit.h"
#include "levind.h"
#include "slabs.h"
#include "switch.h"
#include "timelv.h"
c Local:

      _MOMA_REAL t1(km,imt,jmt,nt), t2(km,imt,jmt,nt), 
     &          t3(km,imt,jmt,nt)
     & ,  u1(km,imt,jmt),  u2(km,imt,jmt), u3(km,imt,jmt)
     & , v1(km,imt,jmt),  v2(km,imt,jmt), v3(km,imt,jmt)
     & , h01(imt,jmt),    h02(imt,jmt),   h03(imt,jmt)
     & , u01(imt,jmt),    u02(imt,jmt),   u03(imt,jmt)
     & , v01(imt,jmt),    v02(imt,jmt),   v03(imt,jmt)
      character file*80
c
      equivalence (t1(1,1,1,1),t(1,1,1,1,1))
      equivalence (t2(1,1,1,1),t(1,1,1,1,2))
      equivalence (t3(1,1,1,1),t(1,1,1,1,3))
      equivalence (u1(1,1,1),u(1,1,1,1))
      equivalence (u2(1,1,1),u(1,1,1,2))
      equivalence (u3(1,1,1),u(1,1,1,3))
      equivalence (v1(1,1,1),v(1,1,1,1))
      equivalence (v2(1,1,1),v(1,1,1,2))
      equivalence (v3(1,1,1),v(1,1,1,3))
c
      equivalence (h01(1,1),h0(1,1,1))
      equivalence (h02(1,1),h0(1,1,2))
      equivalence (h03(1,1),h0(1,1,3))
      equivalence (u01(1,1),u0(1,1,1))
      equivalence (u02(1,1),u0(1,1,2))
      equivalence (u03(1,1),u0(1,1,3))
      equivalence (v01(1,1),v0(1,1,1))
      equivalence (v02(1,1),v0(1,1,2))
      equivalence (v03(1,1),v0(1,1,3))
      integer i,j,k,n,j1,j2,j3,j4,i1,i2,i3,i4
c
c-----------------------------------------------------------------------
c     read restart data set from archive for timesteps itt 
c-----------------------------------------------------------------------
c
      write(stdout,*)'read restart data set from '//
     &     'archive for timesteps itt'
      write(stdout,*)'np: ',np
      entry archrd_std
c  check np equals 3
      if(np.ne.3)goto 900
c
c-----------------------------------------------------------------------
c     initialize t,s,u,v.
c          velocities set to zero
c          temperature and salinity set to land values
c          -2.0 and 45.0 ppt
c-----------------------------------------------------------------------
c
      do n=1,3
       do  j=1,jmt
        do  i=1,imt
         do  k=1,km
          u(k,i,j,n)=c0
          v(k,i,j,n)=c0
          t(k,i,j,1,n)= -2.0
          t(k,i,j,2,n)= 0.01
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
c  read archived data.  Use file fnrest if defined.
c-----------------------------------------------------------------------
c
      if(fnrest(1:1).ne.' ')then
        print *,' read from :',fnrest
        open(iorest, file=fnrest, form='UNFORMATTED', 
     &                            access='SEQUENTIAL',err=920)
      else
        open(iorest, form='UNFORMATTED', access='SEQUENTIAL',err=920)
      endif
      if(ftrest.eq.'rst')then
        read(iorest,err=920) itt, totsec, totday, years, kmt
        read(iorest,err=920) h03,  u03,  v03
        read(iorest,err=920) t3,   u3,   v3
      elseif(ftrest.eq.'ufm')then
        j1 = nhalo
        j2 = jmt+1-nhalo
#ifdef cyclic
        i1 = icbeg
        i2 = icend
#else
        i1 = icbeg-1
        i2 = icend+1
#endif
        read(iorest,err=920) itt, totsec, totday, years
        read(iorest,err=920) ((kmt(i,j),i=i1,i2),j=j1,j2)
        read(iorest,err=920) ((h03(i,j),i=i1,i2),j=j1,j2)
        read(iorest,err=920) ((u03(i,j),i=i1,i2),j=j1,j2)
        read(iorest,err=920) ((v03(i,j),i=i1,i2),j=j1,j2)
        do j=j1,j2
         read(iorest,err=920)((t(k,i,j,1,3),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
         read(iorest,err=920)((t(k,i,j,2,3),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
         read(iorest,err=920)((u(k,i,j,3),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
         read(iorest,err=920)((v(k,i,j,3),k=1,km),i=i1,i2)
        enddo
c
c  set east-west boundary conditions
c
#ifdef cyclic
        do i1=1,nhalo
          i2 = i1+nhalo
          i3 = imtm2h+i1
          i4 = imtm1h+i1
          do j=j1,j2
            kmt(i1,j) = kmt(i3,j)
            kmt(i4,j) = kmt(i2,j)
            h03(i1,j) = h03(i3,j)
            h03(i4,j) = h03(i2,j)
            u03(i1,j) = u03(i3,j)
            u03(i4,j) = u03(i2,j)
            v03(i1,j) = v03(i3,j)
            v03(i4,j) = v03(i2,j)
            do k=1,km
              t(k,i1,j,1,3) = t(k,i3,j,1,3)
              t(k,i4,j,1,3) = t(k,i2,j,1,3)
              t(k,i1,j,2,3) = t(k,i3,j,2,3)
              t(k,i4,j,2,3) = t(k,i2,j,2,3)
              u(k,i1,j,3)   = u(k,i3,j,3)
              u(k,i4,j,3)   = u(k,i2,j,3)
              v(k,i1,j,3)   = v(k,i3,j,3)
              v(k,i4,j,3)   = v(k,i2,j,3)
            end do
          end do
        end do
#else
        if(nhalo.gt.1)then
          i2 = nhalo
          i3 = imt+1-nhalo
          do i1=1,nhalo-1
            i4=imt+1-i1 
            do j=j1,j2
              kmt(i1,j) = kmt(i2,j)
              kmt(i4,j) = kmt(i3,j)
              h03(i1,j) = h03(i2,j)
              h03(i4,j) = h03(i3,j)
              u03(i1,j) = u03(i2,j)
              u03(i4,j) = u03(i3,j)
              v03(i1,j) = v03(i2,j)
              v03(i4,j) = v03(i3,j)
              do k=1,km
                t(k,i1,j,1,3) = t(k,i2,j,1,3)
                t(k,i4,j,1,3) = t(k,i3,j,1,3)
                t(k,i1,j,2,3) = t(k,i2,j,2,3)
                t(k,i4,j,2,3) = t(k,i3,j,2,3)
                u(k,i1,j,3)   = u(k,i2,j,3)
                u(k,i4,j,3)   = u(k,i3,j,3)
                v(k,i1,j,3)   = v(k,i2,j,3)
                v(k,i4,j,3)   = v(k,i3,j,3)
              end do
            end do
          end do
        endif
#endif
c
c  set north-south boundary conditions
c
        if(nhalo.gt.1)then
          j2 = nhalo
          j3 = jmt+1-nhalo
            do j1=1,j2-1
            j4=jmt+1-j1 
            write(*,*)'cyclic n-s',j1,j2,j3,j4
            do i=1,imt
              kmt(i,j1) = kmt(i,j2)
              kmt(i,j4) = kmt(i,j3)
              h03(i,j1) = h03(i,j2)
              h03(i,j4) = h03(i,j3)
              u03(i,j1) = u03(i,j2)
              u03(i,j4) = u03(i,j3)
              v03(i,j1) = v03(i,j2)
              v03(i,j4) = v03(i,j3)
              do k=1,km
                t(k,i,j1,1,3) = t(k,i,j2,1,3)
                t(k,i,j4,1,3) = t(k,i,j3,1,3)
                t(k,i,j1,2,3) = t(k,i,j2,2,3)
                t(k,i,j4,2,3) = t(k,i,j3,2,3)
                u(k,i,j1,3)   = u(k,i,j2,3)
                u(k,i,j4,3)   = u(k,i,j3,3)
                v(k,i,j1,3)   = v(k,i,j2,3)
                v(k,i,j4,3)   = v(k,i,j3,3)
              end do
            end do
          end do
        endif
      endif
c
      close(iorest)

c-------------------------------
c marc 15.10.2001 
	
c	do n=1,2
c    	do j=1,jmt
c    	do i=1,imt
c    	do k=1,km
c        	t(k,i,j,1,n) =  t(k,i,j,1,3)
c        	t(k,i,j,2,n) =  t(k,i,j,2,3)
c	print*,'Temp. 15.10. t(k,i,j,1,n)',t(k,i,j,1,n),t(k,i,j,1,3)
c    	enddo
c    	enddo
c    	enddo
c    	enddo
c--------------------------------

      write (stdout,21) iorest, itt
      if(mod(itt,nmix).ne.0)goto 900

      return
c
c  error
c
 900  write(stdout,22) np, nc, nm
      stop
 910  write(stdout,23) itt, nmix
      stop
 920  write(stdout,24)
      stop
c
  21  format(1x,/,' Routine restio',/,
     &       ' Restart file read from unit ',i2,'.  timestep =',i7)
  22  format(' Routine restio - error.',/,
     &       ' Trying to restart with np not equal to 3.',/,
     &       ' np, nc, nm  =',3i5,/,' Programme stopping ...')
  23  format(' Routine restio - error',/,
     &       ' Trying to restart without forward timestep,',/,
     &       ' i.e. mod(itt,nmix).ne.0.    itt, nmix =',2i5,/,
     &       ' Programme stopping ...')
  24  format(' Routine restio',/,
     &       ' error while reading restart dataset,',/,
     &       ' Programme stopping ...')

c
c-----------------------------------------------------------------------
c     write restart data set to archive.
c-----------------------------------------------------------------------
c
      entry archwr_std
c
c-----------------------------------------------------------------------
c  create archive file name
c-----------------------------------------------------------------------
c
      write (file,'(i10.10)')nint(totday)
      file = 'd'//file(7:10)//'.'//ftarch
      open(iorest, file=file, form='UNFORMATTED',
     &       access='SEQUENTIAL',err=950)
c
      write (stdout,31) itt, file, iorest
      if(ftarch.eq.'rst')then
        write(iorest) itt, totsec, totday, years, kmt
        if(np0.eq.1) write(iorest,err=950)  h01, u01, v01
        if(np0.eq.2) write(iorest,err=950)  h02, u02, v02
        if(np0.eq.3) write(iorest,err=950)  h03, u03, v03
        if(np.eq.1)  write(iorest,err=950)  t1, u1, v1
        if(np.eq.2)  write(iorest,err=950)  t2, u2, v2
        if(np.eq.3)  write(iorest,err=950)  t3, u3, v3
      elseif(ftarch.eq.'ufm')then
        j1 = nhalo
        j2 = jmt+1-nhalo
#ifdef cyclic
        i1 = icbeg
        i2 = icend
#else
        i1 = icbeg-1
        i2 = icend+1
#endif
        write(iorest) itt, totsec, totday, years
        write(iorest,err=950)((kmt(i,j),i=i1,i2),j=j1,j2)
        write(iorest,err=950)((h0(i,j,np0),i=i1,i2),j=j1,j2)
        write(iorest,err=950)((u0(i,j,np0),i=i1,i2),j=j1,j2)
        write(iorest,err=950)((v0(i,j,np0),i=i1,i2),j=j1,j2)
        do j=j1,j2
          write(iorest,err=950)((t(k,i,j,1,np),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
          write(iorest,err=950)((t(k,i,j,2,np),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
          write(iorest,err=950)((u(k,i,j,np),k=1,km),i=i1,i2)
        enddo
        do j=j1,j2
          write(iorest,err=950)((v(k,i,j,np),k=1,km),i=i1,i2)
        enddo
      endif
c
      close(iorest, status = 'KEEP')
      return
c
c error
c
 950  write(stdout,32)
      stop
c
  31  format('  Writing restart file for itt = ',i7,/,
     &       '  filename = ',a,'  unit = ',i2) 
  32  format(' Routine restio',/,
     &       ' error while writing restart dataset',/,
     &       ' Programme stopping ...')
      end
