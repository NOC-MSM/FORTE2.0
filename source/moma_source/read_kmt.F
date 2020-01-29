#include "cpp_options.h"
      subroutine read_kmt
      implicit none
c
c-----------------------------------------------------------------------
c     read array 'kmt' from file 'ocean.kmt'.  
c     The kmt file is always written as if there is only one halo row.
c     If the model uses more than one halo row this is allowed for by 
c     this routine.
c
c     Let j1 = nhalo and let j2 = jmt-nhalo+1
c     If cyclic is set,    let i1 = nhalo+1 and let i2 = imt-nhalo
c     If cyclic is not set let i1 = nhalo   and let i2 = imt-nhalo+1 
c
c     The kmt file is then written as:
c
c     write(56,15)'kmt     ',i2-i1+1,j2-j1+1,km
c     do j=j2,j1,-1
c       write(56,'(1x,nnni3)')(kmt(i,j),i=i1,i2)
c     continue
c     
c     where nnn = i2-i1+1
c-----------------------------------------------------------------------
c
#include "param.h"
#include "coord.h"
#include "iounit.h"
#include "levind.h"
#include "switch.h"
c Local:
      character string*8
      integer iostat,imt1,jmt1,km1,i1,i2,i3,i4,j1,j2,j3,i,j
      character fmtstring*60
c External Function returning last non-blank character in a string
      integer lnblnk
      external lnblnk
c
c  if initial timestep read in kmt field
c
       open(iokmt,file=fnkmt,status='old',iostat = iostat)
       if(iostat.ne.0)then
         write(stdout,11)fnkmt,iostat
         stop
       endif
       read(iokmt,15)string,imt1,jmt1,km1
c
#ifdef cyclic
      i1 = nhalo+1
      i2 = imt-nhalo
#else
      i1 = nhalo
      i2 = imt-nhalo+1
#endif
      j1 = nhalo
      j2 = jmt-nhalo+1
c
      if(imt1.ne.i2-i1+1.or.jmt1.ne.j2-j1+1.or.km1.ne.KM
     &  .or.string.ne.'kmt     ')then
         if(string.ne.'kmt     ')then
           write(stdout,12)fnkmt,string
         else
           write(stdout,13)fnkmt,imt1,jmt1,km1,i2-i1+1,j2-j1+1,KM,
     &                     nhalo
         endif
         stop
       endif
c
c      write(fmtstring,*)'(1x,',imt1,'i3)'
       write(fmtstring,*)'(1x,100i3)'
       do 20 j=j2,j1,-1
         read(iokmt,fmt=fmtstring,end=991)(kmt(i,j),i=i1,i2)
 20    continue
       close(iokmt)
c
c  north-south boundaries
c
      if(nhalo.gt.1)then
        j3=nhalo-1
        do j1 = 1,j3
          j2 = jmt+1-j1
          do i=1,imt
            kmt(i,j1) = 0
            kmt(i,j2) = 0
          enddo
        enddo
      endif
c
c  east-west boundaries
c
#ifdef cyclic
      do i1=1,nhalo
        i2 = i1+nhalo
        i3 = imtm2h+i1
        i4 = i3+nhalo
        do j=1,jmt
          kmt(i1,j) = kmt(i3,j)
          kmt(i4,j) = kmt(i2,j)
        enddo
      enddo
#else
      if(nhalo.gt.1)then
        i2 = nhalo
        i3 = imt+1-nhalo
        do i1=1,nhalo-1
          i4=imt+1-i1 
          do j=j1,j2
            kmt(i1,j) = kmt(i2,j)
          end do
        end do
      endif
#endif
c
  11  format(1x,'Error opening file "',a,'".',/,'  iostat = ',i4,
     &      '    Program stopping')
  12  format(1x,'Error opening file "',a,'".',/,'  File type faulty',
     &      ' File type = ',a8,/,' Program stopping')
  13  format(1x,'Error opening file "',a,'".',/,'  File dimensions ',
     &      'incompatable with current model.',/,
     &      ' i and j range and km from file =',3i5,/,
     &      ' i and j range and km of model  =',3i5,/,
     &      '                         nhalo  =', i5,/,
     &      ' Program stopping ...')
  15  format(1x,a8,6i10)
      return
c Error traps:
 991  continue
      write(stdout,*)' Unexpected end of file while reading kmt data'
      write(stdout,*)' Error occurred in read_kmt.F'
      write(stdout,*)' While reading line ',j2-j+1,' in file ',
     &     fnkmt(1:lnblnk(fnkmt))
      STOP 'ERROR'
      end
