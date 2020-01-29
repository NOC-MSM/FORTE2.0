#include "cpp_options.h"
      subroutine matrix (array, irdim, istrt, im, jstrt, jm, scale)
      implicit none
c
c=======================================================================
c
c     matrix is a general two-dimensional array printing routine,
c     where:
c     array = the array to be printed
c     irdim = the 1st dimension of array
c     istrt = the 1st element of the 1st dimension to be printed
c     im    = the last element of the 1st dimension to be printed
c     jstrt = the 1st element of the 2nd dimension to be printed
c     jm    = the last element of the 2nd dimension to be printed
c             the 2nd dimension is printed in reverse order if both
c             jstrt & jm are negative
c     scale = a scaling factor by which array is divided before
c             printing.  (if this is zero, no scaling is done.)
c             if scale=0, 10 columns are printed across in e format
c             if scale>0, 20 columns are printed across in f format
c
c=======================================================================
c
#include "param.h"
c
c Arguments:
      integer irdim, istrt, im, jstrt, jm
      _MOMA_REAL array(irdim,1000)
      _MOMA_REAL scale
c Local:
      integer js,je,jinc,is,ie,i,l
      _MOMA_REAL scaler
c
      if (jstrt*jm .lt. 0) then
        write (stderr,999)  jstrt, jm
        stop '=>matrix'
      endif
c
c     allow for inversion of 2nd dimension
c
      if (jm .lt. 0) then
        js   = -jm
        je   = -jstrt
        jinc = -1
      else
        js   = jstrt
        je   = jm
        jinc = 1
      endif
c
      if (scale .eq. c0) then
        do 100 is=istrt,im,10
          ie = min(is + 9,im)
          write (stdout,9001) (i, i=is,ie)
          do 90 l=js,je,jinc
            write (stdout,9002) l, (array(i,l),i=is,ie)
90        continue
          write (stdout,'(//)')
100     continue
c
      else
        scaler = c1/scale
        do 200 is=istrt,im,20
          ie = min(is + 19,im)
          write (stdout,9003) (i, i=is,ie)
          do 190 l=js,je,jinc
            write (stdout,9004) l, (array(i,l)*scaler,i=is,ie)
190       continue
          write (stdout,'(//)')
200     continue
      endif
c
      return
c
999   format (1x,'jstrt=',i5,' jm=',i5,' in matrix')
9001  format(10i13)
9002  format(1x,i2,10(1pe13.5))
9003  format(3x,20i6)
9004  format(1x,i3,1x,20f6.2)
c
      end
