#include "cpp_options.h"
      integer function indp(c, ca, ia)
      implicit none
c
c
c=======================================================================
c
c     indp = index of nearest data point
c
c     inputs:
c
c     c      = arbitrary data point...same units as ca
c     ca     = array of data points
c              (must be monotonically increasing)
c     ia     = dimension of ca
c
c     output:
c
c     indp =  index of nearest data point to "c"
c             if "c" is outside the domain of "ca" then i
c             ndp is set to 1 or ia depending on  whether
c             c(1) or c(ia) is closest
c
c        note: if "ca" is dimensioned ca(0:ia) in the calling
c              program, then the returned index should be reduced
c              by one to account for the zero base.
c
c     example:
c
c     let model depths be defined by the following:
c     parameter (km=5)
c     dimension z(km)
c     data z /5.0, 10.0, 50.0, 100.0, 250.0/
c
c     k1 = indp (12.5, z, km)
c     k2 = indp (0.0, z, km)
c
c     k1 would be set to 2, & k2 would be set to 1 so that
c     z(k1) would be the nearest data point to 12.5 and z(k2) would
c     be the nearest data point to 0.0
c
c=======================================================================
c
c
c Routine Arguments:
      integer ia
      _MOMA_REAL c,ca(ia)
c Local:
      integer i
c
      if (c .lt. ca(1) .or. c .gt. ca(ia)) then
        if (c .lt. ca(1))  indp = 1
        if (c .gt. ca(ia)) indp = ia
        return
      else
        do 10 i=2,ia
          if (c .le. ca(i)) then
            indp = i
            if (ca(i)-c .gt. c-ca(i-1)) indp = i-1
            go to 11
          endif
10      continue
11      continue
      endif
      return
      end
