      subroutine fluxadjust(stf, i, j, totday)

c   constant offset to adjust temp and salinity to climatology.
c   Just maybe. Largely ripped from bcest2.F totday (assumes totday 1 = 1-Jan) 

#include "flux_adj.h"

      real stf(2), totday, adj(2)
      integer i,j

      integer month, day, doy, years, month1, month2
      real f1, f2


c   identify where we are from totday 
      years=totday/360

      doy=totday-(years*360)+1


      month=1+((doy-1)/30)
      day=mod(doy,30)
      if (day.eq.0) day=30


c   work out which two month's data to use and assign weights dependent
c   on where in the month we are

c   retrieve and weight the relevant data
      adj(1)=heat_flux(i,j,month)
      adj(2)=water_flux(i,j,month)

c     write(57,*)stf(1),adj(1)

c   do something about southern ocean problems?
c   if we're in the SO and ..umm.., what to choose. Just 
c   ignore everything in the SO?

      if (j.lt.11) adj(1)=0


      stf(1)=stf(1)+adj(1)
      stf(2)=stf(2)+adj(2)

      return
      end
