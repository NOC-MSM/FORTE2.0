      subroutine bcest2(stf,smf, i, j, totday)

c   returns temp and salinity to restore to and a wind stress based on
c   totday (assumes totday 1 = 1-Jan) and some climatology files

#include "restor98.h"

      real stf(2), smf(2), totday
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

      if (day.lt.16) then
       month1=month-1
      else
       month1=month
      end if

      if (month1.eq.0) month1=12
      month2=month1+1
      if (month2.eq.13) month2=1

      if (day.le.15) then
        f1=15-day
        f2=15+day
      else
        f1=45-day
        f2=day-15
      end if

c     if(i.eq.45.and.j.eq.22)  then
c       write(57,*)f1, month1, f2, month2
c     end if
 

c   retrieve and weight the relevant data
      stf(1)=f1*restortemp(i,j,month1)+f2*restortemp(i,j,month2)
      stf(2)=f1*restorsal(i,j,month1)+f2*restorsal(i,j,month2)
      smf(1)=f1*restoruwind(i,j,month1)+f2*restoruwind(i,j,month2)
      smf(2)=f1*restorvwind(i,j,month1)+f2*restorvwind(i,j,month2)

      stf(1)=stf(1)/30.
      stf(2)=stf(2)/30.

c  units conversion on wind stresses
      smf(1)=10.0*smf(1)/30.
      smf(2)=10.0*smf(2)/30.
 
      return
      end
