#include "cpp_options.h"
      function tmnset (days, totday, dt)
      implicit none
c
c=======================================================================
c     tmnset = true if current time is within 1/2 time step of
c              desired interval
c     days   = desired time interval 
c     totday = accumulated time in days from start of run
c     dt     = time step in seconds
c=======================================================================
c
#include "pconst.h"
c Function returns:     
      logical tmnset
c Arguments:
      _MOMA_REAL days,totday,dt
c Local:
      _MOMA_REAL close,remain
      integer i
c
c  return false if interval days is zero
c
      if(days.eq.c0)then
        tmnset = .false.
        return
      endif
c
c  else return true if the elapsed time 'totday' is within half
c  a timestep 'dt' of an integer multiple of the interval 'days'
c
      close  = p5*dt
      i      = nint (totday/days)
      remain = (totday - i*days)*c60*c1440
      if ((remain .ge. c0 .and. remain .lt. close) .or.
     &    (remain .lt. c0 .and. -remain .le. close)) then
        tmnset = .true.
      else
        tmnset = .false.
      endif
      return
      end
