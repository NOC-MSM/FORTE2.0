#include "cpp_options.h"
      subroutine tmngr (dt)
      implicit none
c
c
c=======================================================================
c
c                      t-i-m-e      m-a-n-a-g-e-r
c
c     inputs:
c
c     itt    = current model time step (from ctmngr.h).
c     dtts   = length of time step in seconds.
c     dgnstc = number of days between printing diagnostics
c     nmix   = number of timesteps between mixint timesteps
c     tsi    = number of days between printing timestep information
c     days   = day at which to end run.
c
c     outputs:
c
c     itt    = itt (on input)  + 1
c     totday = total number of days elapsed 
c              (0.5 = noon on first day).
c     years is number of years elapsed (365.5 days = 1 year)
c     diagts = true if time to print diagnostics
c     mixts  = true if a mixing timestep
c     prntsi = true if time to print timestep information
c     last   = true if last timestep of run
c
c=======================================================================
c
#include "param.h"
#include "ctmngr.h"
#include "switch.h"
#include "timelv.h"
c 
      _MOMA_REAL dt !passed to routine
c External function:
      logical tmnset
      external tmnset
c local:
      integer iyear
c
c-----------------------------------------------------------------------
c     increment time step counters
c     and create a time stamp
c-----------------------------------------------------------------------
c
      itt = itt + 1
      totsec = totsec + dt
      totday = totsec*secday
      years  = totday*(c1/360.)
      iyear = int(years)
c      write(stamp,11)totday,years
c  11  format('totday =',f8.2,',years =',f8.3)
      write(stamp,12)iyear,(totday-dble(iyear)*360.)
 12   format('year =',i8,' +day =',f8.4)
c
c-----------------------------------------------------------------------
c
c     set time dependent logical switches.
c     1. set last if it is the last timestep
c     2. set diagts if time to do diagnostics 
c     3. set mixts if time mixing to be done
c     4. set prntsi if time to print timestep info
c     5. set snapts if time to archive model
c
c-----------------------------------------------------------------------
c
      last   = (totday .ge. days) 
      diagts = tmnset (dgnstc, totday, dt)
      mixts  = (mod(itt,nmix) .eq. 1) 
      prntsi = tmnset (tsi, totday, dt)
      snapts = tmnset (snapd, totday, dt)
      archts = tmnset (archd, totday, dt) .and. .not. last

c---------------------------------------------
c marc 10.10.2001
c	print*,'streamd:',streamd,'totday:',totday,'dt:',dt
      streamts = tmnset (streamd,totday,dt).or.first.or.last
c-----------------------------------------------
c     jods 27/02/02
c     Ensure time-step mixing on the first time step.
      mixts = mixts.or.first  

c     
      return
      end
