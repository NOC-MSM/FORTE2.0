C
C -- calendar.h   01-11-95   Version 2.0   Author: Laurent Terray
C    **********
C@
C@  Contents : variables related to the coupler calendar
C@  --------
C@
C@ -- nddeb : beginning date of the simulation (yyyymmdd)
C@
C@ -- nadeb : beginning year of the simulation (yy)
C@
C@ -- nmdeb : beginning month of the simulation (mm)
C@
C@ -- njdeb : beginning day of the simulation (dd)
C@
C@ -- ndate : initial date (yyyymmdd)
C@
C@ -- njini : initial day (dd)
C@
C@ -- nmini : initial month (mm)
C@
C@ -- naini : initial year (yy)
C@
C@ -- njnow : current day (dd)
C@ 
C@ -- njone : inf. day limit for linear time interpolation (dd)
C@              'sea <<<--->>> sea' case
C@
C@ -- njtwo : sup. day limit for linear time interpolation (dd)
C@              'sea <<<--->>> sea' case
C@
C@ -- ndone : inf. day limit for linear time interpolation (dd)
C@              'sea <<<--->>> ice' case
C@
C@ -- ndtwo : sup. day limit for linear time interpolation (dd)
C@              'sea <<<--->>> ice' case
C@
C@ -- nmnow : current month (mm)
C@
C@ -- nmone : inf. month limit for linear time interpolation (mm)
C@
C@ -- nmtwo : sup. month limit for linear time interpolation (mm)
C@
C@ -- nanow : current year (yy)
C@
C@ -- ndinc : day increment for each coupler time step
C@
C@ -- nsrec : number of records to be skipped in climatology (SE) file
C@
C@ -- nmrec : number of records to be skipped in interannual (MO) file
C
C     -------------------------------------------------------------------
C
      COMMON / comdat / ndate, njini, nmini, naini, njnow,
     $                  njone, njtwo, ndone, ndtwo, nmnow,
     $                  nmone, nmtwo, nanow, ndinc, nsrec, nmrec
      COMMON / comdeb / nddeb, nadeb, nmdeb, njdeb
C
C     -------------------------------------------------------------------
