C "@(#)pvm/pvm/include/fpvm3.h	21.2	03/02/95 16:46:04"

C
C  User-level Fortran include file for PVM.  This is a common include file 
C  for the different Cray Research PVM implementations, but different
C  release schedules will lead to the different implementations having
C  different include files at any given point of time.  Make sure you
C  are using the correct include file for your application.
C

C ***** This include file was installed for CRAY Network PVM-3 *****
C
C	Warning:  there are declarations here that only have meaning
C	on CRAY MPP systems.  You should not use these with applications
C	developed for CRAY Network PVM-3.

c  -------------------------------------------------------------------
c          PVM version 3.3:  Parallel Virtual Machine System
c                University of Tennessee, Knoxville TN.
c            Oak Ridge National Laboratory, Oak Ridge TN.
c                    Emory University, Atlanta GA.
c       Authors:  A. L. Beguelin, J. J. Dongarra, G. A. Geist,
c     W. C. Jiang, R. J. Manchek, B. K. Moore, and V. S. Sunderam
c                    (C) 1992 All Rights Reserved
c
c                               NOTICE
c
c  Permission to use, copy, modify, and distribute this software and
c  its documentation for any purpose and without fee is hereby granted
c  provided that the above copyright notice appear in all copies and
c  that both the copyright notice and this permission notice appear in
c  supporting documentation.
c
c  Neither the Institutions (Emory University, Oak Ridge National
c  Laboratory, and University of Tennessee) nor the Authors make any
c  representations about the suitability of this software for any
c  purpose.  This software is provided ``as is'' without express or
c  implied warranty.
c
c  PVM version 3 was funded in part by the U.S. Department of Energy,
c  the National Science Foundation and the State of Tennessee.
c  -------------------------------------------------------------------

C
C	(C) COPYRIGHT CRAY RESEARCH, INC.
C	UNPUBLISHED PROPRIETARY INFORMATION.
C	ALL RIGHTS RESERVED.
C

c     ----------------------------------
c	  fpvm3.h
c
c     Definitions to be included with
c     User's Fortran application
c     ----------------------------------

      integer PVMTASKDEFAULT, PVMTASKHOST, PVMTASKARCH, PVMTASKDEBUG
      integer PVMTASKTRACE, PVMMPPFRONT, PVMHOSTCOMPL
      integer PVMHOST, PVMARCH, PVMDEBUG, PVMTRACE
      integer PVMDATADEFAULT, PVMDATARAW, PVMDATAINPLACE
      integer PVMDEFAULT, PVMRAW, PVMINPLACE
      integer PVMTASKEXIT, PVMHOSTDELETE, PVMHOSTADD
      integer PVMROUTE, PVMDEBUGMASK, PVMAUTOERR
      integer PVMOUTPUTTID, PVMOUTPUTCODE, PVMRESVTIDS
      integer PVMTRACETID, PVMTRACECODE, PVMFRAGSIZE
      integer PVMDONTROUTE, PVMALLOWDIRECT, PVMROUTEDIRECT
c     ---------------------
c     CRAY MPP declarations
c     ---------------------
      integer PVMMAXGTIDS, PVMFASTBARR
      integer PVMTRACEOPTS, PVMCIRCULARTRACE, PVMTOTALSTATS
      integer PVMTASKSTATS
      integer PVMSMPOOL, PVMDATAMAX, PVMDATABUFFERS, PVMMAXPACK
      integer PVMCHECKING
      integer PVMDATABUFFERSINCR, PVMMAXPACKINCR, PVMTOTALPACK
      integer INTEGER8
c     -------------------------
c     end CRAY MPP declarations
c     -------------------------

      integer STRING, BYTE1, INTEGER2, INTEGER4
      integer REAL4, COMPLEX8, REAL8, COMPLEX16

      integer PvmOk, PvmSysErr, PvmBadParam, PvmMismatch
      integer PvmNoData, PvmNoHost, PvmNoFile, PvmNoMem
      integer PvmBadMsg, PvmNoBuf, PvmNoSuchBuf
      integer PvmNullGroup, PvmDupGroup, PvmNoGroup
      integer PvmNotInGroup, PvmNoInst, PvmHostFail, PvmNoParent
      integer PvmNotImpl, PvmDSysErr, PvmBadVersion, PvmOutOfRes
      integer PvmDupHost, PvmCantStart, PvmAlready, PvmNoTask
      integer PvmNoEntry, PvmDupEntry
c     ---------------------
c     Cray MPP declarations
c     ---------------------
      integer PvmTooLong, PvmStillActive
      integer PvmOutOfResBuf, PvmOutOfResSMP, PvmOutOfResGmems
      integer PvmTooMuchData, PvmMemLimit

c     --------------------
c     spawn 'flag' options
c     --------------------
      parameter( PVMTASKDEFAULT  =  0)
      parameter( PVMTASKHOST     =  1)
      parameter( PVMTASKARCH     =  2)
      parameter( PVMTASKDEBUG    =  4)
      parameter( PVMTASKTRACE    =  8)
      parameter( PVMMPPFRONT     = 16)
      parameter( PVMHOSTCOMPL    = 32)
c     --------------------------------
c     old option names still supported
c     --------------------------------
      parameter( PVMHOST  =  1)
      parameter( PVMARCH  =  2)
      parameter( PVMDEBUG =  4)
      parameter( PVMTRACE =  8)

c     -------------------------
c     buffer 'encoding' options
c     -------------------------
      parameter( PVMDATADEFAULT = 0)
      parameter( PVMDATARAW     = 1)
      parameter( PVMDATAINPLACE = 2)
c     --------------------------------
c     old option names still supported
c     --------------------------------
      parameter( PVMDEFAULT = 0)
      parameter( PVMRAW     = 1)
      parameter( PVMINPLACE = 2)

c     ----------------------
c     notify 'about' options
c     ----------------------
      parameter( PVMTASKEXIT   = 1 )
      parameter( PVMHOSTDELETE = 2 )
      parameter( PVMHOSTADD    = 3 )

c     ----------------------------------------------
c     setopt/getopt 'opt' options
c     (options with value of 100+ are CRAY MPP only)
c     ----------------------------------------------
      parameter( PVMROUTE      = 1 )
      parameter( PVMDEBUGMASK  = 2 )
      parameter( PVMAUTOERR    = 3 )
      parameter( PVMOUTPUTTID  = 4 )
      parameter( PVMOUTPUTCODE = 5 )
      parameter( PVMTRACETID   = 6 )
      parameter( PVMTRACECODE  = 7 )
      parameter( PVMFRAGSIZE   = 8 )
      parameter( PVMRESVTIDS   = 9 )

      parameter( PVMMAXGTIDS   = 100 )
      parameter( PVMFASTBARR   = 101 )
      parameter( PVMTRACEOPTS  = 102 )
      parameter( PVMSMPOOL     = 103 )
      parameter( PVMDATAMAX    = 104 )
      parameter( PVMDATABUFFERS = 105 )
      parameter( PVMMAXPACK    = 106 )
      parameter( PVMCHECKING   = 107 )
      parameter( PVMDATABUFFERSINCR = 108 )
      parameter( PVMMAXPACKINCR = 109 )
      parameter( PVMTOTALPACK  = 110 )
      

c     --------------------------------
c     packing/unpacking 'what' options
c     (INTEGER8 is CRAY MPP only)
c     --------------------------------
      parameter( STRING   = 0)
      parameter( BYTE1    = 1)
      parameter( INTEGER2 = 2)
      parameter( INTEGER4 = 3)
      parameter( REAL4    = 4)
      parameter( COMPLEX8 = 5)
      parameter( REAL8    = 6)
      parameter( COMPLEX16= 7)
      parameter( INTEGER8 = 21)

c     --------------------------------------------
c     routing options for 'how' in setopt function
c     --------------------------------------------
      parameter( PVMDONTROUTE  = 1)
      parameter( PVMALLOWDIRECT= 2)
      parameter( PVMROUTEDIRECT= 3)

c     ------------------------------------------
c     error 'info' return values
c     (values of -100 or less are CRAY MPP only)
c     ------------------------------------------
      parameter( PvmOk         =   0)
      parameter( PvmBadParam   =  -2)
      parameter( PvmMismatch   =  -3)
      parameter( PvmNoData     =  -5)
      parameter( PvmNoHost     =  -6)
      parameter( PvmNoFile     =  -7)
      parameter( PvmNoMem      = -10)
      parameter( PvmBadMsg     = -12)
      parameter( PvmSysErr     = -14)
      parameter( PvmNoBuf      = -15)
      parameter( PvmNoSuchBuf  = -16)
      parameter( PvmNullGroup  = -17)
      parameter( PvmDupGroup   = -18)
      parameter( PvmNoGroup    = -19)
      parameter( PvmNotInGroup = -20)
      parameter( PvmNoInst     = -21)
      parameter( PvmHostFail   = -22)
      parameter( PvmNoParent   = -23)
      parameter( PvmNotImpl    = -24)
      parameter( PvmDSysErr    = -25)
      parameter( PvmBadVersion = -26)
      parameter( PvmOutOfRes   = -27)
      parameter( PvmDupHost    = -28)
      parameter( PvmCantStart  = -29)
      parameter( PvmAlready    = -30)
      parameter( PvmNoTask     = -31)
      parameter( PvmNoEntry    = -32)
      parameter( PvmDupEntry   = -33)

      parameter( PvmTooLong    = -100)
      parameter( PvmStillActive = -101)
      parameter( PvmOutOfResBuf = -103)
      parameter( PvmOutOfResSMP = -104)
      parameter( PvmOutOfResGmems = -105)
      parameter( PvmTooMuchData = -106)
      parameter( PvmMemLimit = -107)

c     ---------------------------------
c     CRAY MPP PVM  "global" group name
c     ---------------------------------
      character*8 PVMALL
      common /PVMALL/PVMALL

c     ---------------------------------
c     CRAY MPP trace/statistics options
c     ---------------------------------
      parameter( PVMCIRCULARTRACE   = 1 )
      parameter( PVMTOTALSTATS  = 2 )
      parameter( PVMTASKSTATS   = 4 )
