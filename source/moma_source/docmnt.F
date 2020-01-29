#include "cpp_options.h"
      subroutine docmnt (nunit, wrdoc, wrc0, wrc1, wrc2)
      implicit none
c
c=======================================================================
c
c     This code arranges data to be written out as a form of
c     documentation summarizing the model characteristics that
c     uniquely define this run.
c     In order to reap the full benefits of this feature, the user will
c     need to review this routine when configuring a model run or adding
c     new options to the code.
c     We envision this feature as being beneficial to those setting up
c     new model runs, comparing different model runs, and for analysis
c     purposes (much of the information used in setting up mapping
c     procedures and other analysis products are contained in the
c     data written by this routine).
c
c     Five namelist writes are available.  Which ones are written
c     is determined from the 4 logical variables passed into the
c     subroutine (wrdoc, wrc0, wrc1, & wrc2).  The unit to which
c     they are written is set by "nunit".
c
c     If "wrdoc" is true, then the "ndocs" & "docums" namelists
c     are written...
c       The 1st, "ndocs" simply contains one variable (ndoc); the
c     dimension of "docum(ndoc)" found in the 2nd namelist write.
c       The 2nd, "docums" consists of a series of 60 character-long
c     lines of information the user should use to list the variables
c     in the subsequent namelist writes, and to describe the model
c     run in general.
c
c     If "wrc0" is true, then the "cntrl0" namelist is written...
c       Namelist "cntrl0" contains the 2 variables needed to restart
c     the model (itt, years).
c
c       The "cntrl1" and "cntrl2" namelists contain information
c     describing the model's configuration and specifications.
c
c     If "wrc1" is true, then the "cntrl1" namelist is written...
c       "cntrl1" contains variables (non-dimensioned) that will be
c     common to most, if not all, incarnations of the model code.
c
c     If "wrc2" is true, then the "cntrl2" namelist is written...
c       "cntrl2" is intended to contain the larger (dimensioned)
c     data that will help identify the model specifics to users
c     and analysis programs alike.
c
c=======================================================================
c  This routine is based on the corresponding mom routine but
c  contains only a few of the standard features
c=======================================================================
c
#include "param.h"
#include "switch.h"
#include "versno.h"
c Arguments:
      integer nunit
      logical wrdoc, wrc0, wrc1, wrc2
c Local:
      integer ndocum,ifdmax
      parameter (ndocum = 16, ifdmax = 50)
      integer n,nifdef,nn,i,ns,ne
c
      character expnam*60
      character cifdef(ifdmax)*20, blnk12*20
      data    blnk12 /'                    '/
c      integer itt
c      _MOMA_REAL years
c      namelist /conrl0/ itt,years
c      namelist /cntrl1/ expnam
c
      expnam = model(1)(1:60)
c
c-----------------------------------------------------------------------
c     list options that are in effect on first timestep
c-----------------------------------------------------------------------
c
      if (first) then
c
        do 200 n=1,ifdmax
          cifdef(n) = blnk12
 200    continue
        nifdef = 0
        cifdef(1) = '0 ifdefs set        '
c
#ifdef adiabatic_msq
        nifdef = nifdef + 1
        cifdef(nifdef) = 'adiabatic_msq       '
#endif
#ifdef cyclic
        nifdef = nifdef + 1
        cifdef(nifdef) = 'cyclic              '
#endif
#ifdef de_checkbd
        nifdef = nifdef + 1
        cifdef(nifdef) = 'de_checkbd          '
#endif
#ifdef de_checkbd_T
        nifdef = nifdef + 1
        cifdef(nifdef) = 'de_checkbd_T        '
#endif
#ifdef free_eb
        nifdef = nifdef + 1
        cifdef(nifdef) = 'free_eb             '
#endif
#ifdef full_griffies
        nifdef = nifdef + 1
        cifdef(nifdef) = 'full_griffies       '
#endif
#ifdef gent_mcwilliams
        nifdef = nifdef + 1
        cifdef(nifdef) = 'gent_mcwilliams     '
#endif
#ifdef gm_diag
        nifdef = nifdef + 1
        cifdef(nifdef) = 'gm_diag             '
#endif
#ifdef halo2
        nifdef = nifdef + 1
        cifdef(nifdef) = 'halo2               '
#endif
#ifdef inline
        nifdef = nifdef + 1
        cifdef(nifdef) = 'inline              '
#endif
#ifdef isopycmix
        nifdef = nifdef + 1
        cifdef(nifdef) = 'isopycmix           '
#endif
#ifdef msq
        nifdef = nifdef + 1
        cifdef(nifdef) = 'msq                 '
#endif
#ifdef ncsa_hdf
        nifdef = nifdef + 1
        cifdef(nifdef) = 'ncsa_hdf            '
#endif
#ifdef netcdf
        nifdef = nifdef + 1
        cifdef(nifdef) = 'netcdf              '
#endif
#ifdef OLD_NON_CONSERVING
        nifdef = nifdef + 1
        cifdef(nifdef) = 'OLD_NON_CONSERVING  '
#endif
#ifdef NONLIN_FREE_SURFACE
        nifdef = nifdef + 1
        cifdef(nifdef) = 'NONLIN_FREE_SURFACE '
#endif
#ifdef oldav
        nifdef = nifdef + 1
        cifdef(nifdef) = 'oldav               '
#endif
#ifdef presetp
        nifdef = nifdef + 1
        cifdef(nifdef) = 'presetp             '
#endif
#ifdef REAL_8
        nifdef = nifdef + 1
        cifdef(nifdef) = 'REAL_8              '
#endif
#ifdef simple_world
        nifdef = nifdef + 1
        cifdef(nifdef) = 'simple_world        '
#endif
#ifdef w_write
        nifdef = nifdef + 1
        cifdef(nifdef) = 'w_write             '
#endif
        write (stdout,'(/,a45)')
     $   ' The following "ifdef" options are in effect:'
        nn = (nifdef+4)/5
        do 300 i=1,nn
          ns = 1+(5*(i-1))
          ne = ns + 4
          if (ne .gt. nifdef) ne = nifdef
          write (stdout,9101) (cifdef(n),n=ns,ne)
300     continue
        write (stdout,'(a21,/)') ' end of options list.'
      endif

c----------------------------
c 5.9.2001 marc
c	if (first) then
c	   eb = .false.
c	endif
c--------------------------

      return
9101  format(5(3x,a20))
      end
