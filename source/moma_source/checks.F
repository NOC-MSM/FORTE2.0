#include "cpp_options.h"
      subroutine checks
      implicit none
c
c=======================================================================
c     do consistency checks
c=======================================================================
c
#include "param.h"
#include "scalar.h"
#include "switch.h"
#include "iounit.h"
c
c   check timesteps
c
      if( dtuv.ne.dtts)then
        write (stdout,9000)
     $  '==> Warning: use of unequal time steps implies the transient '
     $, '             response is unimportant and multiple equilibria '
     $, '             do not exist.                                   '
      endif
c
c   check archive and restart file formats
c
      if(ftrest.ne.'rst'.and.ftrest.ne.'ufm'.and.
     &   ftrest.ne.'ncdf'.and.
     &   ftrest.ne.'hdf'.and..not.init)then
        print *,' ftrest has invalid value'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftsnap.ne.'hdf'.and.ftsnap.ne.'ncdf'.and.snapd.ne.0.0)then
        print *,' ftsnap has invalid value'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftarch.ne.'rst'.and.ftarch.ne.'ufm'.and.
     &   ftarch.ne.'ncdf'.and.
     &   ftarch.ne.'hdf'.and.archd.ne.0.0)then
        print *,' ftarch has invalid value'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
#ifndef ncsa_hdf
      if(ftrest.eq.'hdf'.and..not.init)then
        print *,' hdf restart file not supported.  "ncsa_hdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftsnap.eq.'hdf'.and.snapd.ne.0.0)then
        print *,' hdf snapshot file not supported.  "ncsa_hdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftarch.eq.'hdf'.and.(archd.ne.0.0.or.restrt))then
        print *,' hdf restart file not supported.  "ncsa_hdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
#endif
#ifndef netcdf
      if(ftrest.eq.'ncdf'.and..not.init)then
        print *,' netcdf restart file not supported.  "netcdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftsnap.eq.'ncdf'.and.snapd.ne.0.0)then
        print *,' netcdf snapshot file not supported.  "netcdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
      if(ftarch.eq.'ncdf'.and.(archd.ne.0.0.or.restrt))then
        print *,' netcdf restart file not supported.  "netcdf" not set'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
#endif
#ifdef msq
      if(nhalo.lt.2) then
        print *,' msq advection requires at least a 2 '
     &       //'point halo region.'
        print *,' Set "halo2" and re-make the program'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
      endif
#endif        
#ifdef OLD_NON_CONSERVING
      print *,' Warning - "OLD_NON_CONSERVING" is set - tracer '
     &     //'conservation is not guaranteed'
#endif
#ifndef isopycmix
# ifdef full_griffies
        print *,' ERROR "full_griffies" is set and requires "isopycmix"'
     &     //'which is not set.'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
# endif
# ifdef gent_mcwilliams
        print *,' ERROR "gent_mcwilliams" is set and requires '
     &       //'"isopycmix" which is not set.'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
# endif
# ifdef gm_diag
        print *,' ERROR "gm_diag" is set and requires '
     &       //'"isopycmix" which is not set.'
        print *,' Program stopping... '
        stop 'STOPPED in subroutine checks'
# endif
#endif /*ndef isopycmix*/
        if(dtts.eq.0.0) then
          write(stdout,*) 'WARNING : Tracer timestep (dtts) is 0.0'
        endif
        if(dtuv.eq.0.0) then
          write(stdout,*) 'WARNING : Velocity timestep (dtuv) is 0.0'
        endif
        if(dtbt.eq.0.0) then
          write(stdout,*) 
     &         'WARNING : Free surface timestep (dtbt) is 0.0'
        endif
      return
9000  format (/,(1x,a80))
      end
