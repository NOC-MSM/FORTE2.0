#include "cpp_options.h"
      subroutine snap
      implicit none
c
c=======================================================================
c     write snapshot file
c     not currently implemented except for hdf option
c=======================================================================
c
#include "iounit.h"
#include "pconst.h"
c
      if(ftsnap.eq.'asc ')then
        write(stdout,*) 'Snapshot write: file type "',
     &                  ftsnap,'" not supported'
#ifdef ncsa_hdf
      elseif(ftsnap.eq.'hdf ')then
        call snap_hdf
#endif
#ifdef netcdf
      elseif(ftsnap.eq.'ncdf')then
        call snap_netcdf
#endif
      else
        write(stdout,*) 'Snapshot write: file type "',
     &                  ftsnap,'" not supported'
      endif
      return
      end
