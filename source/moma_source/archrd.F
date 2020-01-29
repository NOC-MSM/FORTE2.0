#include "cpp_options.h"

      subroutine archrd
      implicit none
c
c=======================================================================
c     read archive file
c=======================================================================
c
#include "iounit.h"
#include "pconst.h"
c
      if(ftrest.eq.'rst'.or.ftrest.eq.'ufm')then
        call archrd_std
#ifdef ncsa_hdf
      elseif(ftrest.eq.'hdf')then
	write(stdout,*)'Read in archive hdf file'
        call archrd_hdf
	print*,'Archive file read in'
#endif
#ifdef netcdf
      elseif(ftrest.eq.'ncdf')then
	write(stdout,*)'Read in archive netcdf file'
        call archrd_netcdf
	print*,'Archive file read in'
#endif
      else
        write(stdout,*) 'Archive read: file type "',
     &                  ftrest,'" not supported'
      endif
      return
      end
