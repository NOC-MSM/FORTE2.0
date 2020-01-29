#include "cpp_options.h"
      subroutine archwr
      implicit none
c
c=======================================================================
c     write archive file
c=======================================================================
c
#include "iounit.h"
#include "pconst.h"
c
      if(ftarch.eq.'rst'.or.ftarch.eq.'ufm')then
        call archwr_std
#ifdef ncsa_hdf
      elseif(ftarch.eq.'hdf')then
        call archwr_hdf
#endif
#ifdef netcdf
      elseif(ftarch.eq.'ncdf')then
        call archwr_netcdf
#endif
      else
        write(stdout,*) 'Archive write: file type "',
     &                  ftarch,'" not supported'
      endif
      return
      end
