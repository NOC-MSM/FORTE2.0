c====================== include file "iounit.h" ========================
#ifdef hcomments
c
c     %Z% SCCS module: %M%, version %I%
c     Creation date: %G%
c
c-----------------------------------------------------------------------
c     i/o units & related variables
c
c     iorest  = unit for archive files (restarts)
c     iosnap  = unit for snapshot files 
c     fnrest  = name of archive file used to start run
c     iokmt   = unit for kmt file (depths)
c     fnkmt   = name of kmt file used
c     ftrest  = file type of archive file used to start run
c               'std' := restart, 'hdf' := hdf file
c     ftarch  = file type used for archive files
c               'std' := restart, 'hdf' := hdf file
c     ftsnap  = file type for snapshot files
c               'std' := ascout, 'hdf' := hdf file
c   note: ansi needs character variables in a separate common block
c
#endif
      character fnflx*80,fnrest*80,fnkmt*80,ftrest*4,ftsnap*4,ftarch*4
      character fnsst*80
      integer iorest,iokmt
      common /iouniti/ iorest,iokmt
      common /iounitc/ fnsst,fnflx,fnrest,fnkmt,ftrest,ftsnap,ftarch

