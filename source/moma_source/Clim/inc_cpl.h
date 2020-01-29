C
C -- inc_cpl.h  
C    **********
C@
C@  Contents : variables describing field restart file names 
C@  --------
C@
C@ -- cl_write/cl_read  : for fields to write/READ
C@ -- cl_f_write/cl_f_read  : for fields to write/read
C@
C     -------------------------------------------------------------------
C
      CHARACTER*8 cl_writ(jpmaxfld), cl_read(jpmaxfld)
      CHARACTER*8 cl_f_writ(jpmaxfld), cl_f_read(jpmaxfld)
      COMMON / comcpl / cl_writ, cl_read, cl_f_writ, cl_f_read
C     -------------------------------------------------------------------
