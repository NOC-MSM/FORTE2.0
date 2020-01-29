#include "cpp_options.h"
#ifdef ncsa_hdf
      subroutine archwr_hdf
      implicit none
c
c %Z% SCCS information - module:  %M%     
c %Z%                    version: %I%    date: %G%
c
c=======================================================================
c  Subroutine to save hdf version of the restart file
c  using multi-file HDF3.3r3 routines.
c  Copyright D.J. Webb, 1993, 1994, 1995, 1996
c=======================================================================
c
#include "param.h"
#include "ctmngr.h"
#include "frees.h"
#include "iounit.h"
#include "levind.h"
#include "slabs.h"
#include "switch.h"
#include "timelv.h"
c
c  hdf '.h' files
c
#include "hdf.inc"
#include "coord.h"
#include "hdf.h"
c
c  local variables
c
      integer*4 write_id,sds_id,dimid1,dimid2,dimid3,ret,ret2,irank,
     &    start(MAXDIMS),stride(MAXDIMS),edge(MAXDIMS)
      character path*38
      integer k,n,j1,j2,i1,i2,ifill_value,j,idvar
      _MOMA_REAL fill_value
c Function  for last non-blank character in a string:
      INTEGER LNBLNK
      external lnblnk
c
c  initialise the hdf common blocks
c
      data lonlabel(1), lonlabel(2) /'LONGITUDE_T', 'LONGITUDE_U' /
      data latlabel(1), latlabel(2) /'LATITUDE_T',  'LATITUDE_U'  /
      data labels2(1), units2(1), fmats2(1) 
     &     /'Longitude', 'degrees', 'F10.4'/
      data labels2(2), units2(2), fmats2(2) 
     &     /'Latitude' , 'degrees', 'F10.4'/
      data labels3(1), units3(1), fmats3(1) 
     &     /'Depth'    , 'cm'    , 'F5.2' /
      data labels3(2), units3(2), fmats3(2) 
     &     /'Longitude', 'degrees', 'F10.4'/
      data labels3(3), units3(3), fmats3(3) 
     &     /'Latitude' , 'degrees', 'F10.4'/
      data (cfext(k),k=1,2) /'.hdf','.hdfa'/
      data (tlabel(n),n=1,4) /'TIMESTEP', 'SECONDS', 'DAY', 'YEARS'/
      data itsize,itdims(1) /1,1/
      data (varname(n),varunit(n),vartgrd(n),n=1,NARCHV)/
     &  'free surface height      ', 'cm          ', .true.  ,
     &  'barotropic u-velocity    ', 'cm/s        ', .false. ,
     &  'barotropic v-velocity    ', 'cm/s        ', .false. ,
     &  'potential temperature    ', 'C           ', .true.  ,
     &  'salinity                 ', '.001(NSU-35)', .true.  ,
     &  'u-velocity               ', 'cm/s        ', .false. ,
     &  'v-velocity               ', 'cm/s        ', .false. 
#ifdef w_write
     &  ,'w-velocity               ', 'cm/s        ', .true.
#endif 
     &  /

        data (varid(n), varcordsys(n), n=1, NARCHV)/
     &  'FREE SURFACE HEIGHT',          'T_GRID'        ,
     &  'BAROTROPIC U VELOCITY',        'U_GRID'        ,
     &  'BAROTROPIC V VELOCITY',        'U_GRID'        ,
     &  'POTENTIAL TEMPERATURE',        'T_GRID'        ,
     &  'SALINITY',                     'T_GRID'        ,
     &  'U VELOCITY',                   'U_GRID'        ,
     &  'V VELOCITY',                   'U_GRID'        
#ifdef w_write
     &  ,'W VELOCITY',                   'T_GRID'        
#endif
     &  /
c
      write(hdffile, '(i10.10)') nint(totday)
      path='./'
      hdffile = path(1:lnblnk(path))//'d'//hdffile(3:10)//cfext(1)
      inquire(file = hdffile, exist = around )
      if(around) then
         write( hdffile,'(i10.10)') itt
         hdffile = path(1:lnblnk(path))//'d'// hdffile(3:10)//cfext(2)
      endif
        
      write_id = sfstart(hdffile, DFACC_CREATE)
      if(write_id.eq.-1) then
        write(stdout,*)' Error opening the file called "'//
     &   hdffile//'"'
        write(stdout,*)' The file may already exist, or you'
     &   //' may not have permission to write to this location.'
        return
      endif
      write (stdout,24) itt, hdffile
  24  format('  Writing hdf archive file for itt = ',i7,/,
     &       '  hdf filename = ',a) 
c
c write the dataset containing 'itt'
c
      irank = 1
      sds_id = sfcreate( write_id,tlabel(1),
     &                  INTEGER_HDF,irank,itdims )
      call hdf_error(sds_id,'cannot create itt dataset', 0)
      ret = sfsdtstr(sds_id, 'TIMESTEP', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set datastrings', 0)
      ret = sfwdata(sds_id, 0,1,itdims, itt) 
      call hdf_error( ret, 'cannot write dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)
c
      print *, "Wrote dataset containing itt OK"
c
c write the dataset containing 'totsec'
c
      sds_id = sfcreate(write_id,tlabel(2),
     &                 REAL_HDF,itsize,itdims)
      print *, " did sf_create"
      call hdf_error(sds_id,'cannot create totsec dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Seconds', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set datastrings', 0)
      ret = sfwdata(sds_id, 0, 1, itdims, totsec)
      call hdf_error( ret, 'cannot write dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)
c    
      print *, "Wrote dataset containing integer totsec OK"
c write the dataset containing 'totday'
c
      sds_id = sfcreate(write_id,tlabel(3),
     &                 REAL_HDF,itsize,itdims)
      print *, " did sf_create #2"
      call hdf_error(sds_id,'cannot create totday dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Day', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set datastrings', 0)
      ret = sfwdata( sds_id, 0, 1,itdims, totday)
      call hdf_error( ret, 'cannot write dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)
c     
      print *, "Wrote dataset containing totday OK"
c write the dataset containing 'years'
c
      sds_id = sfcreate(write_id,tlabel(4),
     &                 REAL_HDF,itsize,itdims)
      print *, " did sf_create #3"
      call hdf_error(sds_id,'cannot create years dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Year', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set datastrings', 0)
      ret = sfwdata( sds_id,0,1,itdims,years)
      call hdf_error( ret, 'cannot write dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)

      print *, "Wrote dataset containing years OK"
c initialise the KMT array field.
c
      j1 = jcbeg-1
      j2 = jcend+1
#ifdef cyclic
      i1 = icbeg
      i2 = icend
#else
      i1 = icbeg-1
      i2 = icend+1
#endif
      irank = 2
      start(1)= 0
      start(2)= 0
      stride(1) = 1
      stride(2) = 1
      dims2(1) = i2-i1+1
      dims2(2) = j2-j1+1
      edge(1)  = i2-i1+1
      edge(2)  = 1
c
c call the HDF function to create the dataset.
c
      sds_id = sfcreate( write_id,'KMT',
     &                  INTEGER_HDF, irank, dims2) 
      print *, " did sf_create #4"
      call hdf_error(sds_id,'cannot create kmt dataset', 0)
      ret = sfsdtstr(sds_id, 'KMT', ' ', ' ', ' ')
      call hdf_error( ret, ' cannot set kmt datastrings', 0)
      ifill_value = 999
      ret = sfsfill(sds_id,ifill_value)
      call hdf_error( ret, ' cannot set fill value', 0)
c
      dimid1 = sfdimid(sds_id,0)
      call hdf_error( dimid1, ' error getting dimid 1', 0)
      dimid2 = sfdimid(sds_id,1)
      call hdf_error( dimid2, ' error getting dimid 2', 0)
      ret = sfsdmname(dimid1,lonlabel(1))
      call hdf_error( ret, ' cannot set dimname 1', 0)
      ret = sfsdmname(dimid2,latlabel(1))
      call hdf_error( ret, ' cannot set dimname 2', 0)
      ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
      call hdf_error( ret, ' setting dimension string 1', 0)
      ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
      call hdf_error( ret, ' setting dimension string 2', 0)
#ifndef REAL_8
c
c---------------------------------------------------------------------
c  skip writing scales when REAL_8 defined.  This is a fix to overcome 
c  an error in pvm when lengths of scale and main variable (kmt) differ
c---------------------------------------------------------------------
c
      ret = sfsdscale(dimid1, dims2(1),REAL_HDF, xt(i1))
      call hdf_error( ret, 'cannot set scales xt', dimid1)
      ret = sfsdscale(dimid2, dims2(2),REAL_HDF, yt(j1))
      call hdf_error( ret, 'cannot set scales yt', dimid2)
#endif
      do 110 j=j1,j2
        start(2) = j-j1
        ret = sfwdata(sds_id,start,stride,edge,kmt(i1,j))
        call hdf_error( ret,'cannot write kmt dataset',0)
 110  continue
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close kmt dataset', 0)
c
c initialise the KMU array field.
c
      sds_id = sfcreate( write_id,'KMU',
     &                  INTEGER_HDF,irank,dims2)
      call hdf_error(sds_id,'cannot create kmu dataset', 0)
      ret = sfsdtstr(sds_id, 'KMU', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set kmu datastrings', 0)
      ifill_value = 999
      ret = sfsfill(sds_id,ifill_value)
      call hdf_error( ret, 'cannot set fill value', 0)
c
      dimid1 = sfdimid(sds_id,0)
      call hdf_error( dimid1, 'error getting dimid 1', 0)
      dimid2 = sfdimid(sds_id,1)
      call hdf_error( dimid2, 'error getting dimid 2', 0)
      ret = sfsdmname(dimid1,lonlabel(2))
      call hdf_error( ret, 'cannot set dimname 1', 0)
      ret = sfsdmname(dimid2,latlabel(2))
      call hdf_error( ret, 'cannot set dimname 2', 0)
c
      ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
      call hdf_error( ret, 'setting dimension string 1', 0)
      ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
      call hdf_error( ret, 'setting dimension string 2', 0)
#ifndef REAL_8
c
c---------------------------------------------------------------------
c  skip writing scales when REAL_8 defined.  This is a fix to overcome 
c  an error in pvm when lengths of scale and main variable (kmu) differ
c---------------------------------------------------------------------
c
      ret = sfsdscale(dimid1, dims2(1),REAL_HDF,xu(i1))
      call hdf_error( ret,'cannot set scales xu', dimid1)
      ret = sfsdscale(dimid2, dims2(2),REAL_HDF,yu(j1))
      call hdf_error( ret,'cannot set scales yu', dimid2)
#endif
#ifdef cyclic
      do 120 j=j1,j2
        start(2) = j-j1
        ret = sfwdata(sds_id,start,stride,edge,kmu(i1,j))
        call hdf_error( ret,'cannot write kmu dataset',0)
 120  continue
#else
      ret = sfwdata(sds_id,start,stride,edge,kmu)
      call hdf_error( ret,'cannot write kmu dataset',0)
#endif
      ret = sfendacc(sds_id)
      call hdf_error( ret, ' cannot close kmu dataset', 0)
c
c write two dimensional datasets to file
c
      do 10 idvar = 1, NUM2D
c
        sds_id = sfcreate( write_id,varid(idvar),
     &                    REAL_HDF,irank,dims2)
        call hdf_error(sds_id, 'cannot create 2-D dataset', 0)
        ret = sfsdtstr(sds_id,varname(idvar),varunit(idvar),' ',' ')
        call hdf_error( ret, 'cannot set 2-D data strings',0)
        fill_value = 0.0
        ret = sfsfill(sds_id,fill_value)
         call hdf_error( ret, 'cannot set 2-D fill value', 0)
c
c set the scales
c
        dimid1 = sfdimid(sds_id,0)
        call hdf_error( dimid1, 'error getting dimid 1', 0)
        dimid2 = sfdimid(sds_id,1)
        call hdf_error( dimid2, 'error getting dimid 2', 0)
        if(vartgrd(idvar))then
          ret = sfsdmname(dimid1, lonlabel(1))
          call hdf_error( ret, 'cannot set dimname 1', 0)
          ret = sfsdmname(dimid2, latlabel(1))
          call hdf_error( ret, 'cannot set dimname 2', 0)
          ret = sfsdscale(dimid1,dims2(1),REAL_HDF,xt(i1))
          ret2= sfsdscale(dimid2,dims2(2),REAL_HDF,yt(j1))
        else
          ret = sfsdmname(dimid1, lonlabel(2))
          call hdf_error( ret, 'cannot set dimname 1', 0)
          ret = sfsdmname(dimid2, latlabel(2))
          call hdf_error( ret, 'cannot set dimname 2', 0)
          ret = sfsdscale(dimid1,dims2(1),REAL_HDF,xu(i1))
          ret2= sfsdscale(dimid2,dims2(2),REAL_HDF,yu(j1)) 
        endif
        call hdf_error( ret, 'cannot set x-scales', dimid1)
        call hdf_error( ret2, 'cannot set y-scales',dimid2)
c
        ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
        call hdf_error( ret, 'setting dimension string 1', 0)
        ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
        call hdf_error( ret, 'setting dimension string 2', 0)
c
c  write out the dataset
c
      do 130 j=j1,j2
        start(2) = j-j1
        if(idvar.eq.1) then
          ret = sfwdata(sds_id,start,stride,edge,h0(i1,j,np0))
        elseif(idvar.eq.2) then
          ret = sfwdata(sds_id,start,stride,edge,u0(i1,j,np0))
        elseif(idvar.eq.3) then 
          ret = sfwdata(sds_id,start,stride,edge,v0(i1,j,np0))
        endif
 130  continue
c
        call hdf_error( ret, 'writing 2-D dataset ', 0)
        ret = sfsattr(sds_id, LEVEL_ATTR, INTEGER_HDF, 1, 0)
        call hdf_error( ret, 'setting 2-D attribute', 0)
        ret = sfsattr(sds_id, TGRID_ATTR, INTEGER_HDF, 1,
     &                                    vartgrd(idvar))
        call hdf_error( ret, 'setting 2-D attribute', 0)
        ret = sfendacc(sds_id)
        call hdf_error( ret, 'cannot close 2-D dataset', 0)
c
 10   continue
c
c archive the 3-D datasets
c
      irank = 3
      start(1) = 0
      start(2) = 0
      start(3) = 0
      stride(1) = 1
      stride(2) = 1
      stride(3) = 1
c
      dims3(1) = km
      dims3(2) = i2-i1+1
      dims3(3) = j2-j1+1
      edge(1) = km
      edge(2) = i2-i1+1
      edge(3) = 1
c
      do 20 idvar = NUM2D+1,NARCHV
c
        sds_id = sfcreate(write_id,varid(idvar),
     &                   REAL_HDF,irank,dims3)
        call hdf_error(sds_id, 'cannot create 3-D dataset', 0)
        ret = sfsdtstr(sds_id,varname(idvar),varunit(idvar),' ',' ')
        call hdf_error( ret, 'cannot set 3-D data strings', 0)
        fill_value = 0.0
        ret = sfsfill(sds_id,fill_value)
        call hdf_error( ret, 'cannot set 3-D fill value', 0)
c
        dimid1 = sfdimid(sds_id,0)
        call hdf_error( dimid1, 'Cannot get dimid 1:',dimid1) 
        dimid2 = sfdimid(sds_id,1)
        call hdf_error( dimid2, 'Cannot get dimid 2:',dimid2) 
        dimid3 = sfdimid(sds_id,2)
        call hdf_error( dimid3, 'Cannot get dimid 3:',dimid3) 
c
c set the scales for each dimensions
c
        ret = sfsdmname(dimid1,'DEPTH')
        ret = sfsdscale(dimid1,KM,REAL_HDF,zt)
        if( vartgrd(idvar) ) then
          ret = sfsdmname(dimid2,lonlabel(1))
          call hdf_error( ret, 'cannot set dimname 2', 0)
          ret = sfsdmname(dimid3, latlabel(1))
          call hdf_error( ret, 'cannot set dimname 3', 0)
          ret = sfsdscale(dimid2,dims3(2),REAL_HDF,xt(i1))
          ret2= sfsdscale(dimid3,dims3(3),REAL_HDF,yt(j1))
        else
          ret = sfsdmname(dimid2,lonlabel(2))
          call hdf_error( ret, 'cannot set dimname 2', 0)
          ret = sfsdmname(dimid3,latlabel(2))
          call hdf_error( ret, 'cannot set dimname 3', 0)
          ret = sfsdscale(dimid2,dims3(2),REAL_HDF,xu(i1))
          ret2= sfsdscale(dimid3,dims3(3),REAL_HDF,yu(j1))
        endif
        call hdf_error( ret, 'cannot set dimension scale 2' , 0)
        call hdf_error( ret2, 'cannot set dimension scale 3' ,0)
        ret = sfsdmstr(dimid1,labels3(1),units3(1),fmats3(1))
        call hdf_error( ret, 'setting dim strings 1',dimid1)
        ret = sfsdmstr(dimid2,labels3(2),units3(2),fmats3(2))
        call hdf_error( ret, 'setting dim strings 2',dimid2)
        ret = sfsdmstr(dimid3,labels3(3),units3(3),fmats3(3))
        call hdf_error( ret, 'setting dim strings 3',dimid3)
c
        do 220 j=j1,j2
          start(3) = j-j1
          if(idvar.eq.4) then
            ret = sfwdata(sds_id,start,stride,edge,t(1,i1,j,1,np))
          elseif(idvar.eq.5) then
            ret = sfwdata(sds_id,start,stride,edge,t(1,i1,j,2,np))
          elseif(idvar.eq.6) then
            ret = sfwdata(sds_id,start,stride,edge,u(1,i1,j,np))
          elseif(idvar.eq.7) then
            ret = sfwdata(sds_id,start,stride,edge,v(1,i1,j,np))
#ifdef w_write
          elseif(idvar.eq.8) then
            ret = sfwdata(sds_id,start,stride,edge,w(1,i1,j))
#endif
          endif
          if(ret.ne.0)then
            write(stdout,*)' error writing 3-D dataset at j = ',j
            write(stdout,*)' start = ',start
            write(stdout,*)' stride = ',stride
            write(stdout,*)' edge = ',edge
          endif
          call hdf_error(ret,'error writing 3-D dataset ',idvar)
 220    continue
c
c write the dataset attributes
c
        ret = sfsattr(sds_id, LEVEL_ATTR, 
     &                INTEGER_HDF, 1, 0)
        call hdf_error(ret,' error writing 3-D attribute', 0)
        ret = sfsattr(sds_id, TGRID_ATTR, 
     &                INTEGER_HDF, 1,vartgrd(idvar))
        call hdf_error(ret,' error writing 3-D attribute', 0)
c
c End access to this dataset
c
        ret = sfendacc(sds_id)
        call hdf_error( ret, 'cannot close 3-D dataset', 0)
c
c
 20   continue
c
c Now write the global attributes for the whole file
c
      ret = sfsattr(write_id, FMODE_ATTR, 
     &              INTEGER_HDF, 1, MODELLED)
      call hdf_error(ret,' error writing global attribute', 0)
      ret = sfsattr(write_id, FTYPE_ATTR, 
     &              INTEGER_HDF, 1, RESTART)
      call hdf_error(ret,' error writing global attribute', 0)
c
c set rotation attribute.  1= non-rotated 2=rotated
c
      ret = sfsattr(write_id, ROTATION_ATTR, 
     &              INTEGER_HDF, 1, 1)
      call hdf_error(ret,' error writing global attribute', 0)
      ret = sfend(write_id)
      call hdf_error( ret,' error closing SD interface', 0)
c
        end


      subroutine snap_hdf
      implicit none
c
c=======================================================================
c  This routine produces hdf snapshots.  The present version of the
c  routine saves the fields h0, u0, v0.
c  The generated file name starts with 's' and ends with '.hdf'.
c=======================================================================
c
#include "param.h"
#include "ctmngr.h"
#include "frees.h"
#include "iounit.h"
#include "levind.h"
#include "slabs.h"
#include "switch.h"
#include "timelv.h"
c
c using multi-file interface HDF3.3r3 routines for the data
c
# include "hdf.inc"
# include "coord.h"
# include "hdf.h"
c
c  local variables
c
      integer i1,i2,ifill_value,idvar
      _MOMA_REAL fill_value
      integer*4 write_id,sds_id,dimid1,dimid2,ret,ret2,
     &  irank,start(MAXDIMS),stride(MAXDIMS),edge(MAXDIMS)
      integer j,j1,j2
c
      write(hdffile, '(i10.10)') nint(totday)
      hdffile = 's'//hdffile(7:10)//cfext(1)
      inquire(file = hdffile, exist = around )
      if(around) then
         write( hdffile,'(i10.10)') itt
         hdffile = 's'// hdffile(7:10)//cfext(2)
      endif
c        
      write_id = sfstart(hdffile, DFACC_CREATE)
      if(write_id.eq.-1) then
        write(stdout,*)' error opening the file'
        return
      endif
      write (stdout,24) itt, hdffile
  24  format('  Writing hdf snapshot file for itt = ',i7,/,
     &       '  hdf filename = ',a) 
c
c write the dataset containing 'itt'
c
      irank = 1
      sds_id = sfcreate( write_id,tlabel(1),
     &                  INTEGER_HDF,irank,itdims )
      call hdf_error(sds_id,'cannot create itt dataset', 0)
      ret = sfsdtstr(sds_id, 'TIMESTEP', ' ', ' ',' ')
      call hdf_error( ret, 'cannot set itt datastrings', 0)
      ret = sfwdata(sds_id, 0,1,itdims, itt) 
      call hdf_error( ret, 'cannot write itt dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close itt dataset', 0)
c
c write the dataset containing 'totsec'
c
      sds_id = sfcreate(write_id,tlabel(2),
     &                 REAL_HDF,itsize,itdims)
      call hdf_error(sds_id,'cannot create totsec dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Seconds', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set totsec datastrings', 0)
      ret = sfwdata(sds_id, 0, 1, itdims, totsec)
      call hdf_error( ret, 'cannot write totsec dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close totsec dataset', 0)
c
c write the dataset containing 'totday'
c
      sds_id = sfcreate(write_id,tlabel(3),
     &                 REAL_HDF,itsize,itdims)
      call hdf_error(sds_id,'cannot create totday dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Day', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set totday datastrings', 0)
      ret = sfwdata( sds_id, 0, 1,itdims, totday)
      call hdf_error( ret, 'cannot write totday dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close totday dataset', 0)
c
c write the dataset containing 'years'
c
      sds_id = sfcreate(write_id,tlabel(4),
     &                 REAL_HDF,itsize,itdims)
      call hdf_error(sds_id,'cannot create years dataset', 0)
      ret = sfsdtstr(sds_id, 'Model Year', ' ', ' ', ' ')
      call hdf_error( ret, 'cannot set years datastrings', 0)
      ret = sfwdata( sds_id,0,1,itdims,years)
      call hdf_error( ret, 'cannot write years dataset', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close years dataset', 0)
c
c initialise the KMT array field.
c
      irank = 2
      j1 = jcbeg-1
      j2 = jcend+1
#ifdef cyclic
      i1 = icbeg
      i2 = icend
#else
      i1 = icbeg-1
      i2 = icend+1
#endif
      irank = 2
      start(1)= 0
      start(2)= 0
      stride(1) = 1
      stride(2) = 1
      dims2(1) = i2-i1+1
      dims2(2) = j2-j1+1
      edge(1)  = i2-i1+1
      edge(2)  = 1

c
c call the HDF function to create the dataset.
c
      sds_id = sfcreate( write_id,'KMT',
     &                  INTEGER_HDF, irank, dims2) 
      call hdf_error(sds_id,'cannot create kmt dataset', 0)
      ret = sfsdtstr(sds_id, 'KMT', ' ', ' ', ' ')
      call hdf_error( ret, ' cannot set kmt datastrings', 0)
      ifill_value = 999
      ret = sfsfill(sds_id,ifill_value)
      call hdf_error( ret, ' cannot set kmt fill value', 0)
c
      dimid1 = sfdimid(sds_id,0)
      call hdf_error( dimid1, ' error getting dimid', 1)
      dimid2 = sfdimid(sds_id,1)
      call hdf_error( dimid2, ' error getting dimid', 2)
      ret = sfsdmname(dimid1,lonlabel(1))
      call hdf_error( ret, ' cannot set dimname', 1)
      ret = sfsdmname(dimid2,latlabel(1))
      call hdf_error( ret, ' cannot set dimname', 2)
      ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
      call hdf_error( ret, ' setting dimension string', 1)
      ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
      call hdf_error( ret, ' setting dimension string', 2)
#ifndef REAL_8
c
c---------------------------------------------------------------------
c  skip writing scales when REAL_8 defined.  This is a fix to overcome 
c  an error in pvm when lengths of scale and main variable (kmt) differ
c---------------------------------------------------------------------
c
      ret = sfsdscale(dimid1, dims2(1),REAL_HDF, xt(i1))
      call hdf_error( ret, 'cannot set scales 1', dimid1)
      ret = sfsdscale(dimid2, dims2(2),REAL_HDF, yt(j1))
      call hdf_error( ret, 'cannot set scales 2', dimid2)
#endif
      do 110 j=j1,j2
        start(2) = j-j1
        ret = sfwdata(sds_id,start,stride,edge,kmt(i1,j))
        call hdf_error( ret,'cannot write kmt dataset',0)
 110  continue
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)
c
c initialise the KMU array field.
c
      sds_id = sfcreate( write_id,'KMU',
     &                  INTEGER_HDF,irank,dims2)
      call hdf_error(sds_id,'cannot create kmu dataset', 0)
      ret = sfsdtstr(sds_id, 'KMU', ' ', ' ',' ')
      call hdf_error( ret, 'cannot set kmu datastrings', 0)
      ifill_value = 999
      ret = sfsfill(sds_id,ifill_value)
      call hdf_error( ret, 'cannot set kmu fill value', 0)
c
      dimid1 = sfdimid(sds_id,0)
      call hdf_error( dimid1, 'error getting dimid', 1)
      dimid2 = sfdimid(sds_id,1)
      call hdf_error( dimid2, 'error getting dimid', 2)
      ret = sfsdmname(dimid1,lonlabel(2))
      call hdf_error( ret, 'cannot set dimname', 1)
      ret = sfsdmname(dimid2,latlabel(2))
      call hdf_error( ret, 'cannot set dimname', 2)
c
      ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
      call hdf_error( ret, 'setting dimension string', 1)
      ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
      call hdf_error( ret, 'setting dimension string', 2)
#ifndef REAL_8
c
c---------------------------------------------------------------------
c  skip writing scales when REAL_8 defined.  This is a fix to overcome 
c  an error in pvm when lengths of scale and main variable (kmt) differ
c---------------------------------------------------------------------
c
      ret = sfsdscale(dimid1, dims2(1),REAL_HDF,xu(i1))
      call hdf_error( ret,'cannot set scales 1', dimid1)
      ret = sfsdscale(dimid2, dims2(2),REAL_HDF,yu(j1))
      call hdf_error( ret,'cannot set scales 2', dimid2)
#endif
      do 120 j=j1,j2
        start(2) = j-j1
        ret = sfwdata(sds_id,start,stride,edge,kmu(i1,j))
        call hdf_error( ret,'cannot write kmu dataset',0)
 120  continue
      ret = sfendacc(sds_id)
      call hdf_error( ret, ' cannot close kmu dataset', 0)
c
c
c write two dimensional datasets to file
c
      do 10 idvar = 1, NUM2D
c
        sds_id = sfcreate( write_id,varid(idvar),
     &                    REAL_HDF,irank,dims2)
        call hdf_error(sds_id, 'cannot create 2-D dataset', 0)
        ret = sfsdtstr(sds_id,varname(idvar),varunit(idvar),' ',' ')
        call hdf_error( ret, 'cannot set 2-D data strings',0)
        fill_value = 0.0
        ret = sfsfill(sds_id,fill_value)
        call hdf_error( ret, 'cannot set 2-D fill value', 0)
c
c set the scales for the first dimension
c
        dimid1 = sfdimid(sds_id,0)
        call hdf_error( dimid1, 'error getting dimid', 1)
        dimid2 = sfdimid(sds_id,1)
        call hdf_error( dimid2, 'error getting dimid', 2)
c
        if(vartgrd(idvar))then
          ret = sfsdmname(dimid1, lonlabel(1))
          call hdf_error( ret, 'cannot set dimname', 1)
          ret = sfsdmname(dimid2, latlabel(1))
          call hdf_error( ret, 'cannot set dimname', 2)
          ret = sfsdscale(dimid1,dims2(1),REAL_HDF,xt(i1))
          ret2= sfsdscale(dimid2,dims2(2),REAL_HDF,yt(j1))
        else
          ret = sfsdmname(dimid1, lonlabel(2))
          call hdf_error( ret, 'cannot set dimname', 1)
          ret = sfsdmname(dimid2, latlabel(2))
          call hdf_error( ret, 'cannot set dimname', 2)
          ret = sfsdscale(dimid1,dims2(1),REAL_HDF,xu(i1))
          ret2= sfsdscale(dimid2,dims2(2),REAL_HDF,yu(j1)) 
        endif
        call hdf_error( ret, 'cannot set x-scales', dimid1)
        call hdf_error( ret2, 'cannot set y-scales',dimid2)
c
        ret = sfsdmstr(dimid1,labels2(1),units2(1),fmats2(1))
        call hdf_error( ret, 'setting dimension string', 1)
        ret = sfsdmstr(dimid2,labels2(2),units2(2),fmats2(2))
        call hdf_error( ret, 'setting dimension string', 2)
c
c  write out the dataset
c
      do 130 j=j1,j2
        start(2) = j-j1
        if(idvar.eq.1) then
          ret = sfwdata(sds_id,start,stride,edge,h0(i1,j,np0))
        elseif(idvar.eq.2) then
          ret = sfwdata(sds_id,start,stride,edge,u0(i1,j,np0))
        elseif(idvar.eq.3) then 
          ret = sfwdata(sds_id,start,stride,edge,v0(i1,j,np0))
        endif
 130  continue
        call hdf_error( ret, 'writing 2-D dataset ', 0)
        ret = sfsattr(sds_id, LEVEL_ATTR, INTEGER_HDF, 1, 0)
        call hdf_error( ret, 'setting attribute', 0)
        ret = sfsattr(sds_id, TGRID_ATTR, INTEGER_HDF,
     1    1,vartgrd(idvar))
        call hdf_error( ret, 'setting attribute', 0)
        ret = sfendacc(sds_id)
        call hdf_error( ret, 'cannot close dataset', 0)
c
 10   continue
c
c Now write the global attributes for the whole file
c
      ret = sfsattr(write_id, FMODE_ATTR, 
     &              INTEGER_HDF, 1, MODELLED)
      call hdf_error(ret,' error writing global attribute', 0)
      ret = sfsattr(write_id, FTYPE_ATTR, 
     &              INTEGER_HDF, 1, RESTART)
      call hdf_error(ret,' error writing global attribute', 0)
c
c set rotation attribute.  1= non-rotated 2=rotated
c
      ret = sfsattr(write_id, ROTATION_ATTR, 
     &              INTEGER_HDF, 1, 1)
      call hdf_error(ret,' error writing global attribute', 0)
      ret = sfend(write_id)
      call hdf_error( ret,' error closing SD interface', 0)
c
        end

      subroutine hdf_error(istat,cstr,info)
      implicit none
c
c=======================================================================
c subroutine to check return codes from the hdf calls
c   istat - return code
c   cstr  - character string explaining the error
c   info  - optional integer information (printed if non-zero)
c=======================================================================
c
#include "param.h"
        character cstr*(*)
        integer istat, info
c
        if(istat .eq. -1) then
          if(info.ne.0)then
            write(stdout,*)'HDF Error: ',cstr, info
          else
            write(stdout,*)'HDF Error: ',cstr
          endif
          stop
        endif
c
        return
        end

      subroutine archrd_hdf
      implicit none
c
c=======================================================================
c  subroutine to read hdf version of the restart file
c=======================================================================
c
#include "param.h"
#include "ctmngr.h"
#include "frees.h"
#include "iounit.h"
#include "levind.h"
#include "slabs.h"
#include "switch.h"
#include "timelv.h"
c
c  hdf '.h' files
c
#include "hdf.inc"
#include "coord.h"
#include "hdf.h"
c
c  local variables
c
      integer*4 file_id,sds_id,ret,
     &          start(MAXDIMS),stride(MAXDIMS),edge(MAXDIMS)
      integer index,j1,j2,i1,i2,j,j3,j4,idvar,i3,i4,i,k
c
c  open file
c
c
      if(fnrest(1:1).eq.' ')then
        write(stdout,*)' archrd_hdf: restart filename not given'
        write(stdout,*)' Program stopping ...'
        stop
      endif
      inquire(file=fnrest,exist=around)
      if(.not.around)then
        write(stdout,*)' archrd_hdf: restart filename not present'
        write(stdout,*)' Filename = ',fnrest
        write(stdout,*)' Program stopping ...'
        stop
      endif
      file_id = sfstart(fnrest, DFACC_READ)
      call hdf_error( file_id,'cannot open restart dataset', 0)
c
c  read dataset information
c
      start(1) =0
      start(2) =0
      start(3) =0
      stride(1)=1
      stride(2)=1
      stride(3)=1
      edge(1)  =1
      edge(2)  =1
      edge(3)  =1
c
      index = sfn2index(file_id,tlabel(1))
      call hdf_error(index,'cannot find id for TIMESTEP', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for TIMESTEP', 0)
      ret = sfrdata(sds_id,start, stride,edge,itt)
      call hdf_error(ret,'error reading TIMESTEP', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close dataset', 0)
c
      index = sfn2index(file_id,tlabel(2))
      call hdf_error( index,'cannot find id for totsec', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for totsec', 0)
      ret = sfrdata(sds_id,start, stride,edge,totsec)
      call hdf_error(ret,'error reading totsec', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close totsec dataset', 0)
c
      index = sfn2index(file_id,tlabel(3))
      call hdf_error( index,'cannot find id for totday', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for totday', 0)
      ret = sfrdata(sds_id,start, stride,edge,totday)
      call hdf_error(ret,'error reading totday', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close totday dataset', 0)
c
      index = sfn2index(file_id,tlabel(4))
      call hdf_error( index,'cannot find id for years', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for years', 0)
      ret = sfrdata(sds_id,start, stride,edge,years)
      call hdf_error(ret,'error reading years', 0)
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close years dataset', 0)
c
c  read kmt
c
      j1 = jcbeg-1
      j2 = jcend+1
#ifdef cyclic
      i1 = icbeg
      i2 = icend
#else
      i1 = icbeg-1
      i2 = icend+1
#endif
      start(1) = 0
      start(2) = 0
      edge(1) = i2-i1+1
      edge(2) = 1
c
      index = sfn2index(file_id,'KMT')
      call hdf_error( index,'cannot find id for KMT', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for KMT', 0)
c
      do 210 j=j1,j2
        start(2) = j-j1
        print *,j,sds_id,start,stride,edge
        ret = sfrdata(sds_id,start,stride,edge,kmt(i1,j))
        call hdf_error(ret,'error reading KMT', 0)
 210  continue
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close KMT dataset', 0)
c
c  read the 2-D datasets
c
      do 10 idvar = 1, NUM2D
      index = sfn2index(file_id,varid(idvar))
      call hdf_error( index,'cannot find id for 2-D dataset', 0)
      sds_id = sfselect(file_id,index)
      call hdf_error(sds_id,'error getting index for 2-D data', 0)
c
      do 220 j=j1,j2
      start(2) = j-j1
      if(idvar.eq.1) then
        ret = sfrdata(sds_id,start,stride,edge,h0(i1,j,np0))
      elseif(idvar.eq.2) then
        ret = sfrdata(sds_id,start,stride,edge,u0(i1,j,np0))
      elseif(idvar.eq.3) then 
        ret = sfrdata(sds_id,start,stride,edge,v0(i1,j,np0))
      endif
      call hdf_error(ret,'error reading 2-D data', 0)
 220  continue
      ret = sfendacc(sds_id)
      call hdf_error( ret, 'cannot close 2-D dataset', 0)
  10  continue
c
c  read the 3-D dataset
c
      start(1) = 0
      start(2) = 0
      start(3) = 0
      edge(1) = km
      edge(2) = i2-i1+1
      edge(3) = 1
c
      do 20 idvar = NUM2D+1,NARCHV
        index = sfn2index(file_id,varid(idvar))
        call hdf_error( index,'cannot find id for 3-D dataset', 0)
        sds_id = sfselect(file_id,index)
        call hdf_error(sds_id,'error getting index for 3-D data', 0)
        do 230 j=j1,j2
        start(3) = j-j1
        if(idvar.eq.4) then
          ret = sfrdata(sds_id,start,stride,edge,t(1,i1,j,1,np))
        elseif(idvar.eq.5) then
          ret = sfrdata(sds_id,start,stride,edge,t(1,i1,j,2,np))
        elseif(idvar.eq.6) then 
          ret = sfrdata(sds_id,start,stride,edge,u(1,i1,j,np))
        elseif(idvar.eq.7) then 
          ret = sfrdata(sds_id,start,stride,edge,v(1,i1,j,np))
#ifdef w_write
        elseif(idvar.eq.8) then
          ret = sfrdata(sds_id,start,stride,edge,w(1,i1,j))
#endif
        endif
 230    continue
        call hdf_error(ret,'error reading 2-D data', 0)
        ret = sfendacc(sds_id)
        call hdf_error( ret, 'cannot close 2-D dataset', 0)
  20  continue
c
      ret = sfend(file_id)
      call hdf_error( ret,' error closing SD interface', 0)
c
c  set east-west boundary conditions
c
#ifdef cyclic
      do i1=1,nhalo
        i2 = i1+nhalo
        i3 = imtm2h+i1
        i4 = i3+nhalo
        do j=1,jmt
          kmt(i1,j) = kmt(i3,j)
          kmt(i4,j) = kmt(i2,j)
          h0(i1,j,np0) = h0(i3,j,np0)
          h0(i4,j,np0) = h0(i2,j,np0)
          u0(i1,j,np0) = u0(i3,j,np0)
          u0(i4,j,np0) = u0(i2,j,np0)
          v0(i1,j,np0) = v0(i3,j,np0)
          v0(i4,j,np0) = v0(i2,j,np0)
          do k=1,km
            t(k,i1,j,1,np) = t(k,i3,j,1,np)
            t(k,i4,j,1,np) = t(k,i2,j,1,np)
            t(k,i1,j,2,np) = t(k,i3,j,2,np)
            t(k,i4,j,2,np) = t(k,i2,j,2,np)
            u(k,i1,j,np)   = u(k,i3,j,np)
            u(k,i4,j,np)   = u(k,i2,j,np)
            v(k,i1,j,np)   = v(k,i3,j,np)
            v(k,i4,j,np)   = v(k,i2,j,np)
          enddo
        enddo
      enddo
#else
      if(nhalo.gt.1)then
        i2 = nhalo
        i3 = imt+1-nhalo
        do i1=1,nhalo-1
          i4=imt+1-i1 
          do j=j1,j2
            kmt(i1,j) = kmt(i2,j)
            kmt(i4,j) = kmt(i3,j)
            h0(i1,j,np0)  = h0(i2,j,np0)
            h0(i4,j,np0)  = h0(i3,j,np0)
            u0(i1,j,np0)  = u0(i2,j,np0)
            u0(i4,j,np0)  = u0(i3,j,np0)
            v0(i1,j,np0)  = v0(i2,j,np0)
            v0(i4,j,np0)  = v0(i3,j,np0)
            do k=1,km
              t(k,i1,j,1,np) = t(k,i2,j,1,np)
              t(k,i4,j,1,np) = t(k,i3,j,1,np)
              t(k,i1,j,2,np) = t(k,i2,j,2,np)
              t(k,i4,j,2,np) = t(k,i3,j,2,np)
              u(k,i1,j,np)   = u(k,i2,j,np)
              u(k,i4,j,np)   = u(k,i3,j,np)
              v(k,i1,j,np)   = v(k,i2,j,np)
              v(k,i4,j,np)   = v(k,i3,j,np)
            end do
          end do
        end do
      endif
#endif
c
c  set north-south boundary conditions
c
      if(nhalo.gt.1)then
        j2 = nhalo
        j3 = jmt+1-nhalo
        do j1=1,j2-1
          j4=jmt+1-j1 
          print *,'cyclic n-s',j1,j2,j3,j4
          do i=1,imt
            kmt(i,j1) = kmt(i,j2)
            kmt(i,j4) = kmt(i,j3)
            h0(i,j1,np0) = h0(i,j2,np0)
            h0(i,j4,np0) = h0(i,j3,np0)
            u0(i,j1,np0) = u0(i,j2,np0)
            u0(i,j4,np0) = u0(i,j3,np0)
            v0(i,j1,np0) = v0(i,j2,np0)
            v0(i,j4,np0) = v0(i,j3,np0)
            do k=1,km
              t(k,i,j1,1,np) = t(k,i,j2,1,np)
              t(k,i,j4,1,np) = t(k,i,j3,1,np)
              t(k,i,j1,2,np) = t(k,i,j2,2,np)
              t(k,i,j4,2,np) = t(k,i,j3,2,np)
              u(k,i,j1,np)   = u(k,i,j2,np)
              u(k,i,j4,np)   = u(k,i,j3,np)
              v(k,i,j1,np)   = v(k,i,j2,np)
              v(k,i,j4,np)   = v(k,i,j3,np)
            end do
          end do
        end do
      endif
c
c-----------------------------------------------
c marc 15.10.2001

c----------------------------------------------

	do i=1,imt
	do j=1,jmt
	do k=1,km
	 t(k,i,j,1,nc) = t(k,i,j,1,np)
	 t(k,i,j,1,nm) = t(k,i,j,1,np)
	 t(k,i,j,2,nc) = t(k,i,j,2,np)
	 t(k,i,j,2,nm) = t(k,i,j,2,np)

	 u(k,i,j,nc) = u(k,i,j,np)
	 u(k,i,j,nm) = u(k,i,j,np)
	 v(k,i,j,nc) = v(k,i,j,np)
	 v(k,i,j,nm) = v(k,i,j,np)
 
	 
	
	enddo	
	enddo	
	enddo	
c----------------------------------------------

      return
      end
#else  /* Some compilers do not like empty files */
      subroutine undefed_hdf
      end
#endif /*ncsa_hdf*/
