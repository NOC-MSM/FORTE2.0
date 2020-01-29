c      compile with -r8 or it doesn't work!!!!
      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
      real gsg(igc,jg),gsgo(igc,jg)
      COMMON/PHYS/  LBL,LVD,LCR,LLR,LRD,LCUBM,LCBADJ                         
     :              ,TSTAR(IGC,JG),QSTAR(IGC,JG)
     :              ,TSTARO(IGC,JG),TDEEPO(IGC,JG),smstar(igc,jg)            
     :              ,tdeep(igc,jg),hsnow(igc,jg),sqstar(igc,jg)              
     :              ,SALB(IGC,JG),SBAL(IGC,JG),BLCD(IGC)                     
     :              ,TSTARL(IGC,JG),QSTARL(IGC,JG)
     :              ,TSTAROL(IGC,JG),TDEEPOL(IGC,JG),smstarl(igc,jg)
     :              ,tdeepl(igc,jg),hsnowl(igc,jg),sqstarl(igc,jg)
     :              ,SALBL(IGC,JG),SBALL(IGC,JG),SVEGEL(IGC,JG)
     :              ,TSTAROC(IGC,JG),QSTAROC(IGC,JG),OFRAC(IGC,JG)
     :              ,TSTAROOC(IGC,JG),TDEEPOOC(IGC,JG),smstaroc(igc,jg)
     :              ,tdeepoc(igc,jg),hsnowoc(igc,jg),sqstaroc(igc,jg)
     :              ,SALBOC(IGC,JG),SBALOC(IGC,JG)
     :              ,SVEGE(IGC,JG),CD,DRAG,BLVAD,BLA,BLRH,BLVB(IGC)          
     :              ,AKVV,AKTV,AKQV,ESCONA,ESCONB,EPSIQ,CTQ,CCC              
     : ,ctqi,sdsn,shcs,shcsp,shcsn,skse,sksn,slhf,sd1,sd2,sdw                
     :        ,ssmc,sdsnd,LSL,sasnow,saice,shsstar,shsmax                    
     :     ,LOC,SHCO,SHCI,LNOICE,LOLDBL,LCOND,LNNSK,ITSLL,ITSLO              
     :              ,CCR,RCON,DTBUOY,TSLA,TSLB,TSLC,TSLD,CUT1,CUT2           
     :              ,NLCR,NCUTOP,CURHM,AKTC,AKQC,CUBMT,CBADJT,CBADJP         
       COMMON/CPIERS/ICFLAG(IGC,5,2),CFRAC(IGC,5),PNET(IGC,JG)               
     :               ,SNET(IGC,JG),RRFLUX(IGC,JG,6)                          
     :               ,SNETL(IGC,JG),SNETOC(IGC,JG)
      open(59,file='OUTPUT/ofrac.igcm3')

      do j=1,jg
      read(59,*)(gsg(i,j),i=1,mg)
      end do
      do j=jg,1,-1
      read(59,*)(gsg(i+mgpp,j),i=1,mg)
      end do

      close(59)
c------------------------

      open(59,file='INPUT/t42.59')                 

      read(59,*)((gsgo(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsgo(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)

      write(6,*) ' land mask looks like:'
      do j=1,jg
c        write (6,'(130I1)')(nint((gsgo(i,j)/
c    $                      (gsgo(i,j)+.1))),i=1,mgpp)
      enddo
      do j=jg,1,-1
c        write (6,'(130I1)')(nint((gsgo(i+mgpp,j)/
c    $                      (gsgo(i+mgpp,j)+.1))),i=1,mgpp)
      enddo


      write(6,*) 'fractional land mask looks like:'
      write(6,*) gsg(40,9)
      do j=1,jg
c        write (6,'(130I1)')(nint(10.*gsg(i,j)),i=1,mgpp)
      enddo
      do j=jg,1,-1
c        write (6,'(130I1)')(nint(10.*gsg(i+mgpp,j)),i=1,mgpp)
      enddo


     
c------------------------
      read(69,end=200) RCHECK
      write(6,*) rcheck
      read(69) rrkount,rm1tape,rday,CDOY,TSTAR,TDEEP,
     1   SMSTAR,QSTAR,
     $        HSNOW,SQSTAR,SALB,SBAL,TSTARO,TDEEPO,SNET,RM2TAPE
      write(6,*) rrkount,rm1tape,rday,cdoy,snet(10,10),rm2tape
      write(6,*) 'land mask looks like:'
      do j=1,jg
c        write (6,'(130I2)')(nint( 10.*sbal(i,j)  ) ,i=1,mgpp)
c        write (6,'(e16.8)')( tstar(i,j)  ,i=1,mgpp)
      enddo
      do j=jg,1,-1
c        write (6,'(130I2)')(nint( 10.*sbal(i+mgpp,j) ) ,i=1,mgpp)
      enddo

      fact=752.0152253
      zero=273.15
      write(6,*) " A "
      call interp(gsg,gsgo,tstar,tstarl)
      write(6,*) " B "
      call interpo(gsg,gsgo,tstar,tstaroc)
      write(6,*)fact*tstar(1,2)-zero,fact*tstaroc(1,2)
     1   -zero,gsg(1,2)
      write(6,*)fact*tstar(1,32)-zero,fact*tstaroc(1,32)
     1   -zero,gsg(1,32)
      write(6,*) " C "
      call interp(gsg,gsgo,tdeep,tdeepl)
      write(6,*) " D "
      call interpo(gsg,gsgo,tdeep,tdeepoc)
      write(6,*) " E "
      call interp(gsg,gsgo,smstar,smstarl)
      write(6,*) " F "
      call interpo(gsg,gsgo,smstar,smstaroc)
      write(6,*) " G "
      call interp(gsg,gsgo,qstar,qstarl)
      write(6,*) " H "
      call interpo(gsg,gsgo,qstar,qstaroc)
      write(6,*) " I "
      call interp(gsg,gsgo,hsnow,hsnowl)
      write(6,*) " J "
      call interpo(gsg,gsgo,hsnow,hsnowoc)
      write(6,*) " K "
      call interp(gsg,gsgo,sqstar,sqstarl)
      write(6,*) " L "
      call interpo(gsg,gsgo,sqstar,sqstaroc)
      write(6,*) " M "
      call interp(gsg,gsgo,salb,salbl)
      write(6,*) " N "
      call interpo(gsg,gsgo,salb,salboc)
      write(6,*) " O "
      call interp(gsg,gsgo,sbal,sball)
      write(6,*) " P "
      call interpo(gsg,gsgo,sbal,sbaloc)
      write(6,*) " Q "
      call interp(gsg,gsgo,tstaro,tstarol)
      write(6,*) " R "
c     call interpo(gsg,gsgo,tstaro,tstarooc)
      call interpo(gsg,gsgo,tstar,tstarooc)
      write(6,*)fact*tstaro(1,2)-zero,fact*tstarooc(1,2)
     1   -zero,gsg(1,2)
      write(6,*)fact*tstaro(1,32)-zero,fact*tstarooc(1,32)
     1   -zero,gsg(1,32)
      write(6,*) " S "
      call interp(gsg,gsgo,tdeepo,tdeepol)
      write(6,*) " T "
      call interpo(gsg,gsgo,tdeepo,tdeepooc)
      write(6,*) " U "
      call interp(gsg,gsgo,snet,snetl)
      write(6,*) " V "
      call interpo(gsg,gsgo,snet,snetoc)

      write(6,*) 'land mask 2 looks like:'
      do j=1,jg
c        write (6,'(130I2)')(nint(
c    1    100.*(tstarl(i,j)-tstaroc(i,j))  )
c    1    ,i=1,mgpp)
c        write (6,'(130I2)')(nint( 100.*(tstaroc(i,j))  )
c    1    ,i=1,mgpp)
      enddo
      do j=jg,1,-1
c     write (6,'(130I2)')(nint( 
c    1   100.*(tstarl(i+mgpp,j)-tstaroc(i+mgpp,j)) )
c    1    ,i=1,mgpp)
c     write (6,'(130I2)')(nint( 100.*(tstaroc(i+mgpp,j)) )
c    1    ,i=1,mgpp)
      enddo
      write(6,*) " tstarl(67,3) = ",tstarl(67,3)
      write(6,*) " tstar(67,3) = ",tstar(67,3)

      write(70) RCHECK
      write(70) rrkount,rm1tape,rday,CDOY,TSTARL,TDEEPL,
     1   SMSTARL,QSTARL,
     $        HSNOWL,SQSTARL,SALBL,SBALL,TSTAROL,TDEEPOL,
     1   SNETL,RM2TAPE

      write(71) RCHECK
      write(71) rrkount,rm1tape,rday,CDOY,TSTAROC,TDEEPOC,
     1   SMSTAROC,QSTAROC,
     $        HSNOWOC,SQSTAROC,SALBOC,SBALOC,TSTAROOC,
     1   TDEEPOOC,SNETOC,RM2TAPE

      stop
200   write(6,*) " end of file encountered "
      end

      subroutine interp(gsg,gsgo,svege,svege2)

      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
c      input arrays
      real*8 gsg(igc,jg)
      real*8 gsgo(igc,jg)
      real*8 svege(260,32)
c      intermediate calcn arrays
      real*8 gsga(128,64)
      real*8 gsgoa(128,64)
      real*8 svegea(128,64)
      real*8 svegenew(128,64)
c      output array
      real*8 svege2(260,32)

c      now in a position to alter vege types
c      at coastal points 
c      something more appropriate
c      if necessary
c      convert to an i-j array
      write(6,*) " HELLO ! "
      do j=1,32
      do i=1,128
c     write(6,*)i,j,65-j,gsg(i,j),gsgo(i,j)
         svegea(i,65-j)=svege(i,j)
         svegenew(i,65-j)=svege(i,j)
         gsga(i,65-j)=gsg(i,j)
         gsgoa(i,65-j)=gsgo(i,j)
      enddo
      enddo
      write(6,*) " HELLO ! "
      do j=1,32
      do i=1,128
         svegea(i,j)=svege(i+130,j)
         svegenew(i,j)=svege(i+130,j)
         gsga(i,j)=gsg(i+130,j)
         gsgoa(i,j)=gsgo(i+130,j)
      enddo
      enddo
      write(6,*) " HELLO ! "
c      do a distance weighted interpolation
      do i=1,128
      do j=1,64
c      first look for points where the fractional grid says
c      it's a coastal point, but gsgoa says it's ocean
      if(gsga(i,j).lt.1. .and. gsgoa(i,j).eq.0)then
      sumnum=0.
      sumden=0.
c      once you've found such a point look out to a
c      radius of two gridpoints in all directions
      do ii=-4,4
      do jj=-4,4
c      don't include the gridpoint itself of course
      if(.not. (ii.eq.0 .and. jj.eq.0) )then
      ip=i+ii
      if(ip.lt.0)ip=ip+128
c      the grid is periodic in i direction so if you look
c      too far in one direction you come back on the other
c      side of the domain
      ip=mod(ip,128)
      if(ip.eq.0)ip=128
      jp=j+jj
c      only include points which remain in the domain in the
c      j direction
      if(jp.ge.1 .and. jp.le.64)then
       if(gsgoa(ip,jp).gt.0.)then
        r=sqrt( real(ii)**2 + real(jj)**2 )
        sumnum=sumnum+svegea(ip,jp)*1./r**2
        sumden=sumden+1./r**2
       endif
      endif
      endif
      end do !ii
      end do !jj
c      update svegenew
      if(sumden.ne.0.)then
      svegenew(i,j)=sumnum/sumden
      else
c      you are in a bad way if this happens
c     svegenew(i,j)=0.
      write(6,*)i,j
      write(6,*) " sumden = ",sumden
      write(6,*) " sumnum = ",sumnum
      write(6,*) ' gsga = ',gsga(i,j)
      write(6,*) ' gsgoa = ',gsgoa(i,j)
      do jj=2,-2,-1
      write(6,"(5e16.8)")(gsgoa(i+ii,j+jj),ii=-2,2)
      end do
      stop 'bummer'
      endif

      endif
      end do
      end do

      do j=1,32
      do i=1,128
      svege2(i,j)=svegenew(i,2*jg+1-j)
      end do
      end do
      do j=1,32
      do i=1,128
      svege2(130+i,j)=svegenew(i,j)
      end do
      end do
      return
      end
      subroutine interpo(gsg,gsgo,svege,svege2)

      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
c      input arrays
      real*8 gsg(igc,jg)
      real*8 gsgo(igc,jg)
      real*8 svege(260,32)
c      intermediate calcn arrays
      real*8 gsga(128,64)
      real*8 gsgoa(128,64)
      real*8 svegea(128,64)
      real*8 svegenew(128,64)
c      output array
      real*8 svege2(260,32)

c      now in a position to alter vege types
c      at coastal points 
c      something more appropriate
c      if necessary
c      convert to an i-j array
      do j=1,32
      do i=1,128
         svegea(i,2*jg+1-j)=svege(i,j)
         svegenew(i,2*jg+1-j)=svege(i,j)
         gsga(i,2*jg+1-j)=gsg(i,j)
         gsgoa(i,2*jg+1-j)=gsgo(i,j)
      enddo
      enddo
      do j=1,32
      do i=1,128
         svegea(i,j)=svege(i+130,j)
         svegenew(i,j)=svege(i+130,j)
         gsga(i,j)=gsg(i+130,j)
         gsgoa(i,j)=gsgo(i+130,j)
      enddo
      enddo
c      do a distance weighted interpolation
      do i=1,128
      do j=1,64
c      first look for points where the fractional grid says
c      it's a coastal point, but gsgoa says it's land
      if(gsga(i,j).gt.0. .and. gsgoa(i,j).gt.0)then
      sumnum=0.
      sumden=0.
c      once you've found such a point look out to a
c      radius of two gridpoints in all directions
      do ii=-2,2
      do jj=-2,2
c      don't include the gridpoint itself of course
      if(.not. (ii.eq.0 .and. jj.eq.0) )then
      ip=i+ii
      if(ip.lt.0)ip=ip+128
c      the grid is periodic in i direction so if you look
c      too far in one direction you come back on the other
c      side of the domain
      ip=mod(ip,128)
      if(ip.eq.0)ip=128
      jp=j+jj
c      only include points which remain in the domain in the
c      j direction
      if(jp.ge.1 .and. jp.le.64)then
       if(gsgoa(ip,jp).eq.0.)then
        r=sqrt( real(ii)**2 + real(jj)**2 )
        sumnum=sumnum+svegea(ip,jp)*1./r**2
        sumden=sumden+1./r**2
c       write(6,"(2i4,4e16.8)")ip,jp,r,sumden,sumnum,svegea(ip,jp)
       endif
      endif
      endif
      end do !ii
      end do !jj
c      update svegenew
      if(sumden.ne.0.)then
      svegenew(i,j)=sumnum/sumden
      else
c      you are in a bad way if this happens
c     svegenew(i,j)=0.
      write(6,*)i,j
c     stop 'bummer'
      endif

      endif
      end do
      end do

      do j=1,32
      do i=1,128
      svege2(i,j)=svegenew(i,2*jg+1-j)
      end do
      end do
      do j=1,32
      do i=1,128
      svege2(130+i,j)=svegenew(i,j)
      end do
      end do
      return
      end
