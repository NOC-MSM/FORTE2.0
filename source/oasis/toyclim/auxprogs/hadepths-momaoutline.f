      integer jm, jh, im, ih
      integer MomaLons(90)
      integer MomaLats(45), maporig(288,144)
      integer oceankmt(30,135), momamap1(4050), momamap2(90,45)
      integer HadLatI(45), HadLonI(90)
      integer newkmt(90,45)

      real HadLat(45,2), HadLon(90,2)
      real orig(41472), hadarray(288,144),momarray(90,45)
 
      double precision gsg(2112), newgsg(2112), gsgmap(64,33)
      double precision newgsgmap(64,33)
      integer igcmap(64,33), gsgmask(64,33)
c
c     work out the nearest four Hadcm3 gridpoints
c     for any given Moma gridpoint
c
c     first, latitudes
c

      Mo=-88
      Ho=-89.375
      delM=4
      delH=1.25

      do jm=1,45
      MomaLats(jm)=Mo + (jm-1)*delM

      jh= 1 + (Mo-Ho + (jm-1)*delM) / delH

      HadLat(jm,1)=Ho + (jh-1)*delH
      HadLatI(jm)=jh
      end do
c
c     then longitudes
c

      Mo=-2
      Ho=0.625
    
      do im=1,90
      MomaLons(im)=Mo + (im-1)*delM

      ih= 1 + (Mo-Ho + (im-1)*delM) / delH

      HadLon(im,1)=Ho + (ih-1)*delH
      HadLonI(im)=ih
     
      end do

c     assign weights for Hadcm3 values according to distance
c     from the Moma Point
c
      do im=1,90
      HadLon(im,2)=1 - (MomaLons(im)-HadLon(im,1))/1.25 
      end do      

      do jm=1,45
      HadLat(jm,2)=1 - (MomaLats(jm)-HadLat(jm,1))/1.25 
      end do
      write(6,*) MomaLats
      write(6,*) MomaLons

c     write a sample point to check it's done OK
c
      write(6,*)MomaLons(3), MomaLats(38)

      write(6,*)HadLon(3,1), HadLonI(3), HadLon(3,1)+delH
     $        , HadLonI(3)+1, HadLon(3,2), 1-HadLon(3,2)

      write(6,*)HadLat(38,1), HadLatI(38), HadLat(38,1)+delH
     $        , HadLatI(38)+1, HadLat(38,2), 1-HadLat(38,2)

c     read in tdepth file, check we've got things aligned right
c     turn it upside down so it's the same way up as Moma and print
c     a version to look at 
c
      open(1, file='hadcm3.tdepths')

      do i=1,41472
      read(1,*)orig(i)
      end do

      do jh=144,1, -1
      do ih=1,288

c     arrange the hadcm3 data in a correctly dimesioned array
c     N-S like moma maps...
c
      hadarray(ih, 145-jh)=orig( (jh-1)*288+ih)
      if ( hadarray(ih, 145-jh).eq.0) then 
         maporig(ih, 145-jh)=1
      else
         maporig(ih, 145-jh)=0
      end if
      
      end do
      end do

      do jh=1,144
      write(70,'(288I1)')(maporig(ih,jh),ih=1,288)
      end do
      
c     open the moma map to compare with hadcm3's
  
      open(2,file='ocean.kmt')
      read(2,*)

      do jm=1,135
      read(2,*)(oceankmt(im,jm),im=1,30) 

      do im=1,30
      if (oceankmt(im,jm).eq.0) then
      momamap1((jm-1)*30+im)=1
      else
      momamap1((jm-1)*30+im)=0
      end if

      end do
      end do

      do jm=1,45
      write(71, '(90I1)')(momamap1(im+(jm-1)*90), im=1,90)
      end do

c     and now, we make a moma version of the had map using
c     the indexes  and weights we made before...
      
      do jm=1,45
      do im=1,90

      point1=hadarray( HadLonI(im)  , HadLatI(jm) ) * (HadLon(im,2) )
     $               * (HadLat(jm,2) )    

      point2=hadarray( HadLonI(im)+1, HadLatI(jm) ) * (1-HadLon(im,2) )
     $               * (HadLat(jm,2) )     

      point3=hadarray( HadLonI(im),  HadLatI(jm)+1 ) * (HadLon(im,2) )
     $               * (1-HadLat(jm,2) )     

      point4=hadarray( HadLonI(im)+1,HadLatI(jm)+1 ) * (1-HadLon(im,2) )
     $               * (1-HadLat(jm,2) )    

c    to conform to igcm grid, only take had value if we're considering
c    a point already classed as sea

      if (momamap1((jm-1)*90+im).eq.0) then 
        momarray(im,jm)=( point1 + point2 + point3 + point4 )

        if (momarray(im,jm).lt.1E-1) then
        momarray(im,jm)=25
        end if

      else
        momarray(im,jm)=0
      end if

      end do
      end do


c---check for freaky negative points

      do jm=1,45
      do im=1,90
     
      if (momarray(im,jm).lt.0) then
      write(99,*)'Negative Point!', momarray(im,jm)
      write(99,*)'MomIndex',im,jm
      write(99,*)'HadIndex',HadLonI(im), HadLatI(jm)
      write(99,*)'Weights',HadLon(im,2),HadLat(jm,2)
      write(99,*)'HadHeights',hadarray( HadLonI(im)  , HadLatI(jm) ),
     $hadarray( HadLonI(im)+1, HadLatI(jm) )
      write(99,*)hadarray( HadLonI(im),  HadLatI(jm)+1 ),
     $hadarray( HadLonI(im)+1,HadLatI(jm)+1 )

      momarray(im,jm)=0
      end if
      
c---write out an ascii 1's and 0's map to check outline OK

      if (momarray(im,jm).eq.0) then
      momamap2(im,jm)=1
      else
      momamap2(im,jm)=0
      end if

c---write the new map as a list for unimap to read
      write(86,*)momarray(im,46-jm)

      end do
      write(72,'(90I1)')(momamap2(im,jm),im=1,90)
      end do

c     re-scale everything to Moma's 1-15 spec
c ***hacked to get rid of shelves near coast. Badly****

      do jm=1,45
      do im=1,90

      if (momarray(im,jm).gt.4863.9) then
          newkmt(im,jm)=15
      else
      if (momarray(im,jm).gt.4064.2) then
         newkmt(im,jm)=14      
      else
      if (momarray(im,jm).gt.3321.8) then
         newkmt(im,jm)=13
      else
      if (momarray(im,jm).gt.2652.7) then
          newkmt(im,jm)=12
      else
      if (momarray(im,jm).gt.2067.4) then
         newkmt(im,jm)=11      
      else
      if (momarray(im,jm).gt.1570.3) then
         newkmt(im,jm)=10
      else
      if (momarray(im,jm).gt.1160.5) then
          newkmt(im,jm)=9
      else
      if (momarray(im,jm).gt.832.51) then
         newkmt(im,jm)=8      
      else
      if (momarray(im,jm).gt.577.75) then
         newkmt(im,jm)=7
      else
      if (momarray(im,jm).gt.385.64) then
         newkmt(im,jm)=6      
      else
      if (momarray(im,jm).gt.245.01) then
         newkmt(im,jm)=5
      else
      if (momarray(im,jm).gt.145.08) then
          newkmt(im,jm)=4
      else
      if (momarray(im,jm).gt.76.15) then
         newkmt(im,jm)=4      
      else
      if (momarray(im,jm).gt.30) then
         newkmt(im,jm)=4
      else
      if (momarray(im,jm).gt.1E-2) then
         newkmt(im,jm)=4
      else
         newkmt(im,jm)=0
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
      end if
     
      end do
      end do
     
      open(9,file='new.ocean.kmt')
      write(9,*)'kmt             90        45        15'
      do jm=45,1,-1
      do im=1,90
      write(95,*)newkmt(im,jm)
      end do
      write(9,16)(newkmt(im,46-jm),im=1,90)
      end do
 
  15  format(1x,a8,6i10)
  16  format(1x,100i3)


      end
