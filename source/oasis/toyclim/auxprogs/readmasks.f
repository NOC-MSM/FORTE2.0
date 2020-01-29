C read in gridpoint field                                               
      PARAMETER(NN=42,MM=42,NHEM=2,NL=35,MOCT=1,MG=128,JG=32,NWJ2=462   
     P         ,NCRAY=64,JGL=JG,NTRAC=1,NLEVRF=6)                       
      INTEGER imb,jm,imjm                                               
      PARAMETER (imb = 128, jm = 64, imjm = imb*jm)                     
                                                                        
C                                                                       
C                                                                       
C     Sets basic constants, especially those needed for array dimensions
C                                                                       
      PARAMETER(MH=2,PI=3.14159265359,PI2=2.0*PI                        
     +,NNP=NN+1,MGPP=MG+2,JGP=JG+1,JGG=JG*NHEM,JGGP=JGG+1,MJP=NWJ2+NWJ2 
     +,NLM=NL-1,NLP=NL+1,NLPP=NL+2,NLA=NL+3,NLB=NL+4,NL2=NL*NL          
     +,IDA=(MG+MG+MG)/2+1,IDB=NWJ2*NL,IDC=IDB+IDB,IDD=MGPP*NL           
     +,IDE=NL2*NN,IDF=NCRAY*(MG+1),IDG=JG*NL,IDH=JG*MG                  
     +,IDI=NNP/2,IDJ=IDI*IDI,IDK=NL*IDI,IDL=MGPP/2,IDM=NNP/2,IDN=IDM*NL 
     +,NWW=1+(MM-1)/MOCT)

      PARAMETER(IGA=NWJ2*NHEM,IGB=IDB*NHEM,IGC=MGPP*NHEM,IGD=IDD*NHEM   
     +,IGG=IDG*NHEM,IGL=IDL*NHEM,IGM=IDM*NHEM,IGN=IDN*NHEM              
     +,IGO=IGA+IGA,IGP=IGB+IGB,NFTWG=(5+NTRAC)*NL+3                     
     +,NFTGW=(6+3*NTRAC)*NL+2,NFTGD=(3+NTRAC)*NL,NLTR=NL*NTRAC)

      COMMON/TOPOGMMJ/GSGMMJ(IGC,JG),ofrac(igc,jg),GSG(IGC,JG)
      REAL GSGMMJ

      COMMON/GSG/gmap(mg,jg*2),gmap2(mg,jg*2),rmap(mg,jg*2),
     1   gmap3(mg,jg*2),gmap4(mg,jg*2),
     1   runoff(50,2,33),iareanum   
         INTEGER runoff,iareanum,ilando(180,88),ilando2(180,88)


      open(1,file="ocean.kmtc.arctic_ridge_dp_ds")
      read(1,*)
      do j=88,1,-1
      read(1,"(1x,100i3)")(ilando(i,j),i=1,180)
      end do
      do i=154,168
      ilando(i,85)=0.
      end do
      do i=138,150
      ilando(i,85)=0.
      end do
      do j=84,84
      do i=7,10
      ilando(i,j)=0.
      end do
      end do
      do j=84,84
      do i=48,48
      ilando(i,j)=0.
      end do
      end do
      do j=79,80
      do i=29,29
      ilando(i,j)=0.
      end do
      end do
      do j=81,81
      do i=29,30
      ilando(i,j)=0.
      end do
      end do
      do j=81,81
      do i=133,134
      ilando(i,j)=0.
      end do
      end do
      do j=82,82
      do i=134,134
      ilando(i,j)=0.
      end do
      end do
      do j=77,77
      do i=170,173
      ilando(i,j)=0.
      end do
      end do
      do j=61,61
      do i=68,68
      ilando(i,j)=0.
      end do
      end do
      close(1)
      do j=51,51
      do i=50,50
      ilando(i,j)=0.
      end do
      end do
      do j=11,11
      do i=150,152
      ilando(i,j)=0.
      end do
      end do
      do j=17,17
      do i=147,147
      ilando(i,j)=0.
      end do
      end do

      do j=88,1,-1
      do i=1,180
      ilando2(i,j)=ilando(i,j)
      end do
      end do
      do j=88,1,-1
      do i=1,180
      if(ilando(i,j).gt.0)ilando(i,j)=1
      end do
      end do
      do j=88,1,-1
      do i=1,180
      ilando(i,j)=1-ilando(i,j)
      end do
      end do
      write(6,*) " ocean model mask looks like "
      do j=88,1,-1
      write(6,"(180i1)")(ilando(i,j),i=1,180)
      end do
      open(1,file="ocean.kmtc.arctic_ridge_dp_ds_mk2")
      write(1,"(a16,i3,8x,i2,8x,i2)") "kmt            ",180,88,15
      do j=88,1,-1
      write(1,"(1x,100i3)")(ilando2(i,j),i=1,180)
      end do
      close(1)

c     read in the preprepared runoff grid (non-polar)                   
!     open(50,file='orogdata/T42.runoff-real.cmip_poles2.grid'          
!    $,status='old')                                                    
!                                                                       
!      read (50,*) iareanum                                             
!      write(6,*) iareanum                                              
!                                                                       
!     do iarea=1,iareanum                                               
c      read in the land box perimeter                                   
!      read (50,*)                                                      
!        read (50,*) runoff(1,1,iarea),runoff(1,2,iarea)                
!    $             ,runoff(2,1,iarea),runoff(2,2,iarea)                 
!        read (50,*) runoff(3,1,iarea)                                  
!        do ncoast=1, runoff(3,1,iarea)                                 
!        read (50,*) runoff(3+ncoast,1,iarea),runoff(3+ncoast,2,iarea)  
!        end do                                                         
!     end do                                                            
!     close(50)

C skipping extra line on equator                                        
c tstar goes from 1 at pole to jg at equator, gsg made the same         
        open(50,file='t42.59',status='old')                    
                                                                        
        read(50,*)((gsgmmj(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),          
     $      ((gsgmmj(i+mgpp,j),i=1,mg),j=jg,1,-1)                       
        close(50)                                                       
c print out land mask                                                   
      open(50,file='ofrac_newcalc.igcm3',status='old')                 
      do j=1,jg                                                         
      read(50,*)(ofrac(i,j),i=1,mg)                                     
      end do                                                            
      do j=jg,1,-1                                                      
      read(50,*)(ofrac(i+mgpp,j),i=1,mg)                                
      end do                                                            
      close(50)                                                         
                                                                        
      write(6,*) 'fractional land mask looks like:'                     
      do j=1,jg                                                         
         write (6,'(128I1)')(nint( 10.*ofrac(i,j)  ) ,i=1,mg)           
      enddo                                                             
      do j=jg,1,-1                                                      
         write (6,'(128I1)')(nint( 10.*ofrac(i+mgpp,j) ) ,i=1,mg)       
      enddo                                                             
                                                                        
        write(6,*) 'land mask looks like:'                              
        do j=1,jg                                                       
          write (6,'(128I1)')                                           
     $         (nint((gsgmmj(i,j)/(gsgmmj(i,j)+.1))),i=1,mg)            
        enddo                                                           
        do j=jg,1,-1                                                    
          write (6,'(128I1)')                                           
     $         (nint((gsgmmj(i+mgpp,j)/(gsgmmj(i+mgpp,j)+.1))),i=1,mg)  
        enddo                                                           
c      reformat gsg so it makes some sense...                           
      do j=1,jg                                                         
      do i=1,mg                                                         
          gmap(i,j)=gsgmmj(i,j)                                            
          gmap(i,(jg*2)+1-j)=gsgmmj(i+mgpp,j)                              
          rmap(i,j)=ofrac(i,j)                                          
          rmap(i,(jg*2)+1-j)=ofrac(i+mgpp,j)                            
      if(gmap(i,j).gt.0.)then
      write(99,*)gmap(i,j),rmap(i,j)
      endif
      if(gmap(i,j).eq.0.)then
      write(98,*)gmap(i,j),rmap(i,j)
      endif
      end do                                                            
      end do

      write(6,*) 'fractional land mask looks like:'
      do j=1,jg*2
         write (6,'(128I1)')(nint( 10.*rmap(i,j)  ) ,i=1,mg)
      enddo

        write(6,*) 'land mask looks like:'
        do j=1,jg*2
          write (6,'(128I1)')
     $         (nint((gmap(i,j)/(gmap(i,j)+.1))),i=1,mg)
        enddo
      n1=0
      n2=0
      n3=0
      n4=0
      n5=0
      n6=0
      n7=0
      n8=0
      n9=0
      rmin=999.
      do j=1,jg*2
      do i=1,mg
      if(gmap(i,j).eq.0. .and. rmap(i,j).lt.1.)then
c     write(6,"(a10,2i3,2e16.8)") " problem? ", i,j,gmap(i,j), rmap(i,j)
      if(rmap(i,j).lt.rmin .and. rmap(i,j).gt.0.1)rmin=rmap(i,j)
      if(rmap(i,j).gt.0.1 .and. rmap(i,j).lt.0.2)n1=n1+1
      if(rmap(i,j).gt.0.2 .and. rmap(i,j).lt.0.3)n2=n2+1
      if(rmap(i,j).gt.0.3 .and. rmap(i,j).lt.0.4)n3=n3+1
      if(rmap(i,j).gt.0.4 .and. rmap(i,j).lt.0.5)n4=n4+1
      if(rmap(i,j).gt.0.5 .and. rmap(i,j).lt.0.6)n5=n5+1
      if(rmap(i,j).gt.0.6 .and. rmap(i,j).lt.0.7)n6=n6+1
      if(rmap(i,j).gt.0.7 .and. rmap(i,j).lt.0.8)n7=n7+1
      if(rmap(i,j).gt.0.8 .and. rmap(i,j).lt.0.9)n8=n8+1
      if(rmap(i,j).gt.0.9 .and. rmap(i,j).lt.1.0)n9=n9+1
      endif
      end do
      end do

      do j=1,jg*2
      do i=1,mg
      if(gmap(i,j).eq.0. .and. rmap(i,j).eq.0.)then
      write(6,"(a10,2i3,2e16.8)") " problem3? ", i,j,gmap(i,j),
     1    rmap(i,j)
      gmap3(i,j)=9.
      else
      gmap3(i,j)=gmap(i,j)
      endif
      end do
      end do

      do j=1,jg*2
      do i=1,mg
      if(gmap(i,j).gt.0. .and. rmap(i,j).eq.1.)then
      write(6,"(a11,2i3,2e16.8)") " problem2? ", i,j,gmap(i,j),rmap(i,j)
      gmap4(i,j)=8.
      else
      gmap4(i,j)=gmap(i,j)
      endif
      end do
      end do

      do j=1,jg*2
      do i=1,mg
      if(gmap(i,j).eq.0. .and. rmap(i,j).lt.1.
     1   .and. rmap(i,j).gt.0.)then
      gmap2(i,j)=9.
      else if(gmap(i,j).gt.0. .and. rmap(i,j).gt.0.)then
      gmap2(i,j)=2.
      else
      gmap2(i,j)=nint( ( gmap(i,j)/( gmap(i,j)+.1) ) )
      endif
      end do
      end do

        write(6,*) 'land mask looks like:'
        do j=1,jg*2
          write (6,'(128I1)')
     $         (nint(gmap2(i,j)),i=1,mg)
        enddo

      write(6,*)" rmin=",rmin
      write(6,*)" n1=",n1
      write(6,*)" n2=",n2
      write(6,*)" n3=",n3
      write(6,*)" n4=",n4
      write(6,*)" n5=",n5
      write(6,*)" n6=",n6
      write(6,*)" n7=",n7
      write(6,*)" n8=",n8
      write(6,*)" n9=",n9


      write(6,*) " ---- "
      write(6,*) runoff(1,1,1)

c     iarea=1
      do iarea=1,iareanum
         do ncoast=1, runoff(3,1,iarea)                                 
         write (6,*) runoff(3+ncoast,1,iarea),runoff(3+ncoast,2,iarea)  
         end do
      write(6,*) " -gmap rmap ",iarea
         do ncoast=1, runoff(3,1,iarea)                                 
         write (6,*) gmap(runoff(3+ncoast,1,iarea),
     1   runoff(3+ncoast,2,iarea)  ),rmap(runoff(3+ncoast,1,iarea),
     1   runoff(3+ncoast,2,iarea)  )
         end do
      end do !iarea
      write(6,*) " ---- "
      write(6,*) gmap(102,2),rmap(102,2),gmap2(102,2)
        write(6,*) 'problem3 mask looks like:'
        do j=1,jg*2
          write (6,'(128I1)')
     $         (nint(gmap3(i,j)),i=1,mg)
        enddo
        write(6,*) 'problem3 mask looks like:'
        do j=1,jg*2
        do i=1,mg
c         write (6,'(I1)')
c    $         nint(gmap3(i,j))
        enddo
        enddo

        write(6,*) 'problem2 mask looks like:'
        do j=1,jg*2
          write (6,'(128I1)')
     $         (nint(gmap4(i,j)),i=1,mg)
        enddo
 


      stop
      end
