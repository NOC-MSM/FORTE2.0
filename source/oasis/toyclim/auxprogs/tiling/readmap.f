      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)
  
      real gsg(igc,jg)
      real*8 svege(igc,jg),nsvege(igc,jg)
      integer ivege(igc,jg), newvege(igc,jg)

      open(59,file='INPUT/t42.59')                 

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg), 
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)


      open(3,file="OUTPUT/igcm3mask.i1")
      do j=1,jg
         write (3,'(130I1)')(nint((gsg(i,j)/
     $                      (gsg(i,j)+.1))),i=1,mgpp)
      enddo
      do j=jg,1,-1
         write (3,'(130I1)')(nint((gsg(i+mgpp,j)/
     $                      (gsg(i+mgpp,j)+.1))),i=1,mgpp)
      enddo
      close(3)
  
      jj=65
      do j=1,jg
      jj=jj-1
         write (6,'(2I2)')jj,(nint((gsg(i,j)/
     $                      (gsg(i,j)+.1))),i=63,63)
      enddo
      do j=jg,1,-1
      jj=jj-1
         write (6,'(2I2)')jj,(nint((gsg(i+mgpp,j)/
     $                      (gsg(i+mgpp,j)+.1))),i=63,63)
      enddo

      stop
      end


     
