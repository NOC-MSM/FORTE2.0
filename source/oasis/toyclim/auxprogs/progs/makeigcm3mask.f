      subroutine makeigcm3mask
      parameter(mg=128,jg=32,mgpp=mg+2,igc=mgpp*2)

      real gsg(igc,jg)

      open(59,file='t42.59')

      read(59,*)((gsg(i,j),i=1,mg),j=1,jg),(adum,i=1,mg),
     $          ((gsg(i+mgpp,j),i=1,mg),j=jg,1,-1)

      close(59)
      open(3,file='working/igcm3mask.i1')
      do j=1,jg
         write (3,'(128I1)')(nint((gsg(i,j)/
     $                      (gsg(i,j)+.1))),i=1,mg)

      enddo
      do j=jg,1,-1
         write (3,'(128I1)')(nint((gsg(i+mgpp,j)/
     $                      (gsg(i+mgpp,j)+.1))),i=1,mg)
      enddo
      close(3)


      return
      end
