        subroutine makesstocean

        include "parameters.h"

        character*8 clocator
        REAL*8 infield(nxo*nyo)

        open(98,file="OUTPUT/sstocean",form="unformatted")

        clocator="SOSSTSST"

        do i=1,nxo*nyo
          infield(i)=0.
        end do

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="SOZONCUR"

        write(6,*)clocator
        write(98)clocator
        write(98)infield
   
        clocator="SOMERCUR"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        close(98)

        return
        end
