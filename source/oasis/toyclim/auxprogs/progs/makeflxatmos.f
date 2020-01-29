        subroutine makeflxatmos

c Field names are important. do not change.

        include "parametersa.h"

        character*8 clocator
        REAL*8 infield(nxa*nya)

        open(98,file="OUTPUT/flxatmos",form="unformatted")

        clocator="CONSFTOT"

        do i=1,nxa*nya
          infield(i)=0.
        end do

        write(6,*)clocator
        write(98)clocator
        write(98)infield
    
        clocator="COSHFTOT"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="COWATFLU"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="CORUNOFF"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

c        clocator="CORUNOFF"
c
c        write(6,*)clocator
c        write(98)clocator
c        write(98)infield

        clocator="COZOTAUX"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="COMETAUY"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="COZOTAUV"
c        clocator="COZOTAUU"

        write(6,*)clocator
        write(98)clocator
        write(98)infield

        clocator="COMETAUU"
c        clocator="COMETAUV"

        write(6,*)clocator
        write(98)clocator
        write(98)infield



        close(98)

        return
        end
