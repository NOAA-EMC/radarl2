
subroutine build10_superres(lunout,sz,x,y,vid,nam,fix,packet)

     integer ::  lunout
     integer ::  sz
     integer ::  x
     integer ::  y
     real(8) ::  vid
     character(len=8) :: nam
     character(len=128) :: fix
     character(len=1050624) :: packet
     print *,'superez_sz=',sz
!    call tranrad_lev2_sub(lunout,sz,x,y,vid,nam,fix,packet)

end subroutine build10_superres
