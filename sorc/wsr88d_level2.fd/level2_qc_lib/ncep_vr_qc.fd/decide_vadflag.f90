
SUBROUTINE DECIDE_VADFLAG

 use variable_define

 implicit none

 integer :: i,j

 ivadflag=-10
 do ilevel = 1,zlevel
   if( abs(ustor(ilevel))<spval ) then
     ivadflag=10
     exit
   endif
 enddo
 print*,' '
 if (ivadflag==-10) then
     print*,'VADflag==',ivadflag,' There is no VAD, no acting of dealising'
 elseif (ivadflag==10) then
     print*,'VADflag==',ivadflag,' dealiasing method will be SIMPLE_METHOD'
 endif
 print*,' '

END SUBROUTINE DECIDE_VADFLAG
