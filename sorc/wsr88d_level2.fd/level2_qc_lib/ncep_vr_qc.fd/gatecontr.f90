       Subroutine gatecontr(fld,ngate,nazim,nyq,intmax,val_con_jump)
       implicit none

       integer           :: ngate, nazim
       real,dimension(ngate,nazim):: fld
       real,dimension(nazim)      :: vrazim1,vrazim2
       real,dimension(nazim)      :: vrcon
       integer,dimension(ngate)   :: valmax, con_jump_max
       real,parameter             :: spval=999.0
       real               :: aa,nyq,bb
       integer            :: i,j,intmax,con_jump,val_con_jump
       
       valmax=0
       con_jump_max=0
       do i=1,ngate-1
          vrazim1=fld(i,:) 
          vrazim2=fld(i+1,:) 
          
          intmax=0
          con_jump=0
          vrcon=0.0
          do j=2,nazim
             if(vrazim1(j)<spval.and.vrazim2(j)<spval) then
               aa=vrazim1(j)-vrazim2(j) 
               if(abs(aa)>1.0*nyq)then
               intmax=intmax+1
               con_jump=con_jump+1
               vrcon(j)=con_jump
               else
               con_jump=0 
               end if
             else
               con_jump=0 
             end if
          end do
          valmax(i)=intmax
          con_jump_max(i)=maxval(vrcon)
       end do

       intmax=maxval(valmax)
       val_con_jump=maxval(con_jump_max)

       End subroutine gatecontr
