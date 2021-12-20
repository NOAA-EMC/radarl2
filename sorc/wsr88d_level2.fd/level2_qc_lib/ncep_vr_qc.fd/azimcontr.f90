       Subroutine azimcontr(fld,ngate,nazim,nyq,intmax,val_con_jump)
       implicit none

       integer           :: ngate, nazim
       real,dimension(ngate,nazim):: fld
       real,dimension(ngate)      :: vrazim1,vrazim2
       real,dimension(ngate)      :: vrcon
       integer,dimension(nazim)   :: valmax, con_jump_max
       integer,dimension(1)       :: jump_max_loc, con_jump_max_loc
       real,parameter             :: spval=999.0
       real               :: aa,nyq,bb,wnyq
       integer            :: i,j,intmax,con_jump,val_con_jump
       integer            :: debug

       wnyq=0.5
       
       debug=1
       if(debug==0)then
       open(99,file='jump_location')
       write(99,*)'ngate,nazim', ngate,nazim
            do j=1,ngate
            write(99,*)fld(j,367),fld(j,1)
            end do
       end if

       valmax=0
       con_jump_max=0
       do i=1,nazim
          if(i<nazim)then
          vrazim1=fld(:,i) 
          vrazim2=fld(:,i+1) 
          else
          vrazim1=fld(:,nazim) 
          vrazim2=fld(:,1) 
!           if(debug==1)then
!           do j=1,ngate
!           write(99,*)vrazim1(j),vrazim2(j)
!           end do
!           end if
          end if
          
          intmax=0
          con_jump=0
          vrcon=0.0
          do j=2,ngate
             if(vrazim1(j)<spval.and.vrazim2(j)<spval) then
               aa=vrazim1(j)-vrazim2(j) 
               if(abs(aa)>wnyq*nyq)then
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

       jump_max_loc=maxloc(valmax)
       con_jump_max_loc=maxloc(con_jump_max)
 
       if(debug==0)then
!      open(99,file='jump_location')
       write(99,*)'nazim:',nazim
       write(99,*)'jump_max_loc:',intmax,jump_max_loc
       do i=1,ngate
         if(fld(i,jump_max_loc(1))<spval .and. fld(i,jump_max_loc(1)+1)<spval)then
           aa=abs(fld(i,jump_max_loc(1))-fld(i,jump_max_loc(1)+1))
           if(aa>wnyq*nyq)then
           write(99,200)i,fld(i,jump_max_loc(1)),fld(i,jump_max_loc(1)+1)
           end if
         end if
       end do

       write(99,*)'con_jump_max_loc:',val_con_jump,con_jump_max_loc(1)
       do i=1,ngate
         if(fld(i,con_jump_max_loc(1))<spval .and. fld(i,con_jump_max_loc(1)+1)<spval)then
           aa=abs(fld(i,con_jump_max_loc(1))-fld(i,con_jump_max_loc(1)+1))
           if(aa>wnyq*nyq)then
           write(99,200)i,fld(i,con_jump_max_loc(1)),fld(i,con_jump_max_loc(1)+1)
           end if
         end if
       end do
       close(99)
       end if

200  format(i5,2f9.2)
       End subroutine azimcontr
