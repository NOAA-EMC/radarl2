!===========================================================================
!Purpose:
!  dump all 6 level-II varibles with unified code
!
!author:Shun Liu  
!time  :07/16/2012

!input: level2 variable strcture

!output:
!
!revision:


!===========================================================================
      subroutine dumpvar(k,ii,infile,nelv,strct_in_var,cvar)
         use sdata
         integer:: k,ii,nelv_var,na,nb
         type(ccout_strct),dimension(nelv) :: strct_in_var
         character(len=80) :: name1,name2,infile,cvar
             write(name1,101) ii
             name2=infile(1:17)//'_'//trim(cvar)//'_'//name1(1:8)
             write(*,*) name2
                open(31,file=name2,form='formatted')
                write(31,'(a4)') strct_in_var(k)%radar_name
                write(31,'(i8)') strct_in_var(k)%vcpnum
                write(31,'(6i8)') strct_in_var(k)%year                 &
                                 ,strct_in_var(k)%month                &
                                 ,strct_in_var(k)%day                  &
                                 ,strct_in_var(k)%hour                 &
                                 ,strct_in_var(k)%minute               &
                                 ,strct_in_var(k)%second
                write(31,'(2f10.3,f10.1)') strct_in_var(k)%radlat      &
                                          ,strct_in_var(k)%radlon      &
                                          ,strct_in_var(k)%radhgt      
                write(31,'(2f8.1)') strct_in_var(k)%fstgatdis          &
                                   ,strct_in_var(k)%gateWidth(100)
                write(31,'(f8.3)') strct_in_var(k)%elev_angle
                write(31,'(2i8)') strct_in_var(k)%num_beam             &
                                 ,strct_in_var(k)%num_gate
                na=strct_in_var(k)%num_beam
                nb=strct_in_var(k)%num_gate
                write(31,'(f8.3)') strct_in_var(k)%nyq_vel
                write(31,'(15f6.1)') (strct_in_var(k)%azim(j),j=1,na)
                do j=1,na
                write(31,'(20f6.1)') (strct_in_var(k)%field(i,j),i=1,nb)
                end do
                close(31)
101            format(I4.4,'.dat')
      end subroutine dumpvar
!===========================================================================
