c********************************************************************
      subroutine std_dev(npt,fld,smth_fld,nr,np,
     &           iminrg,nrang,spval,sd_fld,fld_prt)
c___________________________________________________
c
c    Author: Pengfei Zhang
c    Date:   May 3, 2002
c
c    Pengfei Zhang
c    Aug.4 2005 : Fix the a bug in the loop
c___________________________________________________

c___________________________________________________
c
c    Input: fld,smth_fld,nr,np,nbeam,iminrg, nrang,spval
c    output: sd_fld
c
c___________________________________________________

       integer npt
       real fld(nr,np)
       real smth_fld(nr,np)
       real fld_prt(nr,np)
       integer nr,np
       integer nbeam,iminrg,nrang
       integer i,j,js
       real spval
       real sd_fld(nr,np)
       integer l_half,n_cnt
       real tmp_sum

       l_half = int((npt-1)/2)

        do j =1, np
         do i = 1, nr
           fld_prt(i,j) = spval
           sd_fld(i,j) = spval
         end do
        end do

        do j =1, np
         do i = iminrg,nrang
          if (fld(i,j).ne.spval
     &        .and. smth_fld(i,j).ne.spval) then
           fld_prt(i,j) = fld(i,j) - smth_fld(i,j)
          else
           fld_prt(i,j) = spval
          end if
         end do
        end do

        do j =1, np
        do i =iminrg + l_half, nrang - l_half

          tmp_sum = 0.0
          n_cnt = 0

         do ll = i-l_half,i+l_half

          if ( fld(ll,j).ne.spval .and. fld_prt(ll,j).ne.spval) then
           n_cnt = n_cnt + 1
           tmp_sum = tmp_sum + fld_prt(ll,j)*fld_prt(ll,j)
          end if

         end do   ! ll loop

          if (n_cnt .gt. l_half) then
           sd_fld(i,j) = sqrt(tmp_sum/(n_cnt-1))
          else
           sd_fld(i,j) =spval 
          end if

        end do    ! i loop
        end do    ! j loop

!       write(6,*)'in std_avg::', tmp_sum, n_cnt

        return
        end
c******End of subroutine stnd_dev******************************************
