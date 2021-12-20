!===========================================================================

subroutine nssl_qcmain(nelv_ref, strct_in_ref, strct_in_vel)
use sdata

implicit none
integer :: nelv_ref
type(ccout_strct),dimension(nelv_ref) :: strct_in_ref,strct_in_vel 
integer i


!============Begin QC filter=========================================================

    do i=1,nelv_ref
    call RemvPattern(nazim,ngate,nelv_ref,        &
                    strct_in_ref(i),strct_in_vel(i)    &
                    )
     call SunBeamFltr(nazim,ngate,nelv_ref,        &
                     strct_in_ref(i),strct_in_vel(i)    &
                     )
    call NoiseFltr(nazim,ngate,nelv_ref,        &
                    strct_in_ref(i),strct_in_vel(i)    &
                    )
    enddo

!============End of QC filter=========================================================


 return
 end subroutine nssl_qcmain
