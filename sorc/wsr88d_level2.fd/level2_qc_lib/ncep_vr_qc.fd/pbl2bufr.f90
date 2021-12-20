 subroutine pbl2_bufr_out(lunout, lundx, station_id, station_lat, station_lon, &
                          station_height, obs_yyyy, obs_mm, obs_dd, obs_hh, &
                          obs_mn, pbl_hgt, angl, num_records)
 implicit none

 integer, parameter :: MAX_MNEMONICS = 15
 integer, parameter :: MAX_LEVELS=255 !maximum records for a station
 character(len=80) :: output_bufr_file
 character(len=8) :: mnemonic_name='NC007021'
 character(len=8) :: station_id 
 character(len=200) :: command_line
 integer :: lunout, lundx
 integer :: num_records !number of data records for a single station

 real(8) :: station_lat, station_lon, station_height  
 integer :: obs_yyyy, obs_mm, obs_dd, obs_hh, obs_mn, idate, ios 
 real(8), dimension(MAX_LEVELS):: pbl_hgt, angl

 real(8), dimension(MAX_MNEMONICS, MAX_LEVELS)::real_arr
 real(8), dimension(MAX_LEVELS)::real_arr2 !1-D array for scalar variables 

 character(len=8) cval !8-character string to store the station id
 real(8) :: rval !store the station id in real format
 integer :: i, iret, nlvls !number of levels of the data reports array

 equivalence (cval, rval)

!open(500,file='testpbl') 
!write(500,*)station_id,station_lat,station_lon,station_height
!write(500,*)obs_yyyy,obs_mm,obs_dd,obs_hh,obs_mn
!write(500,*)pbl_hgt,angl
!close(500)


 call datelen(10) !Store a 10-digit idate in section 1 of 
                  !each output BUFR message

 !create the output bufr message file  NOT NEEDED?
 !open (unit=lunout, file=output_bufr_file, status="new", iostat=ios)

 !open the BUFR tables file
  open(lundx, file="bufrtab.007", status="old", iostat=ios)

 call openbf(lunout, "OUT", lundx)!open BUFR file in unit 51
                           !for output use external BUFR table in unit 31 to
                           !define structure of BUFR reports

 !Insert the station report to the BUFR message
 idate=(obs_yyyy*1000000 + obs_mm*10000 + &
        obs_dd*100 + obs_hh)

 !open a vad2 BUFR message to store new data report
 call openmb(lunout, mnemonic_name, idate)      

 !store the YYYYMMDD to array2
 real_arr2(1)=obs_yyyy
 real_arr2(2)=obs_mm
 real_arr2(3)=obs_dd

 !store the YYYYMMDD to the BUFR message
 call ufbseq(lunout, real_arr2, MAX_MNEMONICS, 1, iret, "YYMMDD")

 !store the HHMM to the array2
 real_arr2(1)=obs_hh
 real_arr2(2)=obs_mn
 
 !store the HHMM to the BUFR message
 call ufbseq(lunout, real_arr2, MAX_MNEMONICS, 1, iret, "HHMM")

 !store station_id, corn, station_lat, station_lon, height 
 !to array2 for mnemonics "NXRID"
 cval=station_id

 real_arr2(1)=rval
 real_arr2(2)=0
 real_arr2(3)=station_lat
 real_arr2(4)=station_lon
 real_arr2(5)=station_height

 !store the NXRID to the BUFR message
 call ufbseq(lunout, real_arr2, MAX_MNEMONICS, 1, iret, "NXRID")

 !store the height, uwind, vwind, qc_flag to the 2-D array
 do i=1, num_records
   real_arr(1,i)=angl(i)
   real_arr(2,i)=pbl_hgt(i)
 end do

 nlvls=num_records
! print *, "nlvls now is: ", nlvls
 
 !store the data array to the BUFR message
 call UFBINT(lunout, real_arr, MAX_MNEMONICS, nlvls, iret, 'RALA HPBL')

 call writsb(lunout)
 call closbf(lunout)

!command_line="mv fort.51 " // output_bufr_file
!call compact(command_line)
!print *, "command_line now is: ", command_line
!call system (command_line)

end subroutine pbl2_bufr_out
