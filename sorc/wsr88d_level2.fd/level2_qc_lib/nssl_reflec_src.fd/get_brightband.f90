SUBROUTINE get_brightband(bb_info, pp)

       use BRIGHTBAND
       use CONFIG_PARS
       implicit none

       type (CCOUT_BRIGHTBAND)           bb_info
       type (CCOUT_CONFIG_PARS)          pp

       bb_info.bb_mode          =  0
       bb_info.volume_bb_exist  =  0
       bb_info.global_bb_top    =  pp.missing_bb
       bb_info.global_bb_bottom =  pp.missing_bb  

       RETURN        

END SUBROUTINE get_brightband
