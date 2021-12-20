// ##########################################################################
//      get_t_z_snd.cc: Function to read in different temperature level height 
// ##########################################################################
//
//      Author: Jian Zhang (CIMMS/NSSL)
//      may 10, 2001
//
//      Modification History:
// ##########################################################################

#include <string>
#include <fstream>
#include <cstdio>

#include "phycst.h"
#include "mosaic_adapt_pars.h"

#include "qpesums_utility.h"
#include "SOUNDING.h"

using namespace std;

int get_t_z_snd(string radar_name_snd, string config_file_name, 
                int n_tlevels, float *z_snd,short debug_flag)
{
    SOUNDING sound_info(radar_name_snd, config_file_name);

    z_snd[0] = sound_info.get_twenty_height();
    z_snd[1] = sound_info.get_ten_height();
    z_snd[2] = sound_info.get_zero_height();
    z_snd[3] = sound_info.get_minus_ten_height();
    z_snd[4] = sound_info.get_minus_twenty_height();
    z_snd[5] = sound_info.get_minus_thirty_height();
    z_snd[6] = sound_info.get_minus_fourty_height();
    z_snd[7] = sound_info.get_minus_fifty_height();
    z_snd[8] = sound_info.get_minus_sixty_height();

    if(debug_flag)
    {
      for( int kt=0; kt<n_tlevels; kt++)
        cout<<"sounding:: "<<t_snd_str[kt]<<"  "<<z_snd[kt]<<endl;
    }

    return 1;
}
