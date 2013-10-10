/*
    GiiBiiAdvance - GBA/GB  emulator
    Copyright (C) 2011 Antonio Ni�o D�az (AntonioND)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef __CONFIG_H__
#define __CONFIG_H__

typedef struct {
    int debug_msg_enable;
    int screen_size;
    int load_from_boot_rom; //gba always tries to load the rom, but this skips initial logo
    int frameskip;
    int highperformancetimer;
    int oglfilter;
    int auto_close_debugger;

    //Sound
    //-----
    int server_buffer_len;
    float volume;
    int chn_flags;
    int snd_mute;

    //GameBoy
    //-------
    int hardware_type;
    int serial_device;
    int enableblur;
    int realcolors;


    //gb palette is not saved here, it is saved in gb_main.c
    } t_config;

extern t_config EmulatorConfig;

void Config_Save(void);
void Config_Load(void);
/*
typedef enum {
    P_KEY_A,
    P_KEY_B,
    P_KEY_START,
    P_KEY_SELECT,
    P_KEY_RIGHT,
    P_KEY_LEFT,
    P_KEY_UP,
    P_KEY_DOWN,

    P_NUM_KEYS,

    P_KEY_SPEEDUP

    } _key_control_enum_;
*/
/*
extern const char * GBKeyNames[P_NUM_KEYS];

inline void Config_Controls_Set_Key(int player, _key_control_enum_ keyindex, SDLKey keysym);
inline SDLKey Config_Controls_Get_Key(int player, _key_control_enum_ keyindex);
*/
#endif //__CONFIG_H__
