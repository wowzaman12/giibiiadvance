/*
    GiiBiiAdvance - GBA/GB  emulator
    Copyright (C) 2011 Antonio Niño Díaz (AntonioND)

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

#include <string.h>
#include <stdio.h>

#include "../build_options.h"

#include "config.h"
#include "main.h"
#include "windows/gui_main.h"

t_config EmulatorConfig = { //Default options...
    0, //debug_msg_enable
    2, //screen_size
    0, //load_from_boot_rom
    -1, //frameskip
    0, //highperformancetimer
    0, //oglfilter
    0, //auto_close_debugger
    //---------
    100, //server_buffer_len
    64, //volume
    0x3F, //chn_flags
    0, //snd_mute
    //---------
    -1, //hardware_type
    0, //enableblur
    0, //realcolors

    };
/*
const char * GBKeyNames[P_NUM_KEYS] =
    { "A", "B", "Start", "Select", "Right", "Left", "Up", "Down" };
*/
#define CFG_DB_MSG_ENABLE "debug_msg_enable"
// "true" - "false"

#define CFG_SCREEN_SIZE "screen_size"
// unsigned integer ( "1" - "3" )

#define CFG_LOAD_BOOT_ROM "load_boot_rom"
// "true" - "false"

#define CFG_FRAMESKIP "frameskip"
// "-1" - "9"

#define CFG_HIGH_PERFORMANCE_TIMER "high_performance_timer"
// "true" - "false"

#define CFG_OPENGL_FILTER "opengl_filter"
char * oglfiltertype[] = { "nearest", "linear" };

#define CFG_AUTO_CLOSE_DEBUGGER "auto_close_debugger"
// "true" - "false"

#define CFG_SND_BUF_LEN "sound_buffer_lenght"
// unsigned integer ( "50" - "250" ) -- in steps of 50

#define CFG_SND_CHN_ENABLE "channels_enabled"
// "#3F" 3F = flags

#define CFG_SND_VOLUME "volume"
// "#VV" 00h - 80h

#define CFG_SND_MUTE "sound_mute"
// "true" - "false"

#define CFG_HW_TYPE "hardware_type"
char * hwtype[] = { "Auto", "GBA" };

#define CFG_ENABLE_BLUR "enable_blur"
// "true" - "false"

//---------------------------------------------------------------------
#ifdef WIN32

#include <windows.h>

boolean
get_reg_str (const char *sub, const char *name, char *out, DWORD len)
{
        HKEY hKey;
        DWORD t;

        if (RegOpenKeyEx (HKEY_CURRENT_USER, sub, 0, KEY_READ, &hKey) ==
                        ERROR_SUCCESS)
        {
                if (RegQueryValueEx (hKey, name, NULL, &t, out, &len) != ERROR_SUCCESS ||
                         t != REG_SZ)
                {
                        RegCloseKey (hKey);
                        return FALSE;
                }
                out[len-1] = 0;
                RegCloseKey (hKey);
                return TRUE;
        }

        return FALSE;
}
#endif

void Config_Save(void)
{
#ifdef WIN32
/*    char out[256];
    if get_reg_str ("Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\Shell Folders", "AppData", out, sizeof (out))
    {

    } */

#endif
    char path[MAXPATHLEN];
    if(GetRunningFolder()) sprintf(path,"%s/GiiBiiAdvance.ini",GetRunningFolder());
    else strcpy(path,"GiiBiiAdvance.ini");
    FILE * ini_file = fopen(path,"wb");
    if(ini_file == NULL) return;
    fprintf(ini_file,"[General]\r\n");
    fprintf(ini_file,CFG_DB_MSG_ENABLE "=%s\r\n",EmulatorConfig.debug_msg_enable?"true":"false");
    fprintf(ini_file,CFG_SCREEN_SIZE "=%d\r\n",EmulatorConfig.screen_size);
    fprintf(ini_file,CFG_LOAD_BOOT_ROM "=%s\r\n",EmulatorConfig.load_from_boot_rom?"true":"false");
    fprintf(ini_file,CFG_FRAMESKIP "=%d\r\n",EmulatorConfig.frameskip);
    fprintf(ini_file,CFG_HIGH_PERFORMANCE_TIMER "=%s\r\n",EmulatorConfig.highperformancetimer?"true":"false");
    fprintf(ini_file,CFG_OPENGL_FILTER "=%s\r\n",oglfiltertype[EmulatorConfig.oglfilter]);
    fprintf(ini_file,CFG_AUTO_CLOSE_DEBUGGER "=%s\r\n",EmulatorConfig.auto_close_debugger?"true":"false");
    fprintf(ini_file,"\r\n");

    fprintf(ini_file,"[Sound]\r\n");
    fprintf(ini_file,CFG_SND_BUF_LEN "=%03d\r\n",EmulatorConfig.server_buffer_len);
    int volume, chn_flags;
    GBA_SoundGetConfig(&volume,&chn_flags); //GB doesn't get all information
    fprintf(ini_file,CFG_SND_CHN_ENABLE "=#%02X\r\n",chn_flags);
    fprintf(ini_file,CFG_SND_VOLUME "=#%02X\r\n",volume);
    fprintf(ini_file,CFG_SND_MUTE "=%s\r\n",EmulatorConfig.snd_mute?"true":"false");
    fprintf(ini_file,"\r\n");

/*
    fprintf(ini_file,"[Controls]\r\n");
    int player, key;
    for(player = 0; player < 4; player ++) for(key = 0; key < P_NUM_KEYS; key ++)
    {
        char * keyname = SDL_GetKeyName(Config_Controls_Get_Key(player,key));
        if(strcmp(keyname,"unknown key") != 0)
            fprintf(ini_file,"P%d_%s=%s\r\n",player+1,GBKeyNames[key],keyname);
    }
    if(Config_Controls_Get_Key(0,P_KEY_SPEEDUP) != 0)
        fprintf(ini_file,"SpeedUp=%s\r\n",SDL_GetKeyName(Config_Controls_Get_Key(0,P_KEY_SPEEDUP)));
    else
        fprintf(ini_file,"SpeedUp=\r\n");
    fprintf(ini_file,"\r\n");
*/
    fclose(ini_file);
}

void Config_Load(void)
{

    char path[MAXPATHLEN];
    if(GetRunningFolder()) sprintf(path,"%s/GiiBiiAdvance.ini",GetRunningFolder());
    else strcpy(path,"GiiBiiAdvance.ini");

    char * ini;
    FileLoad_NoError(path,(void*)&ini,NULL);
    if(ini == NULL) return;

    char * tmp = strstr(ini,CFG_DB_MSG_ENABLE);
    if(tmp)
    {
        tmp += strlen(CFG_DB_MSG_ENABLE) + 1;
        if(strncmp(tmp,"true",strlen("true")) == 0)
            EmulatorConfig.debug_msg_enable = 1;
        else
            EmulatorConfig.debug_msg_enable = 0;
    }

    tmp = strstr(ini,CFG_SCREEN_SIZE);
    if(tmp)
    {
        tmp += strlen(CFG_SCREEN_SIZE) + 1;
        EmulatorConfig.screen_size = atoi(tmp);
        if(EmulatorConfig.screen_size > 3) EmulatorConfig.screen_size = 3;
        else if(EmulatorConfig.screen_size < 1) EmulatorConfig.screen_size = 1;
    }
    GLWindow_SetZoom(EmulatorConfig.screen_size);

    tmp = strstr(ini,CFG_LOAD_BOOT_ROM);
    if(tmp)
    {
        tmp += strlen(CFG_LOAD_BOOT_ROM) + 1;
        if(strncmp(tmp,"true",strlen("true")) == 0)
            EmulatorConfig.load_from_boot_rom = 1;
        else
            EmulatorConfig.load_from_boot_rom = 0;
    }

    EmulatorConfig.frameskip = 0;
    tmp = strstr(ini,CFG_FRAMESKIP);
    if(tmp)
    {
        tmp += strlen(CFG_FRAMESKIP) + 1;
        if(*tmp == '-') //*(tmp+1) == 1
             EmulatorConfig.frameskip = -1;
        else
            EmulatorConfig.frameskip = *tmp - '0';
    }

    EmulatorConfig.highperformancetimer = 0;
    tmp = strstr(ini,CFG_HIGH_PERFORMANCE_TIMER);
    if(tmp)
    {
        tmp += strlen(CFG_HIGH_PERFORMANCE_TIMER) + 1;
        if(strncmp(tmp,"true",strlen("true")) == 0)
            EmulatorConfig.highperformancetimer = 1;
        else
            EmulatorConfig.highperformancetimer = 0;
    }

    tmp = strstr(ini,CFG_OPENGL_FILTER);
    if(tmp)
    {
        tmp += strlen(CFG_OPENGL_FILTER) + 1;

        int i, result = 0;
        for(i = 0; i < ARRAY_NUM_ELEMENTS(oglfiltertype); i ++)
            if(strncmp(tmp,oglfiltertype[i],strlen(oglfiltertype[i])) == 0)
                result = i;

        EmulatorConfig.oglfilter = result;
    }

    tmp = strstr(ini,CFG_AUTO_CLOSE_DEBUGGER);
    if(tmp)
    {
        tmp += strlen(CFG_AUTO_CLOSE_DEBUGGER) + 1;
        if(strncmp(tmp,"true",strlen("true")) == 0)
            EmulatorConfig.auto_close_debugger = 1;
        else
            EmulatorConfig.auto_close_debugger = 0;
    }

    //SOUND
    tmp = strstr(ini,CFG_SND_BUF_LEN);
    if(tmp)
    {
        tmp += strlen(CFG_SND_BUF_LEN) + 1;

        char aux[4]; aux[3] = '\0';
        aux[0] = *tmp++;
        aux[1] = *tmp++;
        aux[2] = *tmp;

        EmulatorConfig.server_buffer_len = asciidectoint(aux);
        if(EmulatorConfig.server_buffer_len < 50) EmulatorConfig.server_buffer_len = 50;
        else if(EmulatorConfig.server_buffer_len > 250) EmulatorConfig.server_buffer_len = 250;
    }

    int vol = 64, chn_flags = 0x3F;
    tmp = strstr(ini,CFG_SND_CHN_ENABLE);
    if(tmp)
    {
        tmp += strlen(CFG_SND_CHN_ENABLE) + 1;
        if(*tmp == '#')
        {
            tmp ++;
            char aux[3]; aux[2] = '\0';
            aux[0] = *tmp; tmp++;
            aux[1] = *tmp;
            chn_flags = asciihextoint(aux);
        }
    }

    tmp = strstr(ini,CFG_SND_VOLUME);
    if(tmp)
    {
        tmp += strlen(CFG_SND_VOLUME) + 1;
        if(*tmp == '#')
        {
            tmp ++;
            char aux[3]; aux[2] = '\0';
            aux[0] = *tmp; tmp++;
            aux[1] = *tmp;
            vol = asciihextoint(aux);
        }
    }
    GBA_SoundSetConfig(vol,chn_flags);

    tmp = strstr(ini,CFG_SND_MUTE);
    if(tmp)
    {
        tmp += strlen(CFG_SND_MUTE) + 1;
        if(strncmp(tmp,"true",strlen("true")) == 0)
            EmulatorConfig.snd_mute = 1;
        else
            EmulatorConfig.snd_mute = 0;
    }

    }

    //free(ini);

