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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "../build_options.h"

#include "gameboy.h"
#include "rom.h"
#include "general.h"
#include "video.h"
#include "cpu.h"
#include "sound.h"
#include "sgb.h"

#include "../png/save_png.h"
#include "../frontend/windows/gui_main.h"
#include "../frontend/windows/main.h"
#include "gb_main.h"

extern _GB_CONTEXT_ GameBoy;

void GB_Screen_WritePixel(int x, int y, int r, int g, int b);
void GB_Screen_WriteBuffer(void);
int GB_Input_Get(int player);
void GB_Input_Update(void);

//---------------------------------

int GB_MainLoad(const char * rom_path)
{
    void * ptr; u32 size;
    FileLoad(rom_path,&ptr,&size);

    if(ptr)
    {
        if(Cartridge_Load(ptr,size))
        {
            //Init after loading the cartridge to set the hardware type value and allow
            //GB_Screen_Init choose the correct dimensions for the texture.

            Cardridge_Set_Filename((char*)rom_path);

            SRAM_Load();
            RTC_Load();
            GB_PowerOn();
            GB_Frameskip(0);
            return 1;
        }

        ErrorMessage("Error while loading cartridge.\n"
                    "Read the console output for details.");

        free(ptr);
        return 0;
    }

    ErrorMessage("Couldn't load data from %s.",rom_path);

    return 0;
}

void GB_End(int save)
{
    if(save)
    {
        SRAM_Save();
        RTC_Save();
    }
    GB_PowerOff();
    Cartridge_Unload();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

int scr_texture_loaded;

extern _GB_CONTEXT_ GameBoy;
extern u16 framebuffer[2][256 * 224];
extern u32 cur_fb;
extern u32 blur;
extern u32 realcolors;

int GB_Screen_Init(void)
{
    memset(framebuffer[0],0,sizeof(framebuffer[0]));
    memset(framebuffer[1],0,sizeof(framebuffer[1]));
    return 0;
}

extern void * SCREEN_TEXTURE;
inline void GB_Screen_WritePixel(int x, int y, int r, int g, int b)
{
    u8 * p = (u8*)SCREEN_TEXTURE + (y*160*4) + (x<<2);
    *p++ = r; *p++ = g; *p = b;
}

inline void GB_Screen_WritePixelSGB(int x, int y, int r, int g, int b)
{
    u8 * p = (u8*)SCREEN_TEXTURE + (y*(160+96)*4) + (x<<2);
    *p++ = r; *p++ = g; *p = b;
}

void gb_scr_writebuffer_sgb(void)
{
    int last_fb = cur_fb ^ 1;
    int i,j;
    for(i = 0; i < 256; i++) for(j = 0; j < 224; j++)
    {
        int data = framebuffer[last_fb][j*256 + i];
        int r = data & 0x1F; int g = (data>>5) & 0x1F; int b = (data>>10) & 0x1F;
        GB_Screen_WritePixelSGB(i,j, r<<3,g<<3,b<<3);
    }
}

void gb_scr_writebuffer_dmg_cgb(void)
{
    int last_fb = cur_fb ^ 1;
    int i,j;
    for(i = 0; i < 160; i++) for(j = 0; j < 144; j++)
    {
        int data = framebuffer[last_fb][j*256 + i];
        int r = data & 0x1F; int g = (data>>5) & 0x1F; int b = (data>>10) & 0x1F;
        GB_Screen_WritePixel(i,j, r<<3,g<<3,b<<3);
    }
}

void gb_scr_writebuffer_dmg_cgb_blur(void)
{
    int i,j;
    for(i = 0; i < 160; i++) for(j = 0; j < 144; j++)
    {
        int data1 = framebuffer[0][j*256 + i];
        int r1 = data1 & 0x1F; int g1 = (data1>>5) & 0x1F; int b1 = (data1>>10) & 0x1F;
        int data2 = framebuffer[1][j*256 + i];
        int r2 = data2 & 0x1F; int g2 = (data2>>5) & 0x1F; int b2 = (data2>>10) & 0x1F;
        int r = (r1+r2)<<2; int g = (g1+g2)<<2; int b = (b1+b2)<<2;
        GB_Screen_WritePixel(i,j, r,g,b);
    }
}

//Real colors:
// R = ((r * 13 + g * 2 + b) >> 1)
// G = ((g * 3 + b) << 1)
// B = ((r * 3 + g * 2 + b * 11) >> 1)

void gb_scr_writebuffer_dmg_cgb_realcolors(void)
{
    int last_fb = cur_fb ^ 1;
    int i,j;
    for(i = 0; i < 160; i++) for(j = 0; j < 144; j++)
    {
        int data = framebuffer[last_fb][j*256 + i];
        int r = data & 0x1F; int g = (data>>5) & 0x1F; int b = (data>>10) & 0x1F;
        int _r =  ((r * 13 + g * 2 + b) >> 1);
        int _g = (g * 3 + b) << 1;
        int _b = ((r * 3 + g * 2 + b * 11) >> 1);
        GB_Screen_WritePixel(i,j, _r,_g,_b);
    }
}

void gb_scr_writebuffer_dmg_cgb_blur_realcolors(void)
{
    int i,j;
    for(i = 0; i < 160; i++) for(j = 0; j < 144; j++)
    {
        for(i = 0; i < 160; i++) for(j = 0; j < 144; j++)
        {
            int data1 = framebuffer[0][j*256 + i];
            int r1 = data1 & 0x1F; int g1 = (data1>>5) & 0x1F; int b1 = (data1>>10) & 0x1F;
            int data2 = framebuffer[1][j*256 + i];
            int r2 = data2 & 0x1F; int g2 = (data2>>5) & 0x1F; int b2 = (data2>>10) & 0x1F;
            int r = (r1+r2)>>1; int g = (g1+g2)>>1; int b = (b1+b2)>>1;
            int _r =  ((r * 13 + g * 2 + b) >> 1);
            int _g = (g * 3 + b) << 1;
            int _b = ((r * 3 + g * 2 + b * 11) >> 1);
            GB_Screen_WritePixel(i,j, _r,_g,_b);
        }
    }
}

void GB_Screen_WriteBuffer(void)
{
    if( (GameBoy.Emulator.HardwareType == HW_SGB) || (GameBoy.Emulator.HardwareType == HW_SGB2) )
    {
        gb_scr_writebuffer_sgb();
    }
    else if(GameBoy.Emulator.HardwareType == HW_GBA)
    {
        if(blur) gb_scr_writebuffer_dmg_cgb_blur();
        else gb_scr_writebuffer_dmg_cgb();
    }
    else
    {
        if(blur)
        {
            if(realcolors) gb_scr_writebuffer_dmg_cgb_blur_realcolors();
            else gb_scr_writebuffer_dmg_cgb_blur();
        }
        else
        {
            if(realcolors) gb_scr_writebuffer_dmg_cgb_realcolors();
            else gb_scr_writebuffer_dmg_cgb();
        }
    }
}

int Keys[4];

int GB_Input_Get(int player)
{
    return Keys[player];
}

void GB_Input_Update_MBC7(int up, int down, int right, int left)
{
    if(up)
    {
        if(GameBoy.Emulator.MBC7.sensorY < 2047 - 50)
            GameBoy.Emulator.MBC7.sensorY += 100;
        else if(GameBoy.Emulator.MBC7.sensorY < 2047 + 100)
            GameBoy.Emulator.MBC7.sensorY += 2;
    }
    else if(down)
    {
        if(GameBoy.Emulator.MBC7.sensorY > 2047 + 50)
            GameBoy.Emulator.MBC7.sensorY -= 100;
        else if(GameBoy.Emulator.MBC7.sensorY > 2047 - 100)
            GameBoy.Emulator.MBC7.sensorY -= 2;
    }
    else
    {
        if(GameBoy.Emulator.MBC7.sensorY > 2047) GameBoy.Emulator.MBC7.sensorY -= 1;
        else GameBoy.Emulator.MBC7.sensorY += 1;
    }

    if(left)
    {
        if(GameBoy.Emulator.MBC7.sensorX < 2047 + 100)
            GameBoy.Emulator.MBC7.sensorX += 2;
    }
    else if(right)
    {
        if(GameBoy.Emulator.MBC7.sensorX > 2047 - 100)
            GameBoy.Emulator.MBC7.sensorX -= 2;
    }
    else
    {
        if(GameBoy.Emulator.MBC7.sensorX > 2047) GameBoy.Emulator.MBC7.sensorX -= 1;
        else GameBoy.Emulator.MBC7.sensorX += 1;
    }
}

void GB_Input_Update(void)
{
    Keys[0] = 0; Keys[1] = 0; Keys[2] = 0; Keys[3] = 0;

    if(Keys_Down[VK_LEFT]) Keys[0] |= KEY_LEFT;
    if(Keys_Down[VK_UP]) Keys[0] |= KEY_UP;
    if(Keys_Down[VK_RIGHT]) Keys[0] |= KEY_RIGHT;
    if(Keys_Down[VK_DOWN]) Keys[0] |= KEY_DOWN;
    if(Keys_Down['X']) Keys[0] |= KEY_A;
    if(Keys_Down['Z']) Keys[0] |= KEY_B;
    if(Keys_Down[VK_RETURN]) Keys[0] |= KEY_START;
    if(Keys_Down[VK_SHIFT]) Keys[0] |= KEY_SELECT;

/*
    if(SGB_MultiplayerIsEnabled())
    {
        int i;
        for(i = 1; i < 4; i++)
        {
            if(keystate[Config_Controls_Get_Key(i,P_KEY_LEFT)]) Keys[i] |= KEY_LEFT;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_UP)]) Keys[i] |= KEY_UP;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_RIGHT)]) Keys[i] |= KEY_RIGHT;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_DOWN)]) Keys[i] |= KEY_DOWN;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_A)]) Keys[i] |= KEY_A;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_B)]) Keys[i] |= KEY_B;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_START)]) Keys[i] |= KEY_START;
            if(keystate[Config_Controls_Get_Key(i,P_KEY_SELECT)]) Keys[i] |= KEY_SELECT;
        }
    }
*/
    if(GameBoy.Emulator.MemoryController == MEM_MBC7)
    {
        GB_Input_Update_MBC7(Keys_Down[VK_NUMPAD8],Keys_Down[VK_NUMPAD2],
                            Keys_Down[VK_NUMPAD6],Keys_Down[VK_NUMPAD4]);
    }
}

static int screenshot_file_number = 0;

void GB_Screenshot(void)
{
    char completepath[MAXPATHLEN];

    if(GetRunningFolder())
	{
		sprintf(completepath,"%s/%s/",GetRunningFolder(),SCREENSHOT_OUTPUT_FOLDER);
	}
	else
	{
		strcpy(completepath,"./" SCREENSHOT_OUTPUT_FOLDER);
	}

	char filename[MAXPATHLEN];

	while(1)
	{
		sprintf(filename,"%sgb_screenshot%d.png",completepath,screenshot_file_number);

		FILE* file=fopen(filename, "rb");
		if(file == NULL) break; //Ok
		screenshot_file_number ++; //look for next free number
	}

	int width, height;

	if(GameBoy.Emulator.SGBEnabled)
	{
	    width = 256; height = 224;
	}
	else
	{
	    width = 160; height = 144;
	}

    u32 * buf_temp = calloc(width*height*4,1);
    int last_fb = cur_fb ^ 1;
	int x, y;
	for(y = 0; y < height; y ++) for(x = 0; x < width; x ++)
	{
	    u32 data = framebuffer[last_fb][y*256 + x];
	    buf_temp[y*width + x] = ((data&0x1F)<<3)|((((data>>5)&0x1F)<<3)<<8)|
            ((((data>>10)&0x1F)<<3)<<16);
	}

    Save_PNG(filename,width,height,buf_temp);
    free(buf_temp);
}

u32 pal_red,pal_green,pal_blue;

void menu_get_gb_palette(u8 * red, u8 * green, u8 * blue)
{
    *red = pal_red;
    *green = pal_green;
    *blue = pal_blue;
}

void menu_set_gb_palette(u8 red, u8 green, u8 blue)
{
    pal_red = red;
    pal_green = green;
    pal_blue = blue;
}

void menu_load_gb_palete_from_config(void)
{
    GB_SetPalette(pal_red,pal_green,pal_blue);
}


