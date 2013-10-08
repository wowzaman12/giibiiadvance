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

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "../build_options.h"

#include "gameboy.h"
#include "general.h"
#include "memory.h"
#include "video.h"
#include "debug.h"
#include "sgb.h"
#include "sound.h"
#include "video.h"

extern _GB_CONTEXT_ GameBoy;
extern _SGB_INFO_ SGBInfo;

//GAMEBOY framebuffer-related
u32 blur;
u32 realcolors;
u32 cur_fb;
u16 framebuffer[2][256 * 224];

u32 frames_skipped;
u32 frames_to_skip;

inline void GB_FrameskipUpdate(void)
{
    if(frames_to_skip)
    {
        frames_skipped++;
        if(frames_skipped >= frames_to_skip)
        {
            frames_skipped = 0;
        }
    }
}

inline void GB_Frameskip(int _frames_to_skip)
{
    if(frames_to_skip == _frames_to_skip) return;
    frames_skipped = 0;
	frames_to_skip = _frames_to_skip;
}

inline int GB_HaveToFrameskip(void)
{
     return (frames_skipped != 0);
}

inline void GB_EnableBlur(bool enable)
{
	blur = enable;
}

inline void GB_EnableRealColors(bool enable)
{
	realcolors = enable;
}

//-------------------------------------------------------------
//-------------------------------------------------------------
//                     GAMEBOY
//-------------------------------------------------------------
//-------------------------------------------------------------


u32 framebuffer_bgcolor0[256];
u32 framebuffer_bgpriority[256]; //FOR GBC

u32 window_current_line;

u32 frame;

u32 gbpalettes[4] = {GB_RGB(31,31,31),GB_RGB(21,21,21),GB_RGB(10,10,10),GB_RGB(0,0,0)};

void GB_SetPalette(u32 red, u32 green, u32 blue)
{
     gbpalettes[0] = GB_RGB(red >> 3, green >> 3, blue >> 3);
     gbpalettes[1] = GB_RGB( ((red*2)/3) >> 3, ((green*2)/3) >> 3, ((blue*2)/3) >> 3);
     gbpalettes[2] = GB_RGB( (red/3) >> 3, (green/3) >> 3, (blue/3) >> 3);
     gbpalettes[3] = GB_RGB(0,0,0);
}

inline u32 GB_GameBoyGetGray(u32 number)
{
	return gbpalettes[number & 3];
}

void GB_ScreenDrawScanline(s32 y)
{
	if(GB_HaveToFrameskip()) return;

	if(y > 143) return;

	_GB_MEMORY_ * mem = &GameBoy.Memory;

	if(y == 0)
	{
		window_current_line = 0;
		if((mem->IO_Ports[WX_REG-0xFF00]-7) > 159)
			if(mem->IO_Ports[WY_REG-0xFF00] > 143)
				window_current_line = -1; // Don't draw
		if( (mem->IO_Ports[LCDC_REG-0xFF00] & (1<<5)) == 0)
			window_current_line = -1; // Don't draw
	}

	u32 x;
	u32 base_index = (y<<8);
	u32 lcd_reg = mem->IO_Ports[LCDC_REG-0xFF00];

	if(GameBoy.Emulator.lcd_on) //SCREEN ENABLED
	{
	    if(GameBoy.Emulator.CPUHalt == 2)
	    {
	        for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(31,31,31);
	        if(y == 143) cur_fb ^= 1;
	        return;
	    }

		u32 bg_pal[4];
		u32 spr_pal0[4];
		u32 spr_pal1[4];

		//GAMEBOY palettes
		u32 bgp_reg = mem->IO_Ports[BGP_REG-0xFF00];
		bg_pal[0] = GB_GameBoyGetGray(bgp_reg & 0x3);
		bg_pal[1] = GB_GameBoyGetGray((bgp_reg>>2) & 0x3);
		bg_pal[2] = GB_GameBoyGetGray((bgp_reg>>4) & 0x3);
		bg_pal[3] = GB_GameBoyGetGray((bgp_reg>>6) & 0x3);

		u32 obp0_reg = mem->IO_Ports[OBP0_REG-0xFF00];
	//	spr_pal0[0] = GB_GameBoyGetGray(obp0_reg & 0x3);
		spr_pal0[1] = GB_GameBoyGetGray((obp0_reg>>2) & 0x3);
		spr_pal0[2] = GB_GameBoyGetGray((obp0_reg>>4) & 0x3);
		spr_pal0[3] = GB_GameBoyGetGray((obp0_reg>>6) & 0x3);

		u32 obp1_reg = mem->IO_Ports[OBP1_REG-0xFF00];
	//	spr_pal1[0] = GB_GameBoyGetGray(obp1_reg & 0x3);
		spr_pal1[1] = GB_GameBoyGetGray((obp1_reg>>2) & 0x3);
		spr_pal1[2] = GB_GameBoyGetGray((obp1_reg>>4) & 0x3);
		spr_pal1[3] = GB_GameBoyGetGray((obp1_reg>>6) & 0x3);

		//SCROLLS
		u32 scx_reg = mem->IO_Ports[SCX_REG-0xFF00];
		u32 scy_reg = mem->IO_Ports[SCY_REG-0xFF00];
		u32 wx_reg = mem->IO_Ports[WX_REG-0xFF00];
		u32 wy_reg = mem->IO_Ports[WY_REG-0xFF00];

		//OTHER
		u8 * tiledata = (lcd_reg & (1<<4)) ? &mem->VideoRAM[0x0000] : &mem->VideoRAM[0x0800];
		u8 * bgtilemap = (lcd_reg & (1<<3)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];
		u8 * wintilemap = (lcd_reg & (1<<6)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];

		bool increase_win = false;

		//DRAW BG + WIN
		for(x = 0; x < 160; x ++)
		{
			u32 color = GB_GameBoyGetGray(0);
			bool window_draw = 0;
			bool bg_color0 = false;

			if(window_current_line >= 0)
				if(lcd_reg & (1<<5)) //WIN
				{
					if(wy_reg <= y)
					if(  (wx_reg < 8)  || ( (wx_reg > 7) && ((wx_reg-7) <= x))  )
					{
						increase_win = true;

						u32 y_ = window_current_line;
						u32 x_ = x-wx_reg;
						u32 tile = wintilemap[( (y_>>3) * 32) + ((x_+7) >> 3)];

						if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
						{
							if(tile & (1<<7)) tile &= 0x7F;
							else tile += 128;
						}

						u8 * data = &tiledata[tile<<4];

						data += ((y_&7) * 2);

						x_ = 7-((x_+7)&7);

						color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

						bg_color0 = (color == 0);

						color = bg_pal[color];
						window_draw = true;
					}
				}

			if(lcd_reg & (1<<0) && (!window_draw)) //BG
			{
				u32 x_ = x+scx_reg;
				u32 y_ = y+scy_reg;

				u32 tile = bgtilemap[( ((y_>>3) & 31) * 32) + ((x_ >> 3) & 31)];

				if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
				{
					if(tile & (1<<7)) tile &= 0x7F;
					else tile += 128;
				}

				u8 * data = &tiledata[tile<<4];

				data += (y_&7) << 1;

				x_ = 7-(x_&7);

				color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

				bg_color0 = (color == 0);

				color = bg_pal[color];
			}

			framebuffer[cur_fb][base_index + x] = color;
			framebuffer_bgcolor0[x] = bg_color0;
		}

		if(increase_win) window_current_line ++;

		//Draw sprites... bg transparent color is bg_pal[0]
		if(lcd_reg & (1<<1)) //if sprites, let's see what sprites are drawn in this scanline
		{
			s32 spriteheight =  8 << ((lcd_reg & (1<<2)) != 0);
			_GB_OAM_ * GB_OAM = (void*)mem->ObjAttrMem;
			u32 tilemask = ((spriteheight == 16) ? 0xFE : 0xFF); //For 8x16 sprites, last bit is ignored
			_GB_OAM_ENTRY_ * GB_Sprite;

			u32 spritesdrawn = 0; //For the 10-sprites-per-scanline limit
			u32 drawsprite[40];

			int a;
			for(a = 0; a < 40; a++)
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				drawsprite[a] = 0;

				s32 real_y = (GB_Sprite->Y-16);

				if( ( real_y <= y ) && ( (real_y+spriteheight) > y ) )
				{
					if(spritesdrawn < 10)
					{
						drawsprite[a] = 1;
						spritesdrawn ++;
					}
				}
			}

			for(a = 39; a >= 0; a --) if(drawsprite[a]) //TODO: FIX?
				/*
				When sprites with different x coordinate values overlap, the one with the
				smaller x coordinate (closer to the left) will have priority and appear above
				any others. This applies in Non CGB Mode only.
				*/
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				s32 real_y = (GB_Sprite->Y-16);

				if( real_y <= y && (real_y+spriteheight) > y)
				{
					u32 tile = GB_Sprite->Tile & tilemask;

					u8 * data = &mem->VideoRAM[tile<<4];

					//flip Y
					if(GB_Sprite->Info & (1<<6)) data += (spriteheight-y+real_y-1)*2;
					else data += ( (y-real_y)*2 );

					//Lets draw the sprite...
					u32 x__;
					s32 real_x = (GB_Sprite->X-8);
					for(x__ = 0; x__ < 8; x__ ++)
					{
						u32 color = ( (*data >> x__) & 1 ) |  ( ( ( (*(data+1)) >> x__)  << 1) & 2);

						if(color != 0) //Color 0 is transparent
						{
							if(GB_Sprite->Info & (1<<4)) color = spr_pal1[color];
							else color = spr_pal0[color];

							s32 x_ = real_x;

							//flip X
							if(GB_Sprite->Info & (1<<5)) x_ = x_+ x__;
							else x_ += 7- x__;

							if(x_ >= 0 && x_ < 160)
							{
								//If Bg has priority and it is enabled...
								if( (GB_Sprite->Info & (1<<7)) && (lcd_reg & (1<<0)) )
								{
									if(framebuffer_bgcolor0[x_])
										framebuffer[cur_fb][base_index + x_] = color;
								}
								else framebuffer[cur_fb][base_index + x_] = color;
							}
						}
					}
				}
			}
		}
	}
	else
	{
		for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(31,31,31);
	}

	if(y == 143) cur_fb ^= 1;
}

//*********************************************************************************************

u32 gbc_getbgpalcolor(int pal, int color)
{
    u32 pal_ram_index = (pal * 8) + (2 * color);
    return GameBoy.Emulator.bg_pal[pal_ram_index] | (GameBoy.Emulator.bg_pal[pal_ram_index+1]<<8);
}

u32 gbc_getsprpalcolor(int pal, int color)
{
    u32 pal_ram_index = (pal * 8) + (2 * color);
    return GameBoy.Emulator.spr_pal[pal_ram_index] | (GameBoy.Emulator.spr_pal[pal_ram_index+1]<<8);
}

void GBC_ScreenDrawScanline(s32 y)
{
	if(GB_HaveToFrameskip()) return;

	if(y > 143) return;

	_GB_MEMORY_ * mem = &GameBoy.Memory;

	if(y == 0)
	{
		window_current_line = 0;
		if((mem->IO_Ports[WX_REG-0xFF00]-7) > 159)
			if(mem->IO_Ports[WY_REG-0xFF00] > 143)
				window_current_line = -1; // Don't draw
		if( (mem->IO_Ports[LCDC_REG-0xFF00] & (1<<5)) == 0)
			window_current_line = -1; // Don't draw
	}

	u32 x;
	u32 base_index = (y<<8);

	u32 lcd_reg = mem->IO_Ports[LCDC_REG-0xFF00];

	if(GameBoy.Emulator.lcd_on) //SCREEN ENABLED
	{
	    if(GameBoy.Emulator.CPUHalt == 2)
	    {
	        //Believe it or not, it behaves this way... or at least my tests say so.
	        //IR port doesn't seem to matter.
	        u32 color_ = GB_SoundHardwareIsOn() ? GB_RGB(31,31,31) : GB_RGB(0,0,0);
	        for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = color_;
	        if(y == 143) cur_fb ^= 1;
	        return;
	    }

		//SCROLLS
		u32 scx_reg = mem->IO_Ports[SCX_REG-0xFF00];
		u32 scy_reg = mem->IO_Ports[SCY_REG-0xFF00];
		u32 wx_reg = mem->IO_Ports[WX_REG-0xFF00];
		u32 wy_reg = mem->IO_Ports[WY_REG-0xFF00];

		//OTHER
		u8 * tiledata = (lcd_reg & (1<<4)) ? &mem->VideoRAM[0x0000] : &mem->VideoRAM[0x0800];
		u8 * bgtilemap = (lcd_reg & (1<<3)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];
		u8 * wintilemap = (lcd_reg & (1<<6)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];

		bool increase_win = false;

		//DRAW BG + WIN
		for(x = 0; x < 160; x ++)
		{
			u32 color = GB_RGB(31,31,31);
			bool window_draw = 0;
			bool color0 = false;
			u32 tileinfo;

			if(window_current_line >= 0)
				if(lcd_reg & (1<<5)) //WIN
				{
					if(wy_reg <= y)
					if(  (wx_reg < 8)  || ( (wx_reg > 7) && ((wx_reg-7) <= x))  )
					{
						increase_win = true;

						u32 y_ = window_current_line;
						u32 x_ = x-wx_reg;

						u32 tile_location = ( (y_>>3) * 32) + ((x_+7) >> 3);
						u32 tile = wintilemap[tile_location];
						tileinfo = wintilemap[tile_location + 0x2000];

						if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
						{
							if(tile & (1<<7)) tile &= 0x7F;
							else tile += 128;
						}

						u8 * data = &tiledata[(tile<<4) + ( (tileinfo&(1<<3)) ? 0x2000 : 0 )];  //Bank 1?

						//V FLIP
						if(tileinfo & (1<<6)) data += (((7-y_)&7) * 2);
						else data += ((y_&7) * 2);

						u32 pal_index = tileinfo&7;

						//H FLIP
						if(tileinfo & (1<<5)) x_ = ((x_+7)&7);
						else x_ = 7-((x_+7)&7);

						color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

						color0 = (color == 0);

						u32 pal_ram_index = (pal_index * 8) + (2*color);
						color = GameBoy.Emulator.bg_pal[pal_ram_index] | (GameBoy.Emulator.bg_pal[pal_ram_index+1]<<8);
						window_draw = true;
					}
				}

			if(!window_draw) //BG
			{
				u32 x_ = x+scx_reg;
				u32 y_ = y+scy_reg;

				u32 tile_location = ( ((y_>>3) & 31) * 32) + ((x_ >> 3) & 31);
				u32 tile = bgtilemap[tile_location];
				tileinfo = bgtilemap[tile_location + 0x2000];

				if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
				{
					if(tile & (1<<7)) tile &= 0x7F;
					else tile += 128;
				}

				u8 * data = &tiledata[(tile<<4) + ( (tileinfo&(1<<3)) ? 0x2000 : 0 )]; //Bank 1?

				//V FLIP
				if(tileinfo & (1<<6)) data += (((7-y_)&7) * 2);
				else data += ((y_&7) * 2);

				//H FLIP
				if(tileinfo & (1<<5)) x_ = (x_&7);
				else x_ = 7-(x_&7);

				color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);
				color0 = (color == 0);

				u32 pal_index = tileinfo&7;

				u32 pal_ram_index = (pal_index * 8) + (2*color);
				color = GameBoy.Emulator.bg_pal[pal_ram_index] | (GameBoy.Emulator.bg_pal[pal_ram_index+1]<<8);
			}


			framebuffer[cur_fb][base_index + x] = color;
			framebuffer_bgcolor0[x] = color0;
			framebuffer_bgpriority[x] = ( (tileinfo & (1<<7)) != 0 );
		}

		if(increase_win) window_current_line ++;

		//Draw sprites... bg transparent color is bg_pal[0]
		if(lcd_reg & (1<<1)) //if sprites, let's see what sprites are drawn in this scanline
		{
			s32 spriteheight =  8 << ((lcd_reg & (1<<2)) != 0);
			_GB_OAM_ * GB_OAM = (void*)mem->ObjAttrMem;
			u32 tilemask = ((spriteheight == 16) ? 0xFE : 0xFF); //For 8x16 sprites, last bit is ignored
			_GB_OAM_ENTRY_ * GB_Sprite;

			u32 spritesdrawn = 0; //For the 10-sprites-per-scanline limit
			u32 drawsprite[40];

			int a;
			for(a = 0; a < 40; a++)
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				drawsprite[a] = 0;

				s32 real_y = (GB_Sprite->Y-16);

				if( ( real_y <= y ) && ( (real_y+spriteheight) > y ) )
				{
					if(spritesdrawn < 10)
					{
						drawsprite[a] = 1;
						spritesdrawn ++;
					}
				}
			}

			for(a = 39; a >= 0; a --) if(drawsprite[a])
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				s32 real_y = (GB_Sprite->Y-16);

				if( real_y <= y && (real_y+spriteheight) > y)
				{
					u32 tile = GB_Sprite->Tile & tilemask;

					u8 * data = &mem->VideoRAM[tile<<4]; //Bank 0

					if(GB_Sprite->Info & (1<<3)) data += 0x2000; //Bank 1;

					//flip Y
					if(GB_Sprite->Info & (1<<6)) data += (spriteheight-y+real_y-1)*2;
					else data += ( (y-real_y)*2 );

					u32 pal_index = GB_Sprite->Info&7;

					pal_index = (pal_index * 8);

					//Lets draw the sprite...
					u32 x__;
					s32 real_x = (GB_Sprite->X-8);
					for(x__ = 0; x__ < 8; x__ ++)
					{
						u32 color = ( (*data >> x__) & 1 ) |  ( ( ( (*(data+1)) >> x__)  << 1) & 2);

						if(color != 0) //Color 0 is transparent
						{
							u32 pal_mem_pointer = pal_index + (2*color);
							color = GameBoy.Emulator.spr_pal[pal_mem_pointer] | (GameBoy.Emulator.spr_pal[pal_mem_pointer+1]<<8);

							s32 x_ = real_x;

							//flip X
							if(GB_Sprite->Info & (1<<5)) x_ = x_+ x__;
							else x_ += 7- x__;

							if(x_ >= 0 && x_ < 160)
							{
								//Priorities...
								if((lcd_reg & (1<<0)) == 0) //Master priority
								{
									framebuffer[cur_fb][base_index + x_] = color;
								}
								else if(framebuffer_bgpriority[x_]) //BG priority
								{
									if(framebuffer_bgcolor0[x_])
										framebuffer[cur_fb][base_index + x_] = color;
								}
								else //OAM priority
								{
									if(GB_Sprite->Info & (1<<7))
									{
										if(framebuffer_bgcolor0[x_])
											framebuffer[cur_fb][base_index + x_] = color;
									}
									else
										framebuffer[cur_fb][base_index + x_] = color;
								}
							}
						}
					}
				}
			}
		}
	}
	else
	{
		for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(31,31,31);
	}

	if(y == 143) cur_fb ^= 1;
}

void GBC_GB_ScreenDrawScanline(s32 y)
{
	if(GB_HaveToFrameskip()) return;

	if(y > 143) return;

	_GB_MEMORY_ * mem = &GameBoy.Memory;

	if(y == 0)
	{
		window_current_line = 0;
		if((mem->IO_Ports[WX_REG-0xFF00]-7) > 159)
			if(mem->IO_Ports[WY_REG-0xFF00] > 143)
				window_current_line = -1; // Don't draw
		if( (mem->IO_Ports[LCDC_REG-0xFF00] & (1<<5)) == 0)
			window_current_line = -1; // Don't draw
	}

	u32 x;
	u32 base_index = (y<<8);
	u32 lcd_reg = mem->IO_Ports[LCDC_REG-0xFF00];

	if(GameBoy.Emulator.lcd_on) //SCREEN ENABLED
	{
	    if(GameBoy.Emulator.CPUHalt == 2)
	    {
	        for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(0,0,0);
	        if(y == 143) cur_fb ^= 1;
	        return;
	    }

		u32 bg_pal[4];
		u32 spr_pal0[4];
		u32 spr_pal1[4];

		//palettes
		u32 bgp_reg = mem->IO_Ports[BGP_REG-0xFF00];
		bg_pal[0] = gbc_getbgpalcolor(0,bgp_reg & 0x3);
		bg_pal[1] = gbc_getbgpalcolor(0,(bgp_reg>>2) & 0x3);
		bg_pal[2] = gbc_getbgpalcolor(0,(bgp_reg>>4) & 0x3);
		bg_pal[3] = gbc_getbgpalcolor(0,(bgp_reg>>6) & 0x3);

		u32 obp0_reg = mem->IO_Ports[OBP0_REG-0xFF00];
	//	spr_pal0[0] = gbc_getsprpalcolor(0,obp0_reg & 0x3);
		spr_pal0[1] = gbc_getsprpalcolor(0,(obp0_reg>>2) & 0x3);
		spr_pal0[2] = gbc_getsprpalcolor(0,(obp0_reg>>4) & 0x3);
		spr_pal0[3] = gbc_getsprpalcolor(0,(obp0_reg>>6) & 0x3);

		u32 obp1_reg = mem->IO_Ports[OBP1_REG-0xFF00];
	//	spr_pal1[0] = gbc_getsprpalcolor(1,obp1_reg & 0x3);
		spr_pal1[1] = gbc_getsprpalcolor(1,(obp1_reg>>2) & 0x3);
		spr_pal1[2] = gbc_getsprpalcolor(1,(obp1_reg>>4) & 0x3);
		spr_pal1[3] = gbc_getsprpalcolor(1,(obp1_reg>>6) & 0x3);

		//SCROLLS
		u32 scx_reg = mem->IO_Ports[SCX_REG-0xFF00];
		u32 scy_reg = mem->IO_Ports[SCY_REG-0xFF00];
		u32 wx_reg = mem->IO_Ports[WX_REG-0xFF00];
		u32 wy_reg = mem->IO_Ports[WY_REG-0xFF00];

		//OTHER
		u8 * tiledata = (lcd_reg & (1<<4)) ? &mem->VideoRAM[0x0000] : &mem->VideoRAM[0x0800];
		u8 * bgtilemap = (lcd_reg & (1<<3)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];
		u8 * wintilemap = (lcd_reg & (1<<6)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];

		bool increase_win = false;

		//DRAW BG + WIN
		for(x = 0; x < 160; x ++)
		{
		    if(lcd_reg & (1<<0)) //This should only disable bg, but gbc in gb mode disables both win and bg
		    {
                u32 color = GB_RGB(31,31,31);
                bool window_draw = 0;
                bool bg_color0 = false;

                if(window_current_line >= 0)
                    if(lcd_reg & (1<<5)) //WIN
                    {
                        if(wy_reg <= y)
                        if(  (wx_reg < 8)  || ( (wx_reg > 7) && ((wx_reg-7) <= x))  )
                        {
                            increase_win = true;

                            u32 y_ = window_current_line;
                            u32 x_ = x-wx_reg;
                            u32 tile = wintilemap[( (y_>>3) * 32) + ((x_+7) >> 3)];

                            if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
                            {
                                if(tile & (1<<7)) tile &= 0x7F;
                                else tile += 128;
                            }

                            u8 * data = &tiledata[tile<<4];

                            data += ((y_&7) * 2);

                            x_ = 7-((x_+7)&7);

                            color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

                            bg_color0 = (color == 0);

                            color = bg_pal[color];
                            window_draw = true;
                        }
                    }

                if(!window_draw) //BG
                {
                    u32 x_ = x+scx_reg;
                    u32 y_ = y+scy_reg;

                    u32 tile = bgtilemap[( ((y_>>3) & 31) * 32) + ((x_ >> 3) & 31)];

                    if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
                    {
                        if(tile & (1<<7)) tile &= 0x7F;
                        else tile += 128;
                    }

                    u8 * data = &tiledata[tile<<4];

                    data += (y_&7) << 1;

                    x_ = 7-(x_&7);

                    color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

                    bg_color0 = (color == 0);

                    color = bg_pal[color];
                }

                framebuffer[cur_fb][base_index + x] = color;
                framebuffer_bgcolor0[x] = bg_color0;
            }
			else
			{
			    framebuffer[cur_fb][base_index + x] = GB_RGB(31,31,31);
                framebuffer_bgcolor0[x] = 1;
			}
		}

		if(increase_win) window_current_line ++;

		//Draw sprites... bg transparent color is bg_pal[0]
		if(lcd_reg & (1<<1)) //if sprites, let's see what sprites are drawn in this scanline
		{
			s32 spriteheight =  8 << ((lcd_reg & (1<<2)) != 0);
			_GB_OAM_ * GB_OAM = (void*)mem->ObjAttrMem;
			u32 tilemask = ((spriteheight == 16) ? 0xFE : 0xFF); //For 8x16 sprites, last bit is ignored
			_GB_OAM_ENTRY_ * GB_Sprite;

			u32 spritesdrawn = 0; //For the 10-sprites-per-scanline limit
			u32 drawsprite[40];

			int a;
			for(a = 0; a < 40; a++)
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				drawsprite[a] = 0;

				s32 real_y = (GB_Sprite->Y-16);

				if( ( real_y <= y ) && ( (real_y+spriteheight) > y ) )
				{
					if(spritesdrawn < 10)
					{
						drawsprite[a] = 1;
						spritesdrawn ++;
					}
				}
			}

			for(a = 39; a >= 0; a --) if(drawsprite[a]) //TODO: FIX?
				/*
				When sprites with different x coordinate values overlap, the one with the
				smaller x coordinate (closer to the left) will have priority and appear above
				any others. This applies in Non CGB Mode only.
				*/
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				s32 real_y = (GB_Sprite->Y-16);

				if( real_y <= y && (real_y+spriteheight) > y)
				{
					u32 tile = GB_Sprite->Tile & tilemask;

					u8 * data = &mem->VideoRAM[tile<<4];

					//flip Y
					if(GB_Sprite->Info & (1<<6)) data += (spriteheight-y+real_y-1)*2;
					else data += ( (y-real_y)*2 );

					//Lets draw the sprite...
					u32 x__;
					s32 real_x = (GB_Sprite->X-8);
					for(x__ = 0; x__ < 8; x__ ++)
					{
						u32 color = ( (*data >> x__) & 1 ) |  ( ( ( (*(data+1)) >> x__)  << 1) & 2);

						if(color != 0) //Color 0 is transparent
						{
							if(GB_Sprite->Info & (1<<4)) color = spr_pal1[color];
							else color = spr_pal0[color];

							s32 x_ = real_x;

							//flip X
							if(GB_Sprite->Info & (1<<5)) x_ = x_+ x__;
							else x_ += 7- x__;

							if(x_ >= 0 && x_ < 160)
							{
								//If Bg has priority and it is enabled...
								if( (GB_Sprite->Info & (1<<7)) && (lcd_reg & (1<<0)) )
								{
									if(framebuffer_bgcolor0[x_])
										framebuffer[cur_fb][base_index + x_] = color;
								}
								else framebuffer[cur_fb][base_index + x_] = color;
							}
						}
					}
				}
			}
		}
	}
	else
	{
		for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(31,31,31);
	}

	if(y == 143) cur_fb ^= 1;
}


//*********************************************************************************************

inline u32 SGB_GetPixelColor(u32 x, u32 y, u32 palindex)
{
	return SGBInfo.palette[SGBInfo.ATF_list[SGBInfo.curr_ATF][ (20*(y>>3)) + (x>>3)]][palindex];
}


void SGB_ScreenDrawBorder(void)
{
	u32 i,j;
	for(i = 0; i < 32; i++) for(j = 0; j < 28; j++)
	{
		//The inside will be drawn in other function
		if( ! ( (i > 5 && i < 26) && (j > 4 && j < 23) ) )
		{
			u32 info = SGBInfo.tile_map[(j*32) + i];

			u32 tile = info & 0xFF;

			u32 * tile_ptr = &SGBInfo.tile_data[((8*8*4)/8) * tile];

			u32 pal =  (info >> 10) & 7; // 4 to 7 (officially 4 to 6)
			if(pal < 4) pal += 4;

			//u32 prio = info & (1<<13); //not used

			u32 xflip = info & (1<<14);
			u32 yflip = info & (1<<15);

			u32 x,y;
			for(y = 0; y < 8; y++) for(x = 0; x < 8; x++)
			{
				u32 * data = tile_ptr;
				u32 * data2 = tile_ptr + 16;

				if(yflip)
				{
					data += (7-y)<<1;
					data2 += (7-y)<<1;
				}
				else
				{
					data += y<<1;
					data2 += y<<1;
				}

				u32 x_;
				if(xflip) x_ = x;
				else x_ = 7-x;

				u32 color = (*data >> x_) & 1;
				color |= ( ( ( (*(data+1)) >> x_) << 1) & (1<<1));
				color |= ( ( ( (*data2) >> x_) << 2) & (1<<2));
				color |= ( ( ( (*(data2+1)) >> x_) << 3) & (1<<3));
				color = SGBInfo.palette[pal][color];

				int temp = ((y+(j<<3))*256) + (x+(i<<3));
				framebuffer[0][temp] = color;
				framebuffer[1][temp] = color;
			}
		}
	}
}


void SGB_ScreenDrawBorderInside(void)
{
	u32 i,j;
	for(i = 6; i < 26; i++) for(j = 4; j < 23; j++)
	{
		u32 info = SGBInfo.tile_map[(j*32) + i];

		u32 * tile_ptr = &SGBInfo.tile_data[((8*8*4)/8) * (info & 0xFF)];

		u32 pal =  (info >> 10) & 7; // 4 to 7 (officially 4 to 6)
		if(pal < 4) pal += 4;

		//u32 prio = info & (1<<13); //not used

		u32 xflip = info & (1<<14);
		u32 yflip = info & (1<<15);

		u32 x,y;
		for(y = 0; y < 8; y++)
		{
			u32 * data = tile_ptr;
			u32 * data2 = tile_ptr + 16;

			if(yflip)
			{
				data += (7-y)<<1;
				data2 += (7-y)<<1;
			}
			else
			{
				data += y<<1;
				data2 += y<<1;
			}

			//if(*data != 0 && *data2 != 0)
			//{
				for(x = 0; x < 8; x++)
				{
					u32 x_;
					if(xflip) x_ = x;
					else x_ = 7-x;

					u32 color = (*data >> x_) & 1;
					color |= ( ( ( (*(data+1)) >> x_) << 1) & (1<<1));
					color |= ( ( ( (*data2) >> x_) << 2) & (1<<2));
					color |= ( ( ( (*(data2+1)) >> x_) << 3) & (1<<3));

					if(color != 0)
					{
						color = SGBInfo.palette[pal][color];

						int temp = ((y+(j<<3))*256) + (x+(i<<3));
						framebuffer[0][temp] = color;
						framebuffer[1][temp] = color;
					}
				}
			//}
		}
	}
}

void SGB_ScreenDrawScanline(s32 y)
{
	if(GB_HaveToFrameskip()) return;

	if(y > 143) return;

	_GB_MEMORY_ * mem = &GameBoy.Memory;

	if(y == 0)
	{
		window_current_line = 0;
		if((mem->IO_Ports[WX_REG-0xFF00]-7) > 159)
			if(mem->IO_Ports[WY_REG-0xFF00] > 143)
				window_current_line = -1; // Don't draw
		if( (mem->IO_Ports[LCDC_REG-0xFF00] & (1<<5)) == 0)
			window_current_line = -1; // Don't draw
	}

	u32 x;
	u32 base_index = ((40+y)<<8);

	if(SGBInfo.freeze_screen == SGB_SCREEN_FREEZE)
	{
        if(y == 143)
        {
            SGB_ScreenDrawBorderInside();
            cur_fb ^= 1;
        }
		return;
	}
	else if(SGBInfo.freeze_screen == SGB_SCREEN_BLACK)
	{
		for(x = 48; x < 160 + 48; x++) framebuffer[cur_fb][base_index + x] = GB_RGB(0,0,0);
		if(y == 143)
        {
            SGB_ScreenDrawBorderInside();
            cur_fb ^= 1;
        }
		return;
	}
	else if(SGBInfo.freeze_screen == SGB_SCREEN_BACKDROP)
	{
		u32 color_ = SGBInfo.palette[0][0];
		for(x = 48; x < 160 + 48; x++) framebuffer[cur_fb][base_index + x] = color_;
		if(y == 143)
        {
            SGB_ScreenDrawBorderInside();
            cur_fb ^= 1;
        }
		return;
	}

	u32 lcd_reg = mem->IO_Ports[LCDC_REG-0xFF00];

	if(GameBoy.Emulator.lcd_on) //SCREEN ENABLED
	{
	    if(GameBoy.Emulator.CPUHalt == 2)
	    {
            for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x + 48] = GB_RGB(31,31,31);
	        if(y == 143)
	        {
	            SGB_ScreenDrawBorderInside();
	            cur_fb ^= 1;
	        }
	        return;
	    }

		u32 bg_pal[4];
		u32 spr_pal0[4];
		u32 spr_pal1[4];

		//GAMEBOY palettes
		u32 bgp_reg = mem->IO_Ports[BGP_REG-0xFF00];
		bg_pal[0] = bgp_reg & 0x3;
		bg_pal[1] = (bgp_reg>>2) & 0x3;
		bg_pal[2] = (bgp_reg>>4) & 0x3;
		bg_pal[3] = (bgp_reg>>6) & 0x3;

		u32 obp0_reg = mem->IO_Ports[OBP0_REG-0xFF00];
//		spr_pal0[0] = obp0_reg & 0x3;
		spr_pal0[1] = (obp0_reg>>2) & 0x3;
		spr_pal0[2] = (obp0_reg>>4) & 0x3;
		spr_pal0[3] = (obp0_reg>>6) & 0x3;

		u32 obp1_reg = mem->IO_Ports[OBP1_REG-0xFF00];
//		spr_pal1[0] = obp1_reg & 0x3;
		spr_pal1[1] = (obp1_reg>>2) & 0x3;
		spr_pal1[2] = (obp1_reg>>4) & 0x3;
		spr_pal1[3] = (obp1_reg>>6) & 0x3;

		//SCROLLS
		u32 scx_reg = mem->IO_Ports[SCX_REG-0xFF00];
		u32 scy_reg = mem->IO_Ports[SCY_REG-0xFF00];
		u32 wx_reg = mem->IO_Ports[WX_REG-0xFF00];
		u32 wy_reg = mem->IO_Ports[WY_REG-0xFF00];

		//OTHER
		u8 * tiledata = (lcd_reg & (1<<4)) ? &mem->VideoRAM[0x0000] : &mem->VideoRAM[0x0800];
		u8 * bgtilemap = (lcd_reg & (1<<3)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];
		u8 * wintilemap = (lcd_reg & (1<<6)) ? &mem->VideoRAM[0x1C00] : &mem->VideoRAM[0x1800];

		bool increase_win = false;

		//DRAW BG + WIN
		for(x = 0; x < 160; x ++)
		{
			u32 color = SGB_GetPixelColor(x,y,0);
			bool window_draw = 0;
			bool bg_color0 = false;

			if(window_current_line >= 0)
				if(lcd_reg & (1<<5)) //WIN
				{
					if(wy_reg <= y)
					if(  (wx_reg < 8)  || ( (wx_reg > 7) && ((wx_reg-7) <= x))  )
					{
						increase_win = true;

						u32 y_ = window_current_line;
						u32 x_ = x-wx_reg;
						u32 tile = wintilemap[( (y_>>3) * 32) + ((x_+7) >> 3)];

						if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
						{
							if(tile & (1<<7)) tile &= 0x7F;
							else tile += 128;
						}

						u8 * data = &tiledata[tile<<4];

						data += ((y_&7) * 2);

						x_ = 7-((x_+7)&7);

						color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

						bg_color0 = (color == 0);

						color = SGB_GetPixelColor(x,y,bg_pal[color]);
						window_draw = true;
					}
				}

			if(lcd_reg & (1<<0) && (!window_draw)) //BG
			{
				u32 x_ = x+scx_reg;
				u32 y_ = y+scy_reg;

				u32 tile = bgtilemap[( ((y_>>3) & 31) * 32) + ((x_ >> 3) & 31)];

				if(!(lcd_reg & (1<<4))) //If tile base is 0x8800
				{
					if(tile & (1<<7)) tile &= 0x7F;
					else tile += 128;
				}

				u8 * data = &tiledata[tile<<4];

				data += (y_&7) << 1;

				x_ = 7-(x_&7);

				color = ( (*data >> x_) & 1 ) |  ( ( ( (*(data+1)) >> x_)  << 1) & 2);

				bg_color0 = (color == 0);

				color = SGB_GetPixelColor(x,y,bg_pal[color]);
			}

			framebuffer[cur_fb][base_index + x + 48] = color;
			framebuffer_bgcolor0[x] = bg_color0;
		}

		if(increase_win) window_current_line ++;

		//Draw sprites... bg transparent color is bg_pal[0]
		if(lcd_reg & (1<<1)) //if sprites, let's see what sprites are drawn in this scanline
		{
			s32 spriteheight =  8 << ((lcd_reg & (1<<2)) != 0);
			_GB_OAM_ * GB_OAM = (void*)mem->ObjAttrMem;
			u32 tilemask = ((spriteheight == 16) ? 0xFE : 0xFF); //For 8x16 sprites, last bit is ignored
			_GB_OAM_ENTRY_ * GB_Sprite;

			u32 spritesdrawn = 0; //For the 10-sprites-per-scanline limit
			u32 drawsprite[40];

			int a;
			for(a = 0; a < 40; a++)
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				drawsprite[a] = 0;

				s32 real_y = (GB_Sprite->Y-16);

				if( ( real_y <= y ) && ( (real_y+spriteheight) > y ) )
				{
					if(spritesdrawn < 10)
					{
						drawsprite[a] = 1;
						spritesdrawn ++;
					}
				}
			}

			for(a = 39; a >= 0; a --) if(drawsprite[a]) //TODO: FIX?
				/*
				When sprites with different x coordinate values overlap, the one with the
				smaller x coordinate (closer to the left) will have priority and appear above
				any others. This applies in Non CGB Mode only.
				*/
			{
				GB_Sprite = &GB_OAM->Sprite[a];

				s32 real_y = (GB_Sprite->Y-16);

				if( real_y <= y && (real_y+spriteheight) > y)
				{
					u32 tile = GB_Sprite->Tile & tilemask;

					u8 * data = &mem->VideoRAM[tile<<4];

					//flip Y
					if(GB_Sprite->Info & (1<<6)) data += (spriteheight-y+real_y-1)*2;
					else data += ( (y-real_y)*2 );

					//Lets draw the sprite...
					u32 x__;
					s32 real_x = (GB_Sprite->X-8);
					for(x__ = 0; x__ < 8; x__ ++)
					{
						u32 color = ( (*data >> x__) & 1 ) |  ( ( ( (*(data+1)) >> x__)  << 1) & 2);

						if(color != 0) //Color 0 is transparent
						{
							s32 x_ = real_x;

							//flip X
							if(GB_Sprite->Info & (1<<5)) x_ = x_+ x__;
							else x_ += 7- x__;

							if(x_ >= 0 && x_ < 160)
							{
								if(GB_Sprite->Info & (1<<4)) color = SGB_GetPixelColor(x_,y,spr_pal1[color]);
								else color = SGB_GetPixelColor(x_,y,spr_pal0[color]);

								//If Bg has priority and it is enabled...
								if( (GB_Sprite->Info & (1<<7)) && (lcd_reg & (1<<0)) )
								{
									if(framebuffer_bgcolor0[x_])
										framebuffer[cur_fb][base_index + x_ + 48] = color;
								}
								else framebuffer[cur_fb][base_index + x_ + 48] = color;
							}
						}
					}
				}
			}
		}
	}
	else
	{
		for(x = 0; x < 160; x++) framebuffer[cur_fb][base_index + x + 48] = GB_RGB(31,31,31);
	}

	if(y == 143)
	{
		SGB_ScreenDrawBorderInside();
		cur_fb ^= 1;
	}
}
