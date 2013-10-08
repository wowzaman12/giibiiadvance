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

#include "../build_options.h"

#include "gba.h"
#include "video.h"
#include "memory.h"

extern _mem_t Mem;
int curr_screen_buffer = 0;
u16 screen_buffer_array[2][240*160]; //doble buffer
u16 * screen_buffer = screen_buffer_array[0];

typedef void (*draw_scanline_fn)(s32);
draw_scanline_fn DrawScanlineFn;

void GBA_DrawScanlineMode0(s32 y);
void GBA_DrawScanlineMode1(s32 y);
void GBA_DrawScanlineMode2(s32 y);
void GBA_DrawScanlineMode3(s32 y);
void GBA_DrawScanlineMode4(s32 y);
void GBA_DrawScanlineMode5(s32 y);
inline void GBA_DrawScanlineWhite(s32 y);

static s32 BG2lastx,BG2lasty; //for affine transformation
static s32 BG3lastx,BG3lasty;

static s32 MosSprX, MosSprY, MosBgX, MosBgY;
static u32 Win0X1, Win0X2, Win0Y1, Win0Y2;
static u32 Win1X1, Win1X2, Win1Y1, Win1Y2;

//-----------------------------------------------------------

static inline s32 max(s32 a, s32 b) { return ((a<b)?b:a); }

static inline void mem_clear_32(u32 * ptr, u32 size)
{
    size >>= 2;
    while(size--) *ptr++ = 0;
}

//-----------------------------------------------------------

int frameskip = 0;
int frameskipcounter = 0;
inline void GBA_SetFrameskip(int value)
{
    if(frameskip == value) return;
    frameskip = value;
    frameskipcounter = 0;
}

inline void GBA_UpdateFrameskip(void)
{
    if(frameskip)
    {
        frameskipcounter++;
        if(frameskipcounter >= frameskip) frameskipcounter = 0;
    }
}

inline int GBA_HasToSkipFrame(void)
{
    return frameskipcounter;
}

//-----------------------------------------------------------

void GBA_UpdateDrawScanlineFn(void)
{
    u32 mode = REG_DISPCNT & 0x7;
    switch(mode)
    {
        case 0: DrawScanlineFn = &GBA_DrawScanlineMode0; break;
        case 1: DrawScanlineFn = &GBA_DrawScanlineMode1; break;
        case 2: DrawScanlineFn = &GBA_DrawScanlineMode2; break;
        case 3: DrawScanlineFn = &GBA_DrawScanlineMode3; break;
        case 4: DrawScanlineFn = &GBA_DrawScanlineMode4; break;
        case 5: DrawScanlineFn = &GBA_DrawScanlineMode5; break;
        case 6:
        case 7:
        default: DrawScanlineFn = &GBA_DrawScanlineMode3; break;
    }
}

inline void GBA_DrawScanline(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    if(y == 0)
    {
        curr_screen_buffer ^= 1;
        screen_buffer = screen_buffer_array[curr_screen_buffer];

        BG2lastx = REG_BG2X; if(BG2lastx&BIT(27)) BG2lastx |= 0xF0000000;
        BG2lasty = REG_BG2Y; if(BG2lasty&BIT(27)) BG2lasty |= 0xF0000000;

        BG3lastx = REG_BG3X; if(BG3lastx&BIT(27)) BG3lastx |= 0xF0000000;
        BG3lasty = REG_BG3Y; if(BG3lasty&BIT(27)) BG3lasty |= 0xF0000000;
    }

    DrawScanlineFn(y);

    BG2lastx += (s32)(s16)REG_BG2PB;
    BG2lasty += (s32)(s16)REG_BG2PD;

    BG3lastx += (s32)(s16)REG_BG3PB;
    BG3lasty += (s32)(s16)REG_BG3PD;
}

inline void GBA_DrawScanlineWhite(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    if(y == 0)
    {
        curr_screen_buffer ^= 1;
        screen_buffer = screen_buffer_array[curr_screen_buffer];
    }
    u32 * destptr = (u32*)&screen_buffer[240*y];
    int i;
    for(i = 0; i < 240/2; i++) *destptr++ = 0x7FFF7FFF;
}

//-----------------------------------------------------------------------------------
u16 sprfb[4][240]; int sprvisible[4][240];  int sprwin[240];
int sprblend[4][240]; int sprblendfb[4][240];

static const int spr_size[4][4][2] = { //shape, size, (x,y)
    {{8,8},{16,16},{32,32},{64,64}}, //Square
    {{16,8},{32,8},{32,16},{64,32}}, //Horizontal
    {{8,16},{8,32},{16,32},{32,64}}, //Vertical
    {{0,0},{0,0},{0,0},{0,0}} //Prohibited
};

static inline void gba_sprites_draw_mode012(s32 ly)
{
    _oam_spr_entry_t * spr = (_oam_spr_entry_t*)Mem.oam;

    int i;
    for(i = 0; i < 128; i++)
    {
        u16 attr0 = spr->attr0;

        if(attr0 & BIT(8)) //affine sprite -- no hflip/vflip
        {
            int mosaic = attr0 & BIT(12);

            u16 attr1 = spr->attr1;
            u16 attr2 = spr->attr2;

            _oam_matrix_entry_t * mat = &(((_oam_matrix_entry_t*)Mem.oam)[(attr1>>9) & 0x1F]);

            u16 shape = attr0>>14;
            u16 size = attr1>>14;
            int hsx = spr_size[shape][size][0]>>1; //half size
            int hsy = spr_size[shape][size][1]>>1;

            int y = (attr0&0xFF); y |= (y < 160) ? 0 : 0xFFFFFF00;
            int x = (int)(attr1&0x1FF) | ((attr1&BIT(8))?0xFFFFFE00:0);

            //is double size?
            int hrealsx = (attr0 & BIT(9)) ? hsx<<1 : hsx; //half canvas size
            int hrealsy = (attr0 & BIT(9)) ? hsy<<1 : hsy;

            int cx = x+hrealsx; //center of the sprite
            int cy = y+hrealsy;

            if((y <= ly) && ((y+(hrealsy<<1)) > ly)) //in this line
            {
                int mode = (attr0>>10)&3;

                u16 prio = (attr2 >> 10) & 3;
                u16 tilebaseno = attr2&0x3FF;
                int ydiff = ly-cy;
                if(mosaic) ydiff = ydiff - ydiff%MosSprY;

                if(attr0&BIT(13)) //256 colors
                {
                    tilebaseno >>= 1; //in 256 mode, they need double space

                    u16 * palptr = (u16*)&(Mem.pal_ram[256*2]);

                    int j = (x < 0) ? 0 : x; //search start point
                    while(j<(x+(hrealsx<<1)) && (j < 240))
                    {
                        if((sprvisible[prio][j] == 0) || (mode == 2))
                        {
                            int xdiff = j-cx;
                            if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                            // get texture coordinates (relative to center)
                            u32 px = (mat->pa*xdiff + mat->pb*ydiff)>>8;
                            u32 py = (mat->pc*xdiff + mat->pd*ydiff)>>8;
                            //get texture coordinates (absolute)
                            px += hsx;
                            py += hsy;

                            //u32, when hold a negative number, are a really high positive number
                            if((px < hsx<<1) && (py < hsy<<1))
                            {
                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*(hsx*2)/8);
                                }
                                else //2D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*16);
                                }

                                u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+((tilebaseno+tileadd)*64)]);

                                int _x = px & 7;
                                int _y = py & 7;

                                u8 data = tile_ptr[_x+(_y*8)];

                                if(data)
                                {
                                    if(mode == 0)
                                    {
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 1) //transp
                                    {
                                        sprblend[prio][j] = 1;
                                        sprblendfb[prio][j] = palptr[data];
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 2) //3 = prohibited
                                    {
                                        sprwin[j] = 1;
                                    }
                                }
                            }
                        }
                        j++;
                    }
                }
                else //16 colors
                {
                    u16 palno = attr2>>12;
                    u16 * palptr = (u16*)&Mem.pal_ram[512+(palno*32)];

                    int j = (x < 0) ? 0 : x; //search start point
                    while(j<(x+(hrealsx<<1)) && (j < 240))
                    {
                        if((sprvisible[prio][j] == 0) || (mode == 2))
                        {
                            int xdiff = j-cx;
                            if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                            // get texture coordinates (relative to center)
                            u32 px = (mat->pa*xdiff + mat->pb*ydiff)>>8;
                            u32 py = (mat->pc*xdiff + mat->pd*ydiff)>>8;
                            //get texture coordinates (absolute)
                            px += hsx;
                            py += hsy;

                            //u32, when hold a negative number, are a really high positive number
                            if((px < hsx<<1) && (py < hsy<<1))
                            {
                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*(hsx*2)/8);
                                }
                                else //2D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*32);
                                }

                                u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+((tilebaseno+tileadd)*32)]);

                                int _x = px & 7;
                                int _y = py & 7;

                                u8 data = tile_ptr[(_x/2)+(_y*4)];

                                if(_x&1) data = data>>4;
                                else data = data & 0xF;

                                if(data)
                                {
                                    if(mode == 0)
                                    {
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 1) //transp
                                    {
                                        sprblend[prio][j] = 1;
                                        sprblendfb[prio][j] = palptr[data];
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 2) //3 = prohibited
                                    {
                                        sprwin[j] = 1;
                                    }
                                }
                            }
                        }
                        j++;
                    }
                }
            }
        }
        else //regular sprite
        {
            if((attr0 & BIT(9)) == 0) //displayed
            {
                int mosaic = attr0 & BIT(12);

                u16 attr1 = spr->attr1;
                u16 attr2 = spr->attr2;

                u16 shape = attr0>>14;
                u16 size = attr1>>14;
                int sx = spr_size[shape][size][0];
                int sy = spr_size[shape][size][1];

                int y = (attr0&0xFF); y |= (y < 160) ? 0 : 0xFFFFFF00;
                int x = (int)(attr1&0x1FF) | ((attr1&BIT(8))?0xFFFFFE00:0);

                if((y <= ly) && ((y+sy) > ly)) //in this line
                {
                    int mode = (attr0>>10)&3;

                    int ydiff = ly-y;

                    if(attr1 & BIT(13)) ydiff = sy-ydiff-1; //vflip

                    if(mosaic) ydiff = ydiff - ydiff%MosSprY;

                    u16 prio = (attr2 >> 10) & 3;
                    u16 tilebaseno = attr2&0x3FF;

                    if(attr0&BIT(13)) //256 colors
                    {
                        tilebaseno >>= 1; //in 256 mode, they need double space

                        u16 * palptr = (u16*)&(Mem.pal_ram[256*2]);

                        int j = (x < 0) ? 0 : x; //search start point
                        while(j<(x+sx) && (j < 240))
                        {
                            if((sprvisible[prio][j] == 0) || (mode == 2))
                            {
                                int xdiff = (j-x);

                                if(attr1 & BIT(12)) xdiff = sx-xdiff-1; //hflip

                                if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*sx/8);
                                }
                                else //2D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*16);
                                }

                                u32 tileindex = tilebaseno+tileadd;
                                u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+(tileindex*64)]);

                                int _x = xdiff & 7;
                                int _y = ydiff & 7;

                                u8 data = tile_ptr[_x+(_y*8)];

                                if(data)
                                {
                                    if(mode == 0)
                                    {
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 1) //transp
                                    {
                                        sprblend[prio][j] = 1;
                                        sprblendfb[prio][j] = palptr[data];
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 2) //3 = prohibited
                                    {
                                        sprwin[j] = 1;
                                    }
                                }
                            }
                            j++;
                        }
                    }
                    else //16 colors
                    {
                        u16 palno = attr2>>12;
                        u16 * palptr = (u16*)&Mem.pal_ram[512+(palno*32)];

                        int j = (x < 0) ? 0 : x; //search start point
                        while(j<(x+sx) && (j < 240))
                        {
                            if((sprvisible[prio][j] == 0) || (mode == 2))
                            {
                                int xdiff = (j-x);

                                if(attr1 & BIT(12)) xdiff = sx-xdiff-1; //hflip

                                if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*sx/8);
                                }
                                else //2D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*32);
                                }

                                u32 tileindex = tilebaseno+tileadd;
                                u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+(tileindex*32)]);

                                int _x = xdiff & 7;
                                int _y = ydiff & 7;

                                u8 data = tile_ptr[(_x/2)+(_y*4)];

                                if(_x&1) data = data>>4;
                                else data = data & 0xF;

                                if(data)
                                {
                                    if(mode == 0)
                                    {
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 1) //transp
                                    {
                                        sprblend[prio][j] = 1;
                                        sprblendfb[prio][j] = palptr[data];
                                        sprfb[prio][j] = palptr[data];
                                        sprvisible[prio][j] = 1;
                                    }
                                    else if(mode == 2) //3 = prohibited
                                    {
                                        sprwin[j] = 1;
                                    }
                                }
                            }
                            j++;
                        }
                    }
                }
            }
        }
        spr++;
    }
}

static inline void gba_sprites_draw_mode345(s32 ly)
{
    _oam_spr_entry_t * spr = (_oam_spr_entry_t*)Mem.oam;

    int i;
    for(i = 0; i < 128; i++)
    {
        u16 attr0 = spr->attr0;

        if(attr0 & BIT(8)) //affine sprite -- no hflip/vflip
        {
            int mosaic = attr0 & BIT(12);

            u16 attr1 = spr->attr1;
            u16 attr2 = spr->attr2;

            _oam_matrix_entry_t * mat = &(((_oam_matrix_entry_t*)Mem.oam)[(attr1>>9) & 0x1F]);

            u16 shape = attr0>>14;
            u16 size = attr1>>14;
            int hsx = spr_size[shape][size][0]>>1; //half size
            int hsy = spr_size[shape][size][1]>>1;

            int y = (attr0&0xFF); y |= (y < 160) ? 0 : 0xFFFFFF00;
            int x = (int)(attr1&0x1FF) | ((attr1&BIT(8))?0xFFFFFE00:0);

            //is double size?
            int hrealsx = (attr0 & BIT(9)) ? hsx<<1 : hsx; //half canvas size
            int hrealsy = (attr0 & BIT(9)) ? hsy<<1 : hsy;

            int cx = x+hrealsx; //center of the sprite
            int cy = y+hrealsy;

            if((y <= ly) && ((y+(hrealsy<<1)) > ly)) //in this line
            {
                int mode = (attr0>>10)&3;

                u16 prio = (attr2 >> 10) & 3;
                u16 tilebaseno = attr2&0x3FF;
                int ydiff = ly-cy;
                if(mosaic) ydiff = ydiff - ydiff%MosSprY;

                if(attr0&BIT(13)) //256 colors
                {
                    tilebaseno >>= 1; //in 256 mode, they need double space

                    u16 * palptr = (u16*)&(Mem.pal_ram[256*2]);

                    int j = (x < 0) ? 0 : x; //search start point
                    while(j<(x+(hrealsx<<1)) && (j < 240))
                    {
                        if((sprvisible[prio][j] == 0) || (mode == 2))
                        {
                            int xdiff = j-cx;
                            if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                            // get texture coordinates (relative to center)
                            u32 px = (mat->pa*xdiff + mat->pb*ydiff)>>8;
                            u32 py = (mat->pc*xdiff + mat->pd*ydiff)>>8;
                            //get texture coordinates (absolute)
                            px += hsx;
                            py += hsy;

                            //u32, when hold a negative number, are a really high positive number
                            if((px < hsx<<1) && (py < hsy<<1))
                            {
                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*(hsx*2)/8);
                                }
                                else //2D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*16);
                                }

                                if(tilebaseno+tileadd >= 256) //each tile needs double space
                                {
                                    u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+((tilebaseno+tileadd)*64)]);

                                    int _x = px & 7;
                                    int _y = py & 7;

                                    u8 data = tile_ptr[_x+(_y*8)];

                                    if(data)
                                    {
                                        if(mode == 0)
                                        {
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 1) //transp
                                        {
                                            sprblend[prio][j] = 1;
                                            sprblendfb[prio][j] = palptr[data];
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 2) //3 = prohibited
                                        {
                                            sprwin[j] = 1;
                                        }
                                    }
                                }
                            }
                        }
                        j++;
                    }
                }
                else //16 colors
                {
                    u16 palno = attr2>>12;
                    u16 * palptr = (u16*)&Mem.pal_ram[512+(palno*32)];

                    int j = (x < 0) ? 0 : x; //search start point
                    while(j<(x+(hrealsx<<1)) && (j < 240))
                    {
                        if((sprvisible[prio][j] == 0) || (mode == 2))
                        {
                            int xdiff = j-cx;
                            if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                            // get texture coordinates (relative to center)
                            u32 px = (mat->pa*xdiff + mat->pb*ydiff)>>8;
                            u32 py = (mat->pc*xdiff + mat->pd*ydiff)>>8;
                            //get texture coordinates (absolute)
                            px += hsx;
                            py += hsy;

                            //u32, when hold a negative number, are a really high positive number
                            if((px < hsx<<1) && (py < hsy<<1))
                            {
                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*(hsx*2)/8);
                                }
                                else //2D
                                {
                                    int tilex = px>>3;
                                    int tiley = py>>3;
                                    tileadd = tilex + (tiley*32);
                                }

                                if(tilebaseno+tileadd >= 512)
                                {
                                    u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+((tilebaseno+tileadd)*32)]);

                                    int _x = px & 7;
                                    int _y = py & 7;

                                    u8 data = tile_ptr[(_x/2)+(_y*4)];

                                    if(_x&1) data = data>>4;
                                    else data = data & 0xF;

                                    if(data)
                                    {
                                        if(mode == 0)
                                        {
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 1) //transp
                                        {
                                            sprblend[prio][j] = 1;
                                            sprblendfb[prio][j] = palptr[data];
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 2) //3 = prohibited
                                        {
                                            sprwin[j] = 1;
                                        }
                                    }
                                }
                            }
                        }
                        j++;
                    }
                }
            }
        }
        else //regular sprite
        {
            if((attr0 & BIT(9)) == 0) //displayed
            {
                int mosaic = attr0 & BIT(12);

                u16 attr1 = spr->attr1;
                u16 attr2 = spr->attr2;

                u16 shape = attr0>>14;
                u16 size = attr1>>14;
                int sx = spr_size[shape][size][0];
                int sy = spr_size[shape][size][1];

                int y = (attr0&0xFF); y |= (y < 160) ? 0 : 0xFFFFFF00;
                int x = (int)(attr1&0x1FF) | ((attr1&BIT(8))?0xFFFFFE00:0);

                if((y <= ly) && ((y+sy) > ly)) //in this line
                {
                    int mode = (attr0>>10)&3;

                    int ydiff = ly-y;

                    if(attr1 & BIT(13)) ydiff = sy-ydiff-1; //vflip

                    if(mosaic) ydiff = ydiff - ydiff%MosSprY;

                    u16 prio = (attr2 >> 10) & 3;
                    u16 tilebaseno = attr2&0x3FF;

                    if(attr0&BIT(13)) //256 colors
                    {
                        tilebaseno >>= 1; //in 256 mode, they need double space

                        u16 * palptr = (u16*)&(Mem.pal_ram[256*2]);

                        int j = (x < 0) ? 0 : x; //search start point
                        while(j<(x+sx) && (j < 240))
                        {
                            if((sprvisible[prio][j] == 0) || (mode == 2))
                            {
                                int xdiff = (j-x);

                                if(attr1 & BIT(12)) xdiff = sx-xdiff-1; //hflip

                                if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*sx/8);
                                }
                                else //2D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*16);
                                }

                                u32 tileindex = tilebaseno+tileadd;

                                if(tileindex >= 256) //each tile needs double space
                                {
                                    u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+(tileindex*64)]);

                                    int _x = xdiff & 7;
                                    int _y = ydiff & 7;

                                    u8 data = tile_ptr[_x+(_y*8)];

                                    if(data)
                                    {
                                        if(mode == 0)
                                        {
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 1) //transp
                                        {
                                            sprblend[prio][j] = 1;
                                            sprblendfb[prio][j] = palptr[data];
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 2) //3 = prohibited
                                        {
                                            sprwin[j] = 1;
                                        }
                                    }
                                }
                            }
                            j++;
                        }
                    }
                    else //16 colors
                    {
                        u16 palno = attr2>>12;
                        u16 * palptr = (u16*)&Mem.pal_ram[512+(palno*32)];

                        int j = (x < 0) ? 0 : x; //search start point
                        while(j<(x+sx) && (j < 240))
                        {
                            if((sprvisible[prio][j] == 0) || (mode == 2))
                            {
                                int xdiff = (j-x);

                                if(attr1 & BIT(12)) xdiff = sx-xdiff-1; //hflip

                                if(mosaic) xdiff = xdiff - xdiff%MosSprX;

                                u32 tileadd = 0;
                                if(REG_DISPCNT & BIT(6)) //1D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*sx/8);
                                }
                                else //2D
                                {
                                    int tilex = xdiff>>3;
                                    int tiley = ydiff>>3;
                                    tileadd = tilex + (tiley*32);
                                }

                                u32 tileindex = tilebaseno+tileadd;

                                if(tileindex >= 512)
                                {
                                    u8 * tile_ptr = (u8*)&(Mem.vram[0x10000+(tileindex*32)]);

                                    int _x = xdiff & 7;
                                    int _y = ydiff & 7;

                                    u8 data = tile_ptr[(_x/2)+(_y*4)];

                                    if(_x&1) data = data>>4;
                                    else data = data & 0xF;

                                    if(data)
                                    {
                                        if(mode == 0)
                                        {
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 1) //transp
                                        {
                                            sprblend[prio][j] = 1;
                                            sprblendfb[prio][j] = palptr[data];
                                            sprfb[prio][j] = palptr[data];
                                            sprvisible[prio][j] = 1;
                                        }
                                        else if(mode == 2) //3 = prohibited
                                        {
                                            sprwin[j] = 1;
                                        }
                                    }
                                }
                            }
                            j++;
                        }
                    }
                }
            }
        }
        spr++;
    }
}

//-----------------------------------------------------------------------------------
u16 bgfb[4][240]; int bgvisible[4][240]; u16 backdrop[240]; int backdropvisible[240];
//backdropvisible filled in GBA_FillFadeTables

static const u32 text_bg_size[4][2] = { {256,256}, {512,256}, {256,512}, {512,512} };

static inline u32 se_index(u32 tx, u32 ty, u32 pitch) //from tonc
{
    u32 sbb = (ty/32)*(pitch/32) + (tx/32);
    return sbb*1024 + (ty%32)*32 + tx%32;
}

static void gba_bg0drawtext(s32 y)
{
    int sx = REG_BG0HOFS;
    int sy = REG_BG0VOFS;
    u16 control = REG_BG0CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u16 * scrbaseblockptr = (u16*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 maskx = text_bg_size[control>>14][0]-1;
    u32 masky = text_bg_size[control>>14][1]-1;

    u32 startx = sx&maskx;
    u32 starty = (y+sy) & masky;

    u32 sizex = text_bg_size[control>>14][0]/8;

    int mosaic = (control & BIT(6)); //mosaic
    if(mosaic) starty -= starty%MosBgY;

    u16 * fb = bgfb[0];
    int * visptr = bgvisible[0];
    if(control & BIT(7)) //256 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            int data = charbaseblockptr[((SE&0x3FF)*64)  +  (_x+(_y*8))];

            *fb++ = ((u16*)Mem.pal_ram)[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
    else //16 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            //12-15-pal
            u16 * palptr = (u16*)&Mem.pal_ram[(SE>>12)*(2*16)];

            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            u32 data = charbaseblockptr[((SE&0x3FF)*32)  +  ((_x/2)+(_y*4))];

            if(_x&1) data = data>>4;
            else data = data & 0xF;

            *fb++ = palptr[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
}

static void gba_bg1drawtext(s32 y)
{
    int sx = REG_BG1HOFS;
    int sy = REG_BG1VOFS;
    u16 control = REG_BG1CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u16 * scrbaseblockptr = (u16*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 maskx = text_bg_size[control>>14][0]-1;
    u32 masky = text_bg_size[control>>14][1]-1;

    u32 startx = sx&maskx;
    u32 starty = (y+sy) & masky;

    u32 sizex = text_bg_size[control>>14][0]/8;

    int mosaic = (control & BIT(6)); //mosaic
    if(mosaic) starty -= starty%MosBgY;

    u16 * fb = bgfb[1];
    int * visptr = bgvisible[1];
    if(control & BIT(7)) //256 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            int data = charbaseblockptr[((SE&0x3FF)*64)  +  (_x+(_y*8))];

            *fb++ = ((u16*)Mem.pal_ram)[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
    else //16 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            //12-15-pal
            u16 * palptr = (u16*)&Mem.pal_ram[(SE>>12)*(2*16)];

            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            u32 data = charbaseblockptr[((SE&0x3FF)*32)  +  ((_x/2)+(_y*4))];

            if(_x&1) data = data>>4;
            else data = data & 0xF;

            *fb++ = palptr[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
}

static void gba_bg2drawtext(s32 y)
{
    int sx = REG_BG2HOFS;
    int sy = REG_BG2VOFS;
    u16 control = REG_BG2CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u16 * scrbaseblockptr = (u16*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 maskx = text_bg_size[control>>14][0]-1;
    u32 masky = text_bg_size[control>>14][1]-1;

    u32 startx = sx&maskx;
    u32 starty = (y+sy) & masky;

    u32 sizex = text_bg_size[control>>14][0]/8;

    int mosaic = (control & BIT(6)); //mosaic
    if(mosaic) starty -= starty%MosBgY;

    u16 * fb = bgfb[2];
    int * visptr = bgvisible[2];
    if(control & BIT(7)) //256 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            int _x = startx & 7;
            if(mosaic) _x = (startx - startx%MosBgX) & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            int data = charbaseblockptr[((SE&0x3FF)*64)  +  (_x+(_y*8))];

            *fb++ = ((u16*)Mem.pal_ram)[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
    else //16 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            //12-15-pal
            u16 * palptr = (u16*)&Mem.pal_ram[(SE>>12)*(2*16)];

            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            u32 data = charbaseblockptr[((SE&0x3FF)*32)  +  ((_x/2)+(_y*4))];

            if(_x&1) data = data>>4;
            else data = data & 0xF;

            *fb++ = palptr[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
}

static void gba_bg3drawtext(s32 y)
{
    int sx = REG_BG3HOFS;
    int sy = REG_BG3VOFS;
    u16 control = REG_BG3CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u16 * scrbaseblockptr = (u16*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 maskx = text_bg_size[control>>14][0]-1;
    u32 masky = text_bg_size[control>>14][1]-1;

    u32 startx = sx&maskx;
    u32 starty = (y+sy) & masky;

    u32 sizex = text_bg_size[control>>14][0]/8;

    int mosaic = (control & BIT(6)); //mosaic
    if(mosaic) starty -= starty%MosBgY;

    u16 * fb = bgfb[3];
    int * visptr = bgvisible[3];
    if(control & BIT(7)) //256 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            int data = charbaseblockptr[((SE&0x3FF)*64)  +  (_x+(_y*8))];

            *fb++ = ((u16*)Mem.pal_ram)[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
    else //16 colors
    {
        int i;
        for(i = 0; i < 240; i++)
        {
            startx = (sx+i)&maskx;
            if(mosaic) startx -= startx%MosBgX;

            u32 index = se_index(startx/8,starty/8,sizex);
            u16 SE = scrbaseblockptr[index];
            //screen entry data
            //0-9 tile id
            //10-hflip
            //11-vflip
            //12-15-pal
            u16 * palptr = (u16*)&Mem.pal_ram[(SE>>12)*(2*16)];

            int _x = startx & 7;
            if(SE & BIT(10)) _x = 7-_x; //hflip

            int _y = starty & 7;
            if(SE & BIT(11)) _y = 7-_y; //vflip

            u32 data = charbaseblockptr[((SE&0x3FF)*32)  +  ((_x/2)+(_y*4))];

            if(_x&1) data = data>>4;
            else data = data & 0xF;

            *fb++ = palptr[data];
            *visptr++ = data;

            //startx = (startx+1)&maskx;
        }
    }
}

//-----------------------------------------------------------------------------------

static inline u32 se_index_affine(u32 tx, u32 ty, u32 tpitch)
{
    return (ty * tpitch) + tx;
}

static const u32 affine_bg_size[4] = { 128, 256, 512, 1024 };

s32 mosBG2lastx, mosBG2lasty, mos2A, mos2C;
static void gba_bg2drawaffine(s32 y)
{
    u16 control = REG_BG2CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u8 * scrbaseblockptr = (u8*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 size = affine_bg_size[control>>14];
    u32 sizemask = size-1;
    u32 tilesize = size/8;

    s32 currx = BG2lastx;
    s32 curry = BG2lasty;

    // | PA PB |
    // | PC PD |

    s32 A = (s32)(s16)REG_BG2PA;
    s32 C = (s32)(s16)REG_BG2PC;

    u16 * fb = bgfb[2];
    int * visptr = bgvisible[2];

    int mosaic = (control & BIT(6)); //mosaic

    if(mosaic)
    {
        if(y%MosBgY == 0)
        {
            mosBG2lastx = BG2lastx;
            mosBG2lasty = BG2lasty;
            mos2A = A;
            mos2C = C;
        }
        else
        {
            currx = mosBG2lastx;
            curry = mosBG2lasty;
            A = mos2A;
            C = mos2C;
        }
    }

    u8 data = 0;
    int i;
    for(i = 0; i < 240; i++) //always 256 color
    {
        u32 _x = (currx>>8);
        u32 _y = (curry>>8);

        if(!mosaic || ((i%MosBgX) == 0))
        {
            data = 0;
            if(control & BIT(13)) //wrap
            {
                _x &= sizemask;
                _y &= sizemask;

                int __x = _x & 7;
                int __y = _y & 7;

                u32 index = se_index_affine(_x/8,_y/8,tilesize);
                u8 SE = scrbaseblockptr[index];
                data = charbaseblockptr[(SE*64) + (__x+(__y*8))];
            }
            else if(_x<size && _y<size)
            {
                int __x = _x & 7;
                int __y = _y & 7;

                u32 index = se_index_affine(_x/8,_y/8,tilesize);
                u8 SE = scrbaseblockptr[index];
                data = charbaseblockptr[(SE*64) + (__x+(__y*8))];
            }
        }
        *fb++ = ((u16*)Mem.pal_ram)[data];
        *visptr++ = data;

        currx += A;
        curry += C;
    }
}

s32 mosBG3lastx, mosBG3lasty, mos3A, mos3C;
static void gba_bg3drawaffine(s32 y)
{
    u16 control = REG_BG3CNT;

    u8 * charbaseblockptr = (u8*)&Mem.vram[((control>>2)&3) * (16*1024)];
    u8 * scrbaseblockptr = (u8*)&Mem.vram[((control>>8)&0x1F) * (2*1024)];

    u32 size = affine_bg_size[control>>14];
    u32 sizemask = size-1;
    u32 tilesize = size/8;

    s32 currx = BG3lastx;
    s32 curry = BG3lasty;

    // | PA PB |
    // | PC PD |

    s32 A = (s32)(s16)REG_BG3PA;
    s32 C = (s32)(s16)REG_BG3PC;

    u16 * fb = bgfb[3];
    int * visptr = bgvisible[3];

    int mosaic = (control & BIT(6)); //mosaic

    if(mosaic)
    {
        if(y%MosBgY == 0)
        {
            mosBG3lastx = BG3lastx;
            mosBG3lasty = BG3lasty;
            mos3A = A;
            mos3C = C;
        }
        else
        {
            currx = mosBG3lastx;
            curry = mosBG3lasty;
            A = mos3A;
            C = mos3C;
        }
    }

    u8 data = 0;
    int i;
    for(i = 0; i < 240; i++) //always 256 color
    {
        u32 _x = (currx>>8);
        u32 _y = (curry>>8);

        if(!mosaic || ((i%MosBgX) == 0))
        {
            data = 0;
            if(control & BIT(13)) //wrap
            {
                _x &= sizemask;
                _y &= sizemask;

                int __x = _x & 7;
                int __y = _y & 7;

                u32 index = se_index_affine(_x/8,_y/8,tilesize);
                u8 SE = scrbaseblockptr[index];
                data = charbaseblockptr[(SE*64) + (__x+(__y*8))];
            }
            else if(_x<size && _y<size)
            {
                int __x = _x & 7;
                int __y = _y & 7;

                u32 index = se_index_affine(_x/8,_y/8,tilesize);
                u8 SE = scrbaseblockptr[index];
                data = charbaseblockptr[(SE*64) + (__x+(__y*8))];
            }
        }

        *fb++ = ((u16*)Mem.pal_ram)[data];
        *visptr++ = data;

        currx += A;
        curry += C;
    }
}

//-----------------------------------------------------------------------------------

static void gba_bg2drawbitmapmode3(s32 y)
{
    s32 currx = BG2lastx;
    s32 curry = BG2lasty;

    u16 * srcptr = (u16*)&Mem.vram;

    // | PA PB |
    // | PC PD |

    s32 A = (s32)(s16)REG_BG2PA;
    s32 C = (s32)(s16)REG_BG2PC;

    u16 * fb = bgfb[2];
    int * visptr = bgvisible[2];

    int i;
    for(i = 0; i < 240; i++)
    {
        u32 x = (currx>>8);
        s32 y = (curry>>8);
        if( !( (x > 239) || (y > 159) ) )
        {
            *fb = srcptr[x+240*y];
            *visptr = 1;
        }
        fb++; visptr++;
        currx += A;
        curry += C;
    }
}

static void gba_bg2drawbitmapmode4(s32 y)
{
    s32 currx = BG2lastx;
    s32 curry = BG2lasty;

    u8 * srcptr = (u8*)&Mem.vram[(REG_DISPCNT&BIT(4))?0xA000:0];

    // | PA PB |
    // | PC PD |

    s32 A = (s32)(s16)REG_BG2PA;
    s32 C = (s32)(s16)REG_BG2PC;

    u16 * fb = bgfb[2];
    int * visptr = bgvisible[2];

    int i;
    for(i = 0; i < 240; i++)
    {
        u32 x = (currx>>8);
        s32 y = (curry>>8);
        if( !( (x > 239) || (y > 159) ) )
        {
            *fb = ((u16*)Mem.pal_ram)[srcptr[x+240*y]];
            *visptr = 1;
        }
        fb++;
        visptr++;
        currx += A;
        curry += C;
    }
}

static void gba_bg2drawbitmapmode5(s32 y)
{
    s32 currx = BG2lastx;
    s32 curry = BG2lasty;

    u16 * srcptr = (u16*)&Mem.vram[((REG_DISPCNT&BIT(4))?0xA000:0)];

    // | PA PB |
    // | PC PD |

    s32 A = (s32)(s16)REG_BG2PA;
    s32 C = (s32)(s16)REG_BG2PC;

    u16 * fb = bgfb[2];
    int * visptr = bgvisible[2];

    int i;
    for(i = 0; i < 240; i++)
    {
        u32 x = (currx>>8);
        s32 y = (curry>>8);
        if( !( (x > 159) || (y > 127) ) )
        {
            *fb = (u16)srcptr[x+160*y];
            *visptr = 1;
        }
        fb++;
        visptr++;
        currx += A;
        curry += C;
    }

}

//-----------------------------------------------------------------------------------

static inline void gba_video_all_buffers_clear(void)
{
    mem_clear_32((u32*)bgfb,sizeof(bgfb)); mem_clear_32((u32*)bgvisible,sizeof(bgvisible));
    mem_clear_32((u32*)sprfb,sizeof(sprfb)); mem_clear_32((u32*)sprvisible,sizeof(sprvisible));
    mem_clear_32((u32*)sprblend,sizeof(sprblend)); mem_clear_32((u32*)sprblendfb,sizeof(sprblendfb));
    mem_clear_32((u32*)sprwin,sizeof(sprwin)); mem_clear_32((u32*)backdrop,sizeof(backdrop));
}

//-----------------------------------------------------------------------------------

/*
typedef enum {
    BG0 = BIT(0), //gba_sort_layers sets this values
    BG1 = BIT(1),
    BG2 = BIT(2),
    BG3 = BIT(3),
    SPR0 = BIT(4)|BIT(10),
    SPR1 = BIT(5)|BIT(10),
    SPR2 = BIT(6)|BIT(10),
    SPR3 = BIT(7)|BIT(10),
    BD = BIT(8),

    LAYER_BG0 = BIT(0), //gba_effects_apply() uses this values
    LAYER_BG1 = BIT(1),
    LAYER_BG2 = BIT(2),
    LAYER_BG3 = BIT(3),
    LAYER_SPR = BIT(10),
    LAYER_BD  = BIT(8)
} _layer_type_;
*/
typedef enum { BG0 = 0,BG1,BG2,BG3,  SPR0,SPR1,SPR2,SPR3,   BD } _layer_type_;

int * layer_vis[9];
u16 * layer_fb[9];
_layer_type_ layer_id[9];
int layer_active_num;

static inline void gba_sort_layers(int video_mode)
{
    static const int bg0act[6] = {1,1,0,0,0,0}; static const int bg1act[6] = {1,1,0,0,0,0};
    static const int bg2act[6] = {1,1,1,1,1,1}; static const int bg3act[6] = {1,0,1,0,0,0};

    u16 cnt = REG_DISPCNT;

    int bgprio[4];
    bgprio[0] = ((cnt & BIT(8)) && bg0act[video_mode]) ? (REG_BG0CNT&3) : -1;
    bgprio[1] = ((cnt & BIT(9)) && bg1act[video_mode]) ? (REG_BG1CNT&3) : -1;
    bgprio[2] = ((cnt & BIT(10)) && bg2act[video_mode]) ? (REG_BG2CNT&3) : -1;
    bgprio[3] = ((cnt & BIT(11)) && bg3act[video_mode]) ? (REG_BG3CNT&3) : -1;

    int spren = cnt & BIT(12);

    int i;
    int cur_layer = 0;

    //BACKDROP
    layer_vis[cur_layer] = backdropvisible; layer_fb[cur_layer] = backdrop;
    layer_id[cur_layer] = BD; cur_layer++;

    //PRIORITY 3
    for(i = 3; i >= 0; i--) if(bgprio[i] == 3)
    { bgprio[i] = -1; layer_vis[cur_layer] = bgvisible[i];
    layer_fb[cur_layer] = bgfb[i]; layer_id[cur_layer] = BG0+i; cur_layer++; }

    if(spren)
    { layer_vis[cur_layer] = sprvisible[3]; layer_fb[cur_layer] = sprfb[3];
    layer_id[cur_layer] = SPR3; cur_layer++; }

    //PRIORITY 2
    for(i = 3; i >= 0; i--) if(bgprio[i] == 2)
    { bgprio[i] = -1; layer_vis[cur_layer] = bgvisible[i];
    layer_fb[cur_layer] = bgfb[i]; layer_id[cur_layer] = BG0+i; cur_layer++; }

    if(spren)
    { layer_vis[cur_layer] = sprvisible[2]; layer_fb[cur_layer] = sprfb[2];
    layer_id[cur_layer] = SPR2; cur_layer++; }

    //PRIORITY 1
    for(i = 3; i >= 0; i--) if(bgprio[i] == 1)
    { bgprio[i] = -1; layer_vis[cur_layer] = bgvisible[i];
    layer_fb[cur_layer] = bgfb[i]; layer_id[cur_layer] = BG0+i; cur_layer++; }

    if(spren)
    { layer_vis[cur_layer] = sprvisible[1]; layer_fb[cur_layer] = sprfb[1];
    layer_id[cur_layer] = SPR1; cur_layer++; }

    //PRIORITY 0
    for(i = 3; i >= 0; i--) if(bgprio[i] == 0)
    { bgprio[i] = -1; layer_vis[cur_layer] = bgvisible[i];
    layer_fb[cur_layer] = bgfb[i]; layer_id[cur_layer] = BG0+i; cur_layer++; }

    if(spren)
    { layer_vis[cur_layer] = sprvisible[0]; layer_fb[cur_layer] = sprfb[0];
    layer_id[cur_layer] = SPR0; cur_layer++; }

    //END
    layer_active_num = cur_layer;
}

static inline void gba_blit_layers(int y)
{
    u16 * destptr = (u16*)&screen_buffer[240*y];

    int i;
    for(i = 0; i < layer_active_num; i ++)
    {
        u16 * dest = destptr;

        int * vis = layer_vis[i];
        u16 * fb = layer_fb[i];

        int j;
        for(j = 0; j < 240; j++)
        {
            if(*vis++) { *dest = *fb; }
            dest++; fb++;
        }
    }
/*
#warning "delete this"
    destptr = (u16*)&screen_buffer[240*y];
    int g; for(g = 0; g < 240; g+=2)
    { if(sprwin[g]) destptr[g] = 0x7C1F; if(sprwin[g+1]) destptr[g+1] = 0x7C00; }
*/
/*
    char * te[9] =  {"BG0", "BG1", "BG2", "BG3", "SPR0", "SPR1", "SPR2", "SPR3", "BD" };
    char text[128]; sprintf(text,"%d -- %s %s %s %s %s %s %s %s %s",layer_active_num,
    layer_active_num > 0 ? te[layer_id[0]] : " ",layer_active_num > 1 ? te[layer_id[1]] : " ",
    layer_active_num > 2 ? te[layer_id[2]] : " ",layer_active_num > 3 ? te[layer_id[3]] : " ",
    layer_active_num > 4 ? te[layer_id[4]] : " ",layer_active_num > 5 ? te[layer_id[5]] : " ",
    layer_active_num > 6 ? te[layer_id[6]] : " ",layer_active_num > 7 ? te[layer_id[7]] : " ",
    layer_active_num > 8 ? te[layer_id[8]] : " "); GLWindow_SetCaption(text);
*/
}

//-----------------------------------------------------------------------------------

int coloreffect[240];

static inline void gba_window_apply(int y, int win0, int win1, int winobj)
{
    if(!(winobj||win1||win0))
    {
        if((REG_BLDCNT>>6)&3) //special effect
        { int i; for(i = 0; i < 240; i++) coloreffect[i] = 1; }
        return;
    }

    u32 in0 = REG_WININ&0xFF; u32 in1 = (REG_WININ>>8)&0xFF;
    u32 out = REG_WINOUT&0xFF; u32 inobj = (REG_WINOUT>>8)&0xFF;

    int win_show[240];
    int i;

    if(REG_DISPCNT & BIT(8))
    {
        if(out&BIT(0)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(0))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(0))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(0))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        int * vis = bgvisible[0]; int * show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
    }
    if(REG_DISPCNT & BIT(9))
    {
        if(out&BIT(1)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(1))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(1))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(1))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        int * vis = bgvisible[1]; int * show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
    }
    if(REG_DISPCNT & BIT(10))
    {
        if(out&BIT(2)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(2))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(2))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(2))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        int * vis = bgvisible[2]; int * show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
    }
    if(REG_DISPCNT & BIT(11))
    {
        if(out&BIT(3)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(3))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(3))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(3))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        int * vis = bgvisible[3]; int * show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
    }
    if(REG_DISPCNT & BIT(12)) //sprites
    {
        if(out&BIT(4)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(4))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(4))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(4))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        int * vis = sprvisible[0]; int * show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
        vis = sprvisible[1]; show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
        vis = sprvisible[2]; show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
        vis = sprvisible[3]; show = win_show;
        for(i = 0; i < 240; i++) { *vis = *vis && *show; vis++; show++; }
    }
    if((REG_BLDCNT>>6)&3) //special effect
    {
        if(out&BIT(5)) { for(i = 0; i < 240; i++) win_show[i] = 1; }
        else memset(win_show,0,sizeof(win_show));
        if(winobj) //obj has lowest priority
        {
            int * show = win_show; int * ptrsprwin = sprwin;
            if(inobj&BIT(5))
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 1; show++; ptrsprwin++; } }
            else
            { for(i = 0; i < 240; i++) { if(*ptrsprwin) *show = 0; show++; ptrsprwin++; } }
        }
        if(win1) //intermediate priority
        {
            if(in1&BIT(5))
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win1Y1 && y <= Win1Y2) for(i = Win1X1; i < Win1X2; i++) win_show[i] = 0; }
        }
        if(win0) //highest priority
        {
            if(in0&BIT(5))
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 1; }
            else
            { if(y >= Win0Y1 && y <= Win0Y2) for(i = Win0X1; i < Win0X2; i++) win_show[i] = 0; }
        }

        memcpy(coloreffect,win_show,sizeof(coloreffect));
    }
}

u16 white_table[32][17]; //color, evy
u16 black_table[32][17]; //color, evy

void GBA_FillFadeTables(void)
{
    int i;
    for(i = 0; i < 32; i++)
    {
        int y;
        for(y = 0; y < 17; y++)
        {
            white_table[i][y] = i + (((31-i)*y)>>4);
            black_table[i][y] = i - ((i*y)>>4);
        }
    }

    //fill array -- backdrop always visible
    //any place is good for filling this array... :P
    for(i = 0; i < 240; i++) backdropvisible[i] = 1;
}

static inline u16 fade_white(u16 col, u16 evy)
{
    return (white_table[(col>>10)&0x1F][evy]<<10)|(white_table[(col>>5)&0x1F][evy]<<5)|white_table[col&0x1F][evy];
}
static inline u16 fade_black(u16 col, u16 evy)
{
    return (black_table[(col>>10)&0x1F][evy]<<10)|(black_table[(col>>5)&0x1F][evy]<<5)|black_table[col&0x1F][evy];
}

static inline u16 min(u16 a, u16 b) { return (a<b) ? a : b ; }
static inline u16 blend(u16 col_1, u16 col_2, u16 eva, u16 evb)
{
    u16 r = min(31, (((col_1&0x1F)*eva)>>4) + (((col_2&0x1F)*evb)>>4) );
    u16 g = min(31, ((((col_1>>5)&0x1F)*eva)>>4) + ((((col_2>>5)&0x1F)*evb)>>4) );
    u16 b = min(31, ((((col_1>>10)&0x1F)*eva)>>4) + ((((col_2>>10)&0x1F)*evb)>>4) );
    return (b<<10)|(g<<5)|r;
}

static inline void gba_effects_apply(void)
{
/*
    u16 bldcnt = REG_BLDCNT;
    int mode = (bldcnt>>6)&3;
    int i;

    if(mode == 0) //nothing
    {
        //blend transparent sprites
        if(bldcnt>>8) //at least, one 2nd target
        {
            int ret = 1; //if any sprite is in blending mode, continue, else return
            i = 0;
            while(i < 240)
            {
                if(sprblend[0][i]|sprblend[1][i]|sprblend[2][i]|sprblend[3][i]) { ret = 0; break; }
                i++;
            }
            if(ret) return;
        }
        else return;
    }

    //First, sort layers
    int * firsttarget_layer_vis[9];
    u16 * firsttarget_layer_fb[9];
    _layer_type_ firsttarget_layer_id[9];

    int * secondtarget_layer_vis[9];
    u16 * secondtarget_layer_fb[9];
    _layer_type_ secondtarget_layer_id[9];

    int firstcount = 0, secondcount = 0;
    for(i = layer_active_num-1; i > 0; i--)
    {
        if(layer_id[i]&LAYER_BG0)
        {
            if(bldcnt&BIT(0))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = BG0;
                firstcount++;
            }
            if(bldcnt&BIT(8))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = BG0;
                secondcount++;
            }
        }
        else if(layer_id[i]&LAYER_BG1)
        {
            if(bldcnt&BIT(1))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = BG1;
                firstcount++;
            }
            if(bldcnt&BIT(9))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = BG1;
                secondcount++;
            }
        }
        else if(layer_id[i]&LAYER_BG2)
        {
            if(bldcnt&BIT(2))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = BG2;
                firstcount++;
            }
            if(bldcnt&BIT(10))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = BG2;
                secondcount++;
            }
        }
        else if(layer_id[i]&LAYER_BG3)
        {
            if(bldcnt&BIT(3))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = BG3;
                firstcount++;
            }
            if(bldcnt&BIT(11))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = BG3;
                secondcount++;
            }
        }
        else if(layer_id[i]&LAYER_SPR)
        {
            if(bldcnt&BIT(4))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = layer_id[i];
                firstcount++;
            }
            if(bldcnt&BIT(12))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = layer_id[i];
                secondcount++;
            }
        }
        else if(layer_id[i]&LAYER_BD)
        {
            if(bldcnt&BIT(5))
            {
                firsttarget_layer_vis[firstcount] = layer_vis[i];
                firsttarget_layer_fb[firstcount] = layer_fb[i];
                firsttarget_layer_id[firstcount] = BD;
                firstcount++;
            }
            if(bldcnt&BIT(13))
            {
                secondtarget_layer_vis[secondcount] = layer_vis[i];
                secondtarget_layer_fb[secondcount] = layer_fb[i];
                secondtarget_layer_id[secondcount] = BD;
                secondcount++;
            }
        }
    }

    //Then, look the top pixel of first target. If the top pixel of second target is the same,
    //go one layer down. If not second target pixel, no blending.




    return;

    //BLIT 2ND TARGET LAYERS
    u16 second_target[240];
    int second_target_vis[240];
    u16 second_target_nospr[240];
    int second_target_vis_nospr[240];
    mem_clear_32(second_target_vis,sizeof(second_target_vis));
    mem_clear_32(second_target_vis_nospr,sizeof(second_target_vis_nospr));

    for(i = 0; i < layer_active_num; i ++)
    {
        int draw = 0, spr = 0, isfirst = 0;
        switch(layer_id[i])
        {
            case BG0: isfirst = bldcnt&BIT(0); draw = bldcnt&BIT(8); break;
            case BG1: isfirst = bldcnt&BIT(1); draw = bldcnt&BIT(9); break;
            case BG2: isfirst = bldcnt&BIT(2); draw = bldcnt&BIT(10); break;
            case BG3: isfirst = bldcnt&BIT(3); draw = bldcnt&BIT(11); break;
            case SPR0: case SPR1: case SPR2: case SPR3:
                spr = 1; isfirst = bldcnt&BIT(4); draw = bldcnt&BIT(12); break;
            case BD: isfirst = bldcnt&BIT(5); draw = bldcnt&BIT(13); break;
            default: draw = 0; break; //???
        }

        if(draw)
        {
            u16 * dest = second_target;
            int * snd_vis = second_target_vis;

            int * vis = layer_vis[i];
            u16 * fb = layer_fb[i];

            int j;
            if(isfirst == 0)
            {
                for(j = 0; j < 240; j++)
                {
                    if(*vis) { *dest = *fb; *snd_vis = 1; }
                    dest++; fb++; snd_vis++; vis++;
                }
            }

            //For semi transparent sprites
            if(spr == 0)
            {
                u16 * dest_nospr = second_target_nospr;
                int * snd_vis_nospr = second_target_vis_nospr;

                vis = layer_vis[i];
                fb = layer_fb[i];

                for(j = 0; j < 240; j++)
                {
                    if(*vis) { *dest_nospr = *fb; *snd_vis_nospr = 1; }
                    fb++; dest_nospr++; snd_vis_nospr++; vis++;
                }
            }
        }
    }

    u32 eva = REG_BLDALPHA&0x1F; if(eva > 16) eva = 16;
    u32 evb = (REG_BLDALPHA>>8)&0x1F; if(evb > 16) evb = 16;

    for(i = 0; i < 240; i++) if(second_target_vis_nospr[i])
    {
        if(sprblend[0][i]) sprfb[0][i] = blend(sprblendfb[0][i],second_target_nospr[i],eva,evb);
        if(sprblend[1][i]) sprfb[1][i] = blend(sprblendfb[1][i],second_target_nospr[i],eva,evb);
        if(sprblend[2][i]) sprfb[2][i] = blend(sprblendfb[2][i],second_target_nospr[i],eva,evb);
        if(sprblend[3][i]) sprfb[3][i] = blend(sprblendfb[3][i],second_target_nospr[i],eva,evb);
    }

    if(mode == 0) return; //nothing more to do here

    if(mode == 1) //blend
    {
        //BLEND WITH 1ST TARGET LAYERS
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[0][i] && second_target_vis[i])
            bgfb[0][i] = blend(bgfb[0][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[1][i] && second_target_vis[i])
            bgfb[1][i] = blend(bgfb[1][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[2][i] && second_target_vis[i])
            bgfb[2][i] = blend(bgfb[2][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[3][i] && second_target_vis[i])
            bgfb[3][i] = blend(bgfb[3][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(4))
        {
            for(i = 0; i < 240; i++) if(second_target_vis[i] && coloreffect[i])
            {
                if(sprvisible[0][i] && (sprblend[0][i]==0)) sprfb[0][i] = blend(sprfb[0][i],second_target[i],eva,evb);
                if(sprvisible[1][i] && (sprblend[1][i]==0)) sprfb[1][i] = blend(sprfb[1][i],second_target[i],eva,evb);
                if(sprvisible[2][i] && (sprblend[2][i]==0)) sprfb[2][i] = blend(sprfb[2][i],second_target[i],eva,evb);
                if(sprvisible[3][i] && (sprblend[3][i]==0)) sprfb[3][i] = blend(sprfb[3][i],second_target[i],eva,evb);
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && second_target_vis[i])
            backdrop[i] = blend(backdrop[i],second_target[i],eva,evb); }
        return;
    }

    if(mode == 2) //white
    {
        u32 evy = REG_BLDY&0x1F; if(evy > 16) evy = 16;
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[0][i] = fade_white(bgfb[0][i],evy); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[1][i] = fade_white(bgfb[1][i],evy); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[2][i] = fade_white(bgfb[2][i],evy); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[3][i] = fade_white(bgfb[3][i],evy); }
        if(bldcnt&BIT(4))
        {
            int bg_pixel_used[240];
            int i; for(i = 0; i < 240; i++)
                bg_pixel_used[i] = bgvisible[0][i] | bgvisible[1][i] | bgvisible[2][i] | bgvisible[3][i];

            for(i = 0; i < 240; i++) if(coloreffect[i])
            {
                if(!second_target_vis[i]) //if = 0, blend (done before)
                {
                    sprfb[0][i] = fade_white(sprfb[0][i],evy);
                    sprfb[1][i] = fade_white(sprfb[1][i],evy);
                    sprfb[2][i] = fade_white(sprfb[2][i],evy);
                    sprfb[3][i] = fade_white(sprfb[3][i],evy);
                }
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) backdrop[i] = fade_white(backdrop[i],evy); }
        return;
    }
    if(mode ==3) //Black
    {
        u32 evy = REG_BLDY&0x1F; if(evy > 16) evy = 16;
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[0][i] = fade_black(bgfb[0][i],evy); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[1][i] = fade_black(bgfb[1][i],evy); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[2][i] = fade_black(bgfb[2][i],evy); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[3][i] = fade_black(bgfb[3][i],evy); }
        if(bldcnt&BIT(4))
        {
            int bg_pixel_used[240];
            int i; for(i = 0; i < 240; i++)
                bg_pixel_used[i] = bgvisible[0][i] | bgvisible[1][i] | bgvisible[2][i] | bgvisible[3][i];

            for(i = 0; i < 240; i++) if(coloreffect[i])
            {
                if(!second_target_vis[i]) //if = 0, blend (done before)
                {
                    sprfb[0][i] = fade_black(sprfb[0][i],evy);
                    sprfb[1][i] = fade_black(sprfb[1][i],evy);
                    sprfb[2][i] = fade_black(sprfb[2][i],evy);
                    sprfb[3][i] = fade_black(sprfb[3][i],evy);
                }
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) backdrop[i] = fade_black(backdrop[i],evy); }
        return;
    }
*/

    u16 bldcnt = REG_BLDCNT;
    int mode = (bldcnt>>6)&3;
    int i;

    if(mode == 0) //nothing
    {
        //blend transparent sprites
        if(bldcnt>>8) //at least, one 2nd target
        {
            int ret = 1; //if any sprite is in blending mode, continue, else return
            i = 0;
            while(i < 240)
            {
                if(sprblend[0][i]|sprblend[1][i]|sprblend[2][i]|sprblend[3][i]) { ret = 0; break; }
                i++;
            }
            if(ret) return;
        }
        else return;
    }

    //BLIT 2ND TARGET LAYERS
    u16 second_target[240];
    int second_target_vis[240];
    u16 second_target_nospr[240];
    int second_target_vis_nospr[240];
    mem_clear_32(second_target_vis,sizeof(second_target_vis));
    mem_clear_32(second_target_vis_nospr,sizeof(second_target_vis_nospr));

    for(i = 0; i < layer_active_num; i ++)
    {
        int draw = 0, spr = 0, isfirst = 0;
        switch(layer_id[i])
        {
            case BG0: isfirst = bldcnt&BIT(0); draw = bldcnt&BIT(8); break;
            case BG1: isfirst = bldcnt&BIT(1); draw = bldcnt&BIT(9); break;
            case BG2: isfirst = bldcnt&BIT(2); draw = bldcnt&BIT(10); break;
            case BG3: isfirst = bldcnt&BIT(3); draw = bldcnt&BIT(11); break;
            case SPR0: case SPR1: case SPR2: case SPR3:
                spr = 1; isfirst = bldcnt&BIT(4); draw = bldcnt&BIT(12); break;
            case BD: isfirst = bldcnt&BIT(5); draw = bldcnt&BIT(13); break;
            default: draw = 0; break; //???
        }

        if(draw)
        {
            u16 * dest = second_target;
            int * snd_vis = second_target_vis;

            int * vis = layer_vis[i];
            u16 * fb = layer_fb[i];

            int j;
            if(isfirst == 0)
            {
                for(j = 0; j < 240; j++)
                {
                    if(*vis) { *dest = *fb; *snd_vis = 1; }
                    dest++; fb++; snd_vis++; vis++;
                }
            }

            //For semi transparent sprites
            if(spr == 0)
            {
                u16 * dest_nospr = second_target_nospr;
                int * snd_vis_nospr = second_target_vis_nospr;

                vis = layer_vis[i];
                fb = layer_fb[i];

                for(j = 0; j < 240; j++)
                {
                    if(*vis) { *dest_nospr = *fb; *snd_vis_nospr = 1; }
                    fb++; dest_nospr++; snd_vis_nospr++; vis++;
                }
            }
        }
    }

    u32 eva = REG_BLDALPHA&0x1F; if(eva > 16) eva = 16;
    u32 evb = (REG_BLDALPHA>>8)&0x1F; if(evb > 16) evb = 16;

    for(i = 0; i < 240; i++) if(second_target_vis_nospr[i])
    {
        if(sprblend[0][i]) sprfb[0][i] = blend(sprblendfb[0][i],second_target_nospr[i],eva,evb);
        if(sprblend[1][i]) sprfb[1][i] = blend(sprblendfb[1][i],second_target_nospr[i],eva,evb);
        if(sprblend[2][i]) sprfb[2][i] = blend(sprblendfb[2][i],second_target_nospr[i],eva,evb);
        if(sprblend[3][i]) sprfb[3][i] = blend(sprblendfb[3][i],second_target_nospr[i],eva,evb);
    }

    if(mode == 0) return; //nothing more to do here

    if(mode == 1) //blend
    {
        //BLEND WITH 1ST TARGET LAYERS
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[0][i] && second_target_vis[i])
            bgfb[0][i] = blend(bgfb[0][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[1][i] && second_target_vis[i])
            bgfb[1][i] = blend(bgfb[1][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[2][i] && second_target_vis[i])
            bgfb[2][i] = blend(bgfb[2][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && bgvisible[3][i] && second_target_vis[i])
            bgfb[3][i] = blend(bgfb[3][i],second_target[i],eva,evb); }
        if(bldcnt&BIT(4))
        {
            for(i = 0; i < 240; i++) if(second_target_vis[i] && coloreffect[i])
            {
                if(sprvisible[0][i] && (sprblend[0][i]==0)) sprfb[0][i] = blend(sprfb[0][i],second_target[i],eva,evb);
                if(sprvisible[1][i] && (sprblend[1][i]==0)) sprfb[1][i] = blend(sprfb[1][i],second_target[i],eva,evb);
                if(sprvisible[2][i] && (sprblend[2][i]==0)) sprfb[2][i] = blend(sprfb[2][i],second_target[i],eva,evb);
                if(sprvisible[3][i] && (sprblend[3][i]==0)) sprfb[3][i] = blend(sprfb[3][i],second_target[i],eva,evb);
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i] && second_target_vis[i])
            backdrop[i] = blend(backdrop[i],second_target[i],eva,evb); }
        return;
    }

    if(mode == 2) //white
    {
        u32 evy = REG_BLDY&0x1F; if(evy > 16) evy = 16;
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[0][i] = fade_white(bgfb[0][i],evy); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[1][i] = fade_white(bgfb[1][i],evy); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[2][i] = fade_white(bgfb[2][i],evy); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[3][i] = fade_white(bgfb[3][i],evy); }
        if(bldcnt&BIT(4))
        {
            int bg_pixel_used[240];
            int i; for(i = 0; i < 240; i++)
                bg_pixel_used[i] = bgvisible[0][i] | bgvisible[1][i] | bgvisible[2][i] | bgvisible[3][i];

            for(i = 0; i < 240; i++) if(coloreffect[i])
            {
                if(!second_target_vis[i]) //if = 0, blend (done before)
                {
                    sprfb[0][i] = fade_white(sprfb[0][i],evy);
                    sprfb[1][i] = fade_white(sprfb[1][i],evy);
                    sprfb[2][i] = fade_white(sprfb[2][i],evy);
                    sprfb[3][i] = fade_white(sprfb[3][i],evy);
                }
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) backdrop[i] = fade_white(backdrop[i],evy); }
        return;
    }
    if(mode ==3) //Black
    {
        u32 evy = REG_BLDY&0x1F; if(evy > 16) evy = 16;
        if(bldcnt&BIT(0))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[0][i] = fade_black(bgfb[0][i],evy); }
        if(bldcnt&BIT(1))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[1][i] = fade_black(bgfb[1][i],evy); }
        if(bldcnt&BIT(2))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[2][i] = fade_black(bgfb[2][i],evy); }
        if(bldcnt&BIT(3))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) bgfb[3][i] = fade_black(bgfb[3][i],evy); }
        if(bldcnt&BIT(4))
        {
            int bg_pixel_used[240];
            int i; for(i = 0; i < 240; i++)
                bg_pixel_used[i] = bgvisible[0][i] | bgvisible[1][i] | bgvisible[2][i] | bgvisible[3][i];

            for(i = 0; i < 240; i++) if(coloreffect[i])
            {
                if(!second_target_vis[i]) //if = 0, blend (done before)
                {
                    sprfb[0][i] = fade_black(sprfb[0][i],evy);
                    sprfb[1][i] = fade_black(sprfb[1][i],evy);
                    sprfb[2][i] = fade_black(sprfb[2][i],evy);
                    sprfb[3][i] = fade_black(sprfb[3][i],evy);
                }
            }
        }
        if(bldcnt&BIT(5))
        { for(i = 0; i < 240; i++) if(coloreffect[i]) backdrop[i] = fade_black(backdrop[i],evy); }
        return;
    }
}

//--------------------------------------------------------------------------------------

void GBA_DrawScanlineMode0(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT & BIT(8)) gba_bg0drawtext(y);
    if(REG_DISPCNT & BIT(9)) gba_bg1drawtext(y);
    if(REG_DISPCNT & BIT(10)) gba_bg2drawtext(y);
    if(REG_DISPCNT & BIT(11)) gba_bg3drawtext(y);
    if(REG_DISPCNT & BIT(12)) gba_sprites_draw_mode012(y);
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(0);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

void GBA_DrawScanlineMode1(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT & BIT(8)) gba_bg0drawtext(y);
    if(REG_DISPCNT & BIT(9)) gba_bg1drawtext(y);
    if(REG_DISPCNT & BIT(10)) gba_bg2drawaffine(y);
    if(REG_DISPCNT & BIT(12)) gba_sprites_draw_mode012(y);
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(1);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

void GBA_DrawScanlineMode2(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT & BIT(10)) gba_bg2drawaffine(y);
    if(REG_DISPCNT & BIT(11)) gba_bg3drawaffine(y);
    if(REG_DISPCNT & BIT(12)) gba_sprites_draw_mode012(y);
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(2);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

void GBA_DrawScanlineMode3(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT&BIT(12)) gba_sprites_draw_mode345(y);
    if(REG_DISPCNT&BIT(10)) gba_bg2drawbitmapmode3(y); // BG2 enabled
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(3);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

void GBA_DrawScanlineMode4(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT&BIT(12)) gba_sprites_draw_mode345(y);
    if(REG_DISPCNT&BIT(10)) gba_bg2drawbitmapmode4(y); // BG2 enabled
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(4);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

void GBA_DrawScanlineMode5(s32 y)
{
    if(GBA_HasToSkipFrame()) return;

    gba_video_all_buffers_clear();

    //DRAW LAYERS
    u16 bd_col = *((u16*)Mem.pal_ram); int i; for(i = 0; i < 240; i++) backdrop[i] = bd_col;
    if(REG_DISPCNT&BIT(12)) gba_sprites_draw_mode345(y);
    if(REG_DISPCNT&BIT(10)) gba_bg2drawbitmapmode5(y); // BG2 enabled
    gba_window_apply(y,REG_DISPCNT & BIT(13),REG_DISPCNT & BIT(14),REG_DISPCNT & BIT(15));

    //MIX
    gba_sort_layers(5);
    gba_effects_apply();
    gba_blit_layers(y);
    return;
}

//-----------------------------------------------------------------------------------

inline void GBA_VideoUpdateRegister(u32 address)
{
    switch(address)
    {
        case BG2X_L: case BG2X_H:
            BG2lastx = REG_BG2X;
            if(BG2lastx&BIT(27)) BG2lastx |= 0xF0000000;
            break;
        case BG2Y_L: case BG2Y_H:
            BG2lasty = REG_BG2Y;
            if(BG2lasty&BIT(27)) BG2lasty |= 0xF0000000;
            break;

        case BG3X_L: case BG3X_H:
            BG3lastx = REG_BG3X;
            if(BG3lastx&BIT(27)) BG3lastx |= 0xF0000000;
            break;
        case BG3Y_L: case BG3Y_H:
            BG3lasty = REG_BG3Y;
            if(BG3lasty&BIT(27)) BG3lasty |= 0xF0000000;
            break;

        case WIN0H:
            Win0X1 = (REG_WIN0H>>8)&0xFF; Win0X2 = REG_WIN0H&0xFF;
            if(Win0X2 > 240) Win0X2 = 240; if(Win0X1 > Win0X2) Win0X2 = 240; //real bounds
            if(Win0X1 > 240) Win0X1 = 240;
            break;
        case WIN0V:
            Win0Y1 = (REG_WIN0V>>8)&0xFF; Win0Y2 = REG_WIN0V&0xFF;
            if(Win0Y2 > 160) Win0Y2 = 160; if(Win0Y1 > Win0Y2) Win0X2 = 160; //real bounds
            if(Win0Y1 > 160) Win0Y1 = 160;
            break;
        case WIN1H:
            Win1X1 = (REG_WIN1H>>8)&0xFF; Win1X2 = REG_WIN1H&0xFF;
            if(Win1X2 > 240) Win1X2 = 240; if(Win1X1 > Win1X2) Win1X2 = 240; //real bounds
            if(Win1X1 > 240) Win1X1 = 240;
            break;
        case WIN1V:
            Win1Y1 = (REG_WIN1V>>8)&0xFF; Win1Y2 = REG_WIN1V&0xFF;
            if(Win1Y2 > 160) Win1Y2 = 160; if(Win1Y1 > Win1Y2) Win1X2 = 160; //real bounds
            if(Win1Y1 > 160) Win1Y1 = 160;
            break;

        case MOSAIC:
        {
            int mos = REG_MOSAIC;
            MosBgX = (mos&0xF)+1;
            MosBgY = ((mos>>4)&0xF)+1;
            MosSprX = ((mos>>8)&0xF)+1;
            MosSprY = ((mos>>12)&0xF)+1;
            break;
        }

        default: break;
    }
}

//-----------------------------------------------------------------------------------

void GBA_ConvertScreenBufferTo32RGB(void * dst)
{
    u16 * src = screen_buffer_array[curr_screen_buffer^1];
    u32 * dest = (u32*)dst;
    int i;
    for(i = 0; i < 240*160; i++)
    {
        u32 data = (u32)*src++;
        *dest++ = ((data&0x1F)<<3)|((data&(0x1F<<5))<<6)|((data&(0x1F<<10))<<9)|(0xFF<<24);
    }
}

