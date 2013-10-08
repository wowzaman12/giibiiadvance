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

#ifndef __GBA_VIDEO__
#define __GBA_VIDEO__

#include "gba.h"

inline void GBA_SetFrameskip(int value);
inline void GBA_UpdateFrameskip(void);
inline int GBA_HasToSkipFrame(void);

//---------------------------------------------------------

void GBA_FillFadeTables(void);

//---------------------------------------------------------

void GBA_UpdateDrawScanlineFn(void);

inline void GBA_VideoUpdateRegister(u32 address);

inline void GBA_DrawScanline(s32 y);
inline void GBA_DrawScanlineWhite(s32 y);

void GBA_ConvertScreenBufferTo32RGB(void * dst);

#endif //__GBA_VIDEO__

