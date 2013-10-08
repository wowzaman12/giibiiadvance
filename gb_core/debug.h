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

#ifndef __DEBUG__
#define __DEBUG__

inline int gb_debug_get_address_increment(u32 address);
inline int gb_debug_get_address_is_code(u32 address);
char * GB_Dissasemble(u16 addr, int * step);

void GB_Debug_Get_Palette(int is_sprite, int num, int color, u32 * red, u32 * green, u32 * blue);

#endif //__DEBUG__

