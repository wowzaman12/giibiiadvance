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

#ifndef __GB_MAIN__
#define __GB_MAIN__

void GB_Input_Update(void);

int GB_MainLoad(const char * rom_path);
void GB_End(int save);

int GB_Screen_Init(void);

void GB_Screen_WriteBuffer(void);

int GB_Input_Get(int player);

void GB_Screenshot(void);

void menu_get_gb_palette(u8 * red, u8 * green, u8 * blue);
void menu_set_gb_palette(u8 red, u8 green, u8 blue);
void menu_load_gb_palete_from_config(void);

#endif //__GB_MAIN__
