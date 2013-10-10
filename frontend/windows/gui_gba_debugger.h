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

#ifndef __GUI_GBA_DEBUGGER__
#define __GUI_GBA_DEBUGGER__

inline void GLWindow_GBADisassemblerStartAddressSetDefault(void);
void GLWindow_GBACreateDissasembler(void);
void GLWindow_GBADisassemblerUpdate(void);
void GLWindow_GBADisassemblerStep(void);
void GLWindow_GBACloseDissasembler(void);

void GLWindow_GBACreateMemViewer(void);
void GLWindow_GBAMemViewerUpdate(void);
void GLWindow_GBACloseMemViewer(void);

void GLWindow_GBACreateIOViewer(void);
void GLWindow_GBAIOViewerUpdate(void);
void GLWindow_GBACloseIOViewer(void);

#endif //__GUI_GBA_DEBUGGER__

