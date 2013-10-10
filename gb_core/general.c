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

#include <string.h>

#include "../build_options.h"

#include "gameboy.h"
#include "cpu.h"
#include "memory.h"
#include "sound.h"
#include "interrupts.h"
#include "sgb.h"
#include "serial.h"
#include "video.h"
#include "gb_main.h"

#include "../frontend/windows/main.h"
#include "../frontend/windows/config.h"

_GB_CONTEXT_ GameBoy;

void GB_PowerOn(void)
{
	GB_CPUInit();
	GB_MemInit();
	GB_SoundInit();
	GB_Screen_Init();
	GB_SerialInit();

	if(GameBoy.Emulator.SGBEnabled) SGB_Init();

	if(GameBoy.Emulator.MemoryController == MEM_CAMERA)
	{
	    if(GB_CameraInit(EmulatorConfig.debug_msg_enable) == 0)
            DebugMessage("Camera functions won't be emulated... How about some screen noise instead? :)");
	}
}

void GB_PowerOff(void)
{
    if(GameBoy.Emulator.MemoryController == MEM_CAMERA) GB_CameraEnd();

	if(GameBoy.Emulator.SGBEnabled) SGB_End();

	GB_SerialEnd();
	GB_SoundEnd();
	GB_CPUInterruptsEnd();
}

void GB_HardReset(void)
{
    GB_PowerOff();

    GameBoy.Emulator.FrameDrawn = 1;

	GB_Screen_Init();

    if(GameBoy.Emulator.boot_rom_loaded)
    {
        GameBoy.Emulator.enable_boot_rom = 1;

        if(GameBoy.Emulator.gbc_in_gb_mode)
        {
            GameBoy.Emulator.gbc_in_gb_mode = 0;
            GameBoy.Emulator.CGBEnabled = 1;
            GameBoy.Emulator.DrawScanlineFn = &GBC_ScreenDrawScanline;
        }
    }

    GB_PowerOn();
}


