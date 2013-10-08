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

#include <malloc.h>
#include <time.h>

#include "../build_options.h"

#include "gameboy.h"
#include "cpu.h"
#include "debug.h"
#include "memory.h"
#include "general.h"
#include "video.h"
#include "interrupts.h"

#include "gb_main.h"

extern _GB_CONTEXT_ GameBoy;

/*******************************************************
** Mode 2 -  80 clks \                                **
** Mode 3 - 172 clks |- 456 clks  -  144 times        **
** Mode 0 - 204 clks /                                **
**                                                    **
** Mode 3: 10 SPR - 296 clks || 0 SPR - 172 clks      **
** Mode 0: 10 SPR -  80 clks || 0 SPR - 204 clks      **
**                                                    **
** VBlank (Mode 1) lasts 4560 clks.                   **
**                                                    **
** A complete screen refresh occurs every 70224 clks. **
** 230230230230...11111111...230230230....1111111...  **
*******************************************************/


void GB_HandleTime(void)
{
	GameBoy.Emulator.FPS = GameBoy.Emulator.FrameCount;
	GameBoy.Emulator.FrameCount = 0;

	if(GameBoy.Emulator.HasTimer) //Handle time...
	{
		if(GameBoy.Emulator.Timer.halt == 0) //Increase timer...
		{
			GameBoy.Emulator.Timer.sec ++;
			if(GameBoy.Emulator.Timer.sec > 59)
			{
				GameBoy.Emulator.Timer.sec -= 60;

				GameBoy.Emulator.Timer.min ++;
				if(GameBoy.Emulator.Timer.min > 59)
				{
					GameBoy.Emulator.Timer.min -= 60;

					GameBoy.Emulator.Timer.hour ++;
					if(GameBoy.Emulator.Timer.hour > 23)
					{
						GameBoy.Emulator.Timer.hour -= 24;

						GameBoy.Emulator.Timer.days ++;
						if(GameBoy.Emulator.Timer.days > 511)
						{
							GameBoy.Emulator.Timer.days -= 512;

							GameBoy.Emulator.Timer.carry = 1;
						}
					}
				}
			}
		}
	}
}

void GB_CPUInterruptsInit(void)
{
//    GameBoy.Emulator.mode3len = 172;
//    GameBoy.Emulator.mode0len = 204;

	GameBoy.Emulator.FrameDrawn = 0;
	GameBoy.Emulator.FrameCount = 0;
	GameBoy.Emulator.Clocks = 0;
	GameBoy.Emulator.TimerClocks = 0;
	GameBoy.Emulator.timer_total_clocks = 0;
	GameBoy.Emulator.timer_enabled = 0;
	GameBoy.Emulator.DivClocks = 0;

	if(GameBoy.Emulator.HasTimer)
	{
		GameBoy.Emulator.LatchedTime.sec = 0;
		GameBoy.Emulator.LatchedTime.min = 0;
		GameBoy.Emulator.LatchedTime.hour = 0;
		GameBoy.Emulator.LatchedTime.days = 0;
		GameBoy.Emulator.LatchedTime.carry = 0; //GameBoy.Emulator.Timer is already loaded
		GameBoy.Emulator.LatchedTime.halt = 0; //GameBoy.Emulator.Timer.halt; //?
	}
}

void GB_CPUInterruptsEnd(void)
{
}

inline int GB_CPUHandleInterrupts(void)
{
	_GB_MEMORY_ * mem = &GameBoy.Memory;
	_GB_CPU_ * cpu = &GameBoy.CPU;
	int interrupts = mem->IO_Ports[IF_REG-0xFF00] & mem->HighRAM[IE_REG-0xFF80] & 0x1F;

	if(interrupts == 0) return 0;

	if(mem->InterruptMasterEnable == 0 || GameBoy.Emulator.CPUHalt) return 0;

	GameBoy.Memory.InterruptMasterEnable = 0;

	cpu->Reg16.SP -= 2;
	GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);

	if(interrupts & I_VBLANK)
	{
		cpu->Reg16.PC = 0x0040;
		mem->IO_Ports[IF_REG-0xFF00] &= ~I_VBLANK;
		return 1;
	}
	else if(interrupts & I_STAT)
	{
		cpu->Reg16.PC = 0x0048;
		mem->IO_Ports[IF_REG-0xFF00] &= ~I_STAT;
		return 1;
	}
	else if(interrupts & I_TIMER)
	{
		cpu->Reg16.PC = 0x0050;
		mem->IO_Ports[IF_REG-0xFF00] &= ~I_TIMER;
		return 1;
	}
	else if(interrupts & I_SERIAL)
	{
		cpu->Reg16.PC = 0x0058;
		mem->IO_Ports[IF_REG-0xFF00] &= ~I_SERIAL;
		return 1;
	}
	else //if(interrupts & I_JOYPAD)
	{
		cpu->Reg16.PC = 0x0060;
		mem->IO_Ports[IF_REG-0xFF00] &= ~I_JOYPAD;
		return 1;
	}
}

inline void GB_SetInterrupt(int flag)
{
	GameBoy.Memory.IO_Ports[IF_REG-0xFF00] |= flag;
	if(GameBoy.Memory.HighRAM[IE_REG-0xFF80] & flag)
        GameBoy.Emulator.CPUHalt = 0;
}

inline void GB_CheckStatSignal(void)
{
	if(GameBoy.Emulator.lcd_on == 0)
	{
		GameBoy.Emulator.stat_signal = 0;
		return;
	}

	_GB_MEMORY_ * mem = &GameBoy.Memory;
	u32 screenmode = GameBoy.Emulator.ScreenMode;
	int stat = mem->IO_Ports[STAT_REG-0xFF00];

	if(    (mem->IO_Ports[LY_REG-0xFF00] == mem->IO_Ports[LYC_REG-0xFF00] &&
												stat & IENABLE_LY_COMPARE)  ||
		(stat & IENABLE_HBL && screenmode == 0) ||
		(stat & IENABLE_OAM && screenmode == 2)  ||
		(stat & (IENABLE_VBL|IENABLE_OAM) && screenmode == 1)
	//	(stat & IENABLE_VBL && screenmode == 1)
	)

	{
		if(GameBoy.Emulator.stat_signal == 0 /*&& mem->HighRAM[IE_REG-0xFF80] & I_STAT*/)
			GB_SetInterrupt(I_STAT);

		GameBoy.Emulator.stat_signal = 1;

		return;
	}

	GameBoy.Emulator.stat_signal = 0;
}

inline void GB_CheckLYC(void)
{
	if(GameBoy.Memory.IO_Ports[LY_REG-0xFF00] == GameBoy.Memory.IO_Ports[LYC_REG-0xFF00])
		GameBoy.Memory.IO_Ports[STAT_REG-0xFF00] |= I_LY_EQUALS_LYC;
	else
		GameBoy.Memory.IO_Ports[STAT_REG-0xFF00] &= ~I_LY_EQUALS_LYC;
}
/*
inline int GB_SpriteCount(int scanline)
{
    if((GameBoy.Memory.IO_Ports[LCDC_REG-0xFF00] & (1<<1)) == 0) return 0;

    _GB_OAM_ * GB_OAM = (_GB_OAM_*)GameBoy.Memory.ObjAttrMem;

    int count = 0;
    int spriteheight =  8 << ((GameBoy.Memory.IO_Ports[LCDC_REG-0xFF00] & (1<<2)) != 0);

    int i;
    for(i = 0; i < 40; i ++)
    {
        int real_y = (GB_OAM->Sprite[i].Y-16);
        if( (real_y <= scanline) && ((real_y+spriteheight) > scanline) )
        {
            count++;
            if(count == 10) return 10; //can't draw more than 10 sprites...
        }
    }

    return count;
}


inline void GB_Mode03UpdateLenght(void)
{
    int clocks = 0; //GB_SpriteCount(GameBoy.Emulator.CurrentScanLine) * 12;

    GameBoy.Emulator.mode3len = 172 + clocks;
    GameBoy.Emulator.mode0len = 204 - clocks;
}
*/

inline void GB_CPUHandleClock(void)
{
	_GB_MEMORY_ * mem = &GameBoy.Memory;

	//SCREEN
	switch(GameBoy.Emulator.ScreenMode)
	{
		case 2:
			if(GameBoy.Emulator.Clocks > 79)
			{
				GameBoy.Emulator.Clocks -= 80;

				GameBoy.Emulator.ScreenMode = 3;
			//	mem->IO_Ports[STAT_REG-0xFF00] &= 0xFC;
				mem->IO_Ports[STAT_REG-0xFF00] |= 0x03;

				GB_CheckStatSignal();
			}
			break;
		case 3:
			if(GameBoy.Emulator.Clocks > 171) //(GameBoy.Emulator.mode3len-1))
			{
				GameBoy.Emulator.Clocks -= 172; //GameBoy.Emulator.mode3len;

				GameBoy.Emulator.DrawScanlineFn(GameBoy.Emulator.CurrentScanLine);

				GameBoy.Emulator.ScreenMode = 0;
				mem->IO_Ports[STAT_REG-0xFF00] &= 0xFC;

				GB_CheckStatSignal();
			}
			break;
		case 0:
			if(GameBoy.Emulator.Clocks > 203) //(GameBoy.Emulator.mode0len-1))
			{
				GameBoy.Emulator.Clocks -= 204; //GameBoy.Emulator.mode0len;

				GameBoy.Emulator.CurrentScanLine ++;

				mem->IO_Ports[LY_REG-0xFF00] = GameBoy.Emulator.CurrentScanLine;

				GB_CheckLYC();

				if(GameBoy.Emulator.CurrentScanLine == 144)
				{
					GameBoy.Emulator.ScreenMode = 1;
					mem->IO_Ports[STAT_REG-0xFF00] &= 0xFC;
					mem->IO_Ports[STAT_REG-0xFF00] |= 0x01;

					//Call VBL interrupt...
					if(GameBoy.Emulator.lcd_on)
					{
					//	if(mem->HighRAM[IE_REG-0xFF80] & I_VBLANK)
						//	GB_SetInterrupt(I_VBLANK);
							GameBoy.Emulator.VBL_clocks_delay = 24;
							//check in GB_CPUClock()
					}
				}
				else
				{
				    //GB_Mode03UpdateLenght();
					GameBoy.Emulator.ScreenMode = 2;
					mem->IO_Ports[STAT_REG-0xFF00] &= 0xFC;
					mem->IO_Ports[STAT_REG-0xFF00] |= 0x02;
				}

				GB_CheckStatSignal();
			}
			break;
		case 1:
			if(GameBoy.Emulator.Clocks > 455)
			{
				GameBoy.Emulator.Clocks -= 456;

				if(GameBoy.Emulator.CurrentScanLine == 0)
				{
					GameBoy.Emulator.ScreenMode = 2;
					mem->IO_Ports[STAT_REG-0xFF00] &= 0xFC;
					mem->IO_Ports[STAT_REG-0xFF00] |= 0x02;
                    //GB_Mode03UpdateLenght();
					GB_CheckStatSignal();
					break;
				}

				GameBoy.Emulator.CurrentScanLine ++;

				if(GameBoy.Emulator.CurrentScanLine == 153)
				{
					GameBoy.Emulator.Clocks += 456 - 8; // 8 clocks this scanline
				}
				else if(GameBoy.Emulator.CurrentScanLine == 154)
				{
					GameBoy.Emulator.CurrentScanLine = 0;
					GameBoy.Emulator.FrameDrawn = 1;
					GameBoy.Emulator.FrameCount ++;

					GameBoy.Emulator.Clocks += 8; // 456 - 8 cycles left of vblank...
				}

				mem->IO_Ports[LY_REG-0xFF00] = GameBoy.Emulator.CurrentScanLine;

				GB_CheckLYC();

				GB_CheckStatSignal();
			}
			break;
	}

	//TIMERS

	//DIV -- EVERY 256 CLOCKS
	while(GameBoy.Emulator.DivClocks > 255)
	{
		mem->IO_Ports[DIV_REG-0xFF00] ++;
		GameBoy.Emulator.DivClocks -= 256;
	}

	//TIMA
	if(GameBoy.Emulator.timer_enabled)
	{
    	while(GameBoy.Emulator.TimerClocks > (GameBoy.Emulator.timer_total_clocks-1))
    	{
    		if(mem->IO_Ports[TIMA_REG-0xFF00] == 0xFF) //overflow
    		{
    			//if(mem->HighRAM[IE_REG-0xFF80] & I_TIMER)
    				GB_SetInterrupt(I_TIMER);
    			mem->IO_Ports[TIMA_REG-0xFF00] = mem->IO_Ports[TMA_REG-0xFF00];
    		}
    		else mem->IO_Ports[TIMA_REG-0xFF00]++;

    		GameBoy.Emulator.TimerClocks -= GameBoy.Emulator.timer_total_clocks;
    	}
    }

	//JOYPAD
	if( (GameBoy.Emulator.HardwareType == HW_GBC) || (GameBoy.Emulator.HardwareType == HW_GBA) )
		return; //No joypad interrupt in GBA/GBC

	//if((mem->HighRAM[IE_REG-0xFF80] & I_JOYPAD) == 0) return;

	u32 i = mem->IO_Ports[P1_REG-0xFF00];
	u32 result = 0;

    int Keys = GB_Input_Get(0);
	if((i & (1<<5)) == 0) //A-B-SEL-STA
		result |= Keys & (KEY_A|KEY_B|KEY_SELECT|KEY_START);
	if((i & (1<<4)) == 0) //PAD
		result |= Keys & (KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT);

	if(result) GB_SetInterrupt(I_JOYPAD);

	return;
}

