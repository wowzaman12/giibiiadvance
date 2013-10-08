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

#include "../build_options.h"

#include "gba.h"
#include "memory.h"
#include "video.h"
#include "cpu.h"
#include "bios.h"

#define SCR_DRAW      (0)
#define SCR_HBL       (1)
#define SCR_VBL_DRAW  (2)
#define SCR_VBL_HBL   (3)
u32 screenmode = SCR_DRAW;

#define HDRAW_CLOCKS (960)
#define HBL_CLOCKS   (272)
//#define HLINE_CLOCKS (1232)
//#define VBL_CLOCKS   (83776) //68*HLINE_CLOCKS
s32 scrclocks = HDRAW_CLOCKS;

u32 ly = 0;

inline void GBA_CallInterrupt(u32 flag)
{
    REG_IF |= flag;
}

inline int GBA_InterruptCheck(void)
{
    if(GBA_CPUGetHalted() == 2) return 0;

    u32 IRQ_ENABLED = (REG_IME&1) && ((CPU.CPSR&F_IRQ_DIS) == 0);

    if(REG_IE&REG_IF)
    {
        GBA_CPUClearHalted();

        if(IRQ_ENABLED)
        {
            GBA_CPUChangeMode(M_IRQ);
            CPU.R[R_LR] = CPU.R[R_PC]+4;//(CPU.EXECUTION_MODE==EXEC_ARM?4:2); // save return address
            CPU.SPSR = CPU.CPSR; //save CPSR flags
            CPU.CPSR &= ~(0x1F|F_T|F_I); //Enter IRQ mode, ARM state, IRQs disabled
            CPU.CPSR |= M_IRQ|F_I;
            CPU.R[R_PC] = 0x18; //jump to IRQ vector address
            CPU.EXECUTION_MODE = EXEC_ARM;
            return 1;
        }
    }

    return 0;
}

inline void GBA_CheckKeypadInterrupt(void)
{
    u16 keys = REG_KEYCNT & 0x03FF;
    u16 keyspressed = (~REG_KEYINPUT) & 0x03FF;

    if(REG_KEYCNT&BIT(15)) //AND
    {
        if((keys&keyspressed) == keys)
        {
            if(REG_KEYCNT&BIT(14)) GBA_CallInterrupt(BIT(12)); //IRQ Enable Flag
            if(GBA_CPUGetHalted()==2) GBA_CPUClearHalted();
        }
    }
    else //OR
    {
        if(keys&keyspressed)
        {
            if(REG_KEYCNT&BIT(14)) GBA_CallInterrupt(BIT(12)); //IRQ Enable Flag
            if(GBA_CPUGetHalted()==2) GBA_CPUClearHalted();
        }
    }
}

inline void GBA_InterruptLCD(u32 flag)
{
    if(REG_DISPSTAT&flag) GBA_CallInterrupt(flag>>3);
}

int justchangedscreenmode = 0;
inline int GBA_ScreenJustChangedMode(void)
{
    return justchangedscreenmode;
}

s32 GBA_UpdateScreenTimings(s32 clocks)
{
    static int hblinterruptexecuted = 0;

    scrclocks -= clocks;
    justchangedscreenmode = 0;
    switch(screenmode)
    {
        case SCR_DRAW:
        {
            if(scrclocks <= 0)
            {
                //Check if forced blank and draw
                if(REG_DISPCNT&BIT(7)) GBA_DrawScanlineWhite(ly);
                else GBA_DrawScanline(ly);

                GBA_InterruptLCD(BIT(4)); // Does this go here?
                //Although the drawing time is only 960 cycles (240*4), the H-Blank flag is "0" for a total of 1006 cycles.
                screenmode = SCR_HBL;
                scrclocks = HBL_CLOCKS+scrclocks;
                justchangedscreenmode = 1;
                hblinterruptexecuted = 0;
            }
            break;
        }
        case SCR_HBL:
        {
            if(scrclocks <= 0)
            {
                ly++;
                REG_VCOUNT = ly;

                if(ly == 160)
                {
                    REG_DISPSTAT |= BIT(0);
                    GBA_InterruptLCD(BIT(3));
                    screenmode = SCR_VBL_DRAW;
                    scrclocks = HDRAW_CLOCKS+scrclocks;
                }
                else
                {
                    REG_DISPSTAT &= ~BIT(1);
                    screenmode = SCR_DRAW;
                    scrclocks = HDRAW_CLOCKS+scrclocks;
                }

                if((REG_DISPSTAT>>8) == ly) { REG_DISPSTAT |= BIT(2); GBA_InterruptLCD(BIT(5)); }
                else REG_DISPSTAT &= ~BIT(2);

                justchangedscreenmode = 1;
            }
            else if( (scrclocks <= (HBL_CLOCKS-(1006-HDRAW_CLOCKS))) && (hblinterruptexecuted == 0) )
            {
                REG_DISPSTAT |= BIT(1);
                //GBA_InterruptLCD(BIT(4));
                hblinterruptexecuted = 1;
            }
            break;
        }
        case SCR_VBL_DRAW:
        {
            if(scrclocks <= 0)
            {
                GBA_InterruptLCD(BIT(4)); // Does this go here?
                //Although the drawing time is only 960 cycles (240*4), the H-Blank flag is "0" for a total of 1006 cycles.
                screenmode = SCR_VBL_HBL;
                scrclocks = HBL_CLOCKS+scrclocks;
                hblinterruptexecuted = 0;
            }
            break;
        }
        case SCR_VBL_HBL:
        {
            if(scrclocks <= 0)
            {
                ly++;

                if(ly == 227)
                {
                    REG_DISPSTAT &= ~(BIT(0)|BIT(1));
                    REG_VCOUNT = ly;
                    screenmode = SCR_VBL_DRAW;
                    scrclocks = HDRAW_CLOCKS+scrclocks;
                }
                else if(ly == 228)
                {
                    ly = 0;
                    REG_VCOUNT = 0;
                    screenmode = SCR_DRAW;
                    scrclocks = HDRAW_CLOCKS+scrclocks;
                }
                else
                {
                    REG_DISPSTAT &= ~BIT(1);
                    REG_VCOUNT = ly;
                    screenmode = SCR_VBL_DRAW;
                    scrclocks = HDRAW_CLOCKS+scrclocks;
                }

                if((REG_DISPSTAT>>8) == ly) { REG_DISPSTAT |= BIT(2); GBA_InterruptLCD(BIT(5)); }
                else REG_DISPSTAT &= ~BIT(2);
            }
            else if( (scrclocks <= (HBL_CLOCKS-(1006-HDRAW_CLOCKS))) && (hblinterruptexecuted == 0) )
            {
                REG_DISPSTAT |= BIT(1);
                //GBA_InterruptLCD(BIT(4));
                hblinterruptexecuted = 1;
            }
            break;
        }
        default: //???
            break;
    }

    return scrclocks;
}

void GBA_InterruptInit(void)
{
    scrclocks = HDRAW_CLOCKS;
    screenmode = SCR_DRAW;
    ly = 0;
    justchangedscreenmode = 0;
}


