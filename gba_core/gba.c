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

#include "../build_options.h"

#include "gba.h"
#include "cpu.h"
#include "bios.h"
#include "memory.h"
#include "interrupts.h"
#include "video.h"
#include "dma.h"
#include "timers.h"
#include "save.h"
#include "rom.h"
#include "sound.h"

static s32 clocks;

static int inited = 0;

static s32 lastresidualclocks = 0;

int GBA_ROM_SIZE;
inline int GBA_GetRomSize(void)
{
    return GBA_ROM_SIZE;
}

int GBA_InitRom(void * bios_ptr, void * rom_ptr, u32 romsize)
{
    if(inited) GBA_EndRom(1); //shouldn't be needed here

    if(romsize > 0x02000000)
    {
        ErrorMessage("Rom too big!\nSize = 0x%08X bytes\nMax = 0x02000000 bytes",romsize);
        GBA_ROM_SIZE = 0x02000000;
    }
    else
    {
        GBA_ROM_SIZE = romsize;
    }

    GBA_DetectSaveType(rom_ptr,romsize);
    GBA_ResetSaveBuffer();
    GBA_SaveReadFile();

    GBA_HeaderCheck(rom_ptr);

    GBA_CPUInit();
    GBA_InterruptInit();
    GBA_TimerInitAll();
    GBA_MemoryInit(bios_ptr,rom_ptr,GBA_ROM_SIZE);
    GBA_UpdateDrawScanlineFn();
    GBA_DMA0Setup();
    GBA_DMA1Setup();
    GBA_DMA2Setup();
    GBA_DMA3Setup();
    GBA_SoundInit();
    GBA_FillFadeTables();

    clocks = 1;
    lastresidualclocks = 0;

    inited = 1;

    return 1;
}

int GBA_EndRom(int save)
{
    if(inited == 0) return 0;

    if(save) GBA_SaveWriteFile();

    GBA_MemoryEnd();

    inited = 0;

    return 1;
}

void GBA_Reset(void)
{
    GBA_CPUClearHalted();
    GBA_Swi(0x26);
    //it will be enough for now
}


#ifdef min
#undef min
#endif

static inline s32 min(s32 a, s32 b)
{
    return ((a < b) ? a : b);
}

u32 GBA_RunFor(s32 totalclocks)
{
    s32 residualclocks, executedclocks;
    totalclocks += lastresidualclocks;
    u32 has_executed = 0;

    while(totalclocks >= clocks)
    {
        if(GBA_DMAisWorking())
        {
            executedclocks = clocks - GBA_DMAGetExtraClocksElapsed();
        }
        else
        {
            has_executed = 1;

            if(GBA_InterruptCheck())
            {
                executedclocks = GBA_MemoryGetAccessCycles(0,1,CPU.OldPC) + GBA_MemoryGetAccessCyclesNoSeq(1,CPU.R[R_PC]) +
                        GBA_MemoryGetAccessCyclesSeq(1,CPU.R[R_PC]); //2S + 1N cycles ?
            }
            else
            {
                residualclocks = GBA_Execute(clocks);
                executedclocks = clocks-residualclocks;
                /**
                 * Not entirely sure if this is safe, but for now it works
                 * will investigate later
                 **/

                /*if(RUNNING != RUN_GBA) return 0;*/
            }
        }

        clocks = GBA_UpdateScreenTimings(executedclocks);
        clocks = min(GBA_DMAUpdate(executedclocks),clocks);
        clocks = min(GBA_TimersUpdate(executedclocks),clocks);
        clocks = min(GBA_SoundUpdate(executedclocks),clocks);
        //mirar si otros eventos van a suceder antes y cambiar clocks por eso
        totalclocks -= executedclocks;
    }

    while(totalclocks > 0)
    {
        if(GBA_DMAisWorking())
        {
            executedclocks = totalclocks - GBA_DMAGetExtraClocksElapsed();
            totalclocks = GBA_DMAGetExtraClocksElapsed();
        }
        else
        {
            has_executed = 1;

            if(GBA_InterruptCheck())
            {
                executedclocks = GBA_MemoryGetAccessCycles(0,1,CPU.OldPC) + GBA_MemoryGetAccessCyclesNoSeq(1,CPU.R[R_PC]) +
                        GBA_MemoryGetAccessCyclesSeq(1,CPU.R[R_PC]); //2S + 1N cycles ?
                residualclocks = totalclocks - executedclocks;
            }
            else
            {
                residualclocks = GBA_Execute(totalclocks);
                executedclocks = totalclocks-residualclocks;
                /**
                 * Not entirely sure if this is safe, but for now it works
                 * will investigate later
                 **/
                /*if(RUNNING != RUN_GBA) return 0;*/
            }
            totalclocks = residualclocks;
        }

        clocks = GBA_UpdateScreenTimings(executedclocks);
        clocks = min(GBA_DMAUpdate(executedclocks),clocks);
        clocks = min(GBA_TimersUpdate(executedclocks),clocks);
        clocks = min(GBA_SoundUpdate(executedclocks),clocks);
        //mirar si otros eventos van a suceder antes y cambiar clocks por eso
    }

    lastresidualclocks = totalclocks;

    return has_executed;

        /*
        //code that executes only one instruction
    s32 executedclocks;

    if(GBA_DMAisWorking())
    {
        executedclocks = 1;
        GBA_DMAUpdate(executedclocks);
        GBA_UpdateScreenTimings(executedclocks);
        return 0;
    }
    else
    {
        if(GBA_InterruptCheck())
        {
            executedclocks = GBA_MemoryGetAccessCycles(0,1,CPU.OldPC) + GBA_MemoryGetAccessCyclesNoSeq(1,CPU.R[R_PC]) +
                    GBA_MemoryGetAccessCyclesSeq(1,CPU.R[R_PC]); //2S + 1N cycles ?
        }
        else
        {
            s32 residualclocks = GBA_Execute(1);
            executedclocks = 1-residualclocks;
        }
        GBA_UpdateScreenTimings(executedclocks);
        return 1;
    }

*/
}

void GBA_DebugStep(void)
{
    int clockstostop = 1000000;
    do
    {
        while(!GBA_RunFor(1)) if(clockstostop-- <= 0) return;
        //if infinite loop, break. safety always first :P
    }
    while( /*(CPU.R[R_PC] < 0x02000000) ||*/ (GBA_CPUGetHalted()) ); //skip any wait period

    //GLWindow_MemViewerUpdate();
    //GLWindow_IOViewerUpdate();
    //GLWindow_DisassemblerUpdate();
}

