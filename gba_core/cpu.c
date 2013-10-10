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

#include "gba.h"
#include "cpu.h"
#include "memory.h"
#include "../frontend/windows/config.h"

//-----------------------------------------------------------------------------------

_cpu_t CPU;
u32 cpu_loop_break = 0;

void GBA_CPUInit(void)
{
    memset(&CPU,0,sizeof(CPU));
    CPU.EXECUTION_MODE = EXEC_ARM;

    if(EmulatorConfig.load_from_boot_rom) //<- TO INIT FROM BIOS
    {
        CPU.R[R_SP] = 0x03007F00;
        CPU.R[R_LR] = 0x08000000;

        CPU.R[R_PC] = 0x00000000;
        CPU.CPSR = M_SUPERVISOR | F_IRQ_DIS | F_FIQ_DIS;
        CPU.MODE = CPU_SUPERVISOR;
    }
    else
    {
        CPU.R[R_SP] = 0x03007F00;
        CPU.R[R_LR] = 0x08000000;
        CPU.R[R_PC] = 0x08000000;
        CPU.CPSR = M_SYSTEM;
        CPU.MODE = CPU_SYSTEM;
    }

    //Default stack pointers (Set by bios)
    CPU.R_user[13-8] = 0x03007F00;
    CPU.R13_svc = 0x03007FE0;
    CPU.R13_irq = 0x03007FA0;

    CPU.OldPC = 0;

    REG_IME = 0;

    cpu_loop_break = 0;

    GBA_CPUClearHalted();
}

// USER       - 0x10
// FIQ        - 0x11
// IRQ        - 0x12
// SUPERVISOR - 0x13
// ABORT      - 0x17
// UNDEFINED  - 0x1B
// SYSTEM     - 0x1F

void GBA_CPUChangeMode(u32 value)
{
    static const u32 mode_value[CPU_MODE_NUMBER] = {
        M_USER,M_FIQ,M_IRQ,M_SUPERVISOR,M_ABORT,M_UNDEFINED,M_SYSTEM
    };

    if(mode_value[CPU.MODE] == value) return; //same mode

    //CPU.SPSR = CPU.CPSR;

    switch(CPU.MODE) //save current registers
    {
        case CPU_USER: case CPU_SYSTEM:
        {
            int i;
            for(i = 0; i < 7; i++) CPU.R_user[i] = CPU.R[i+8];
            break;
        }
        case CPU_FIQ:
        {
            int i;
            for(i = 0; i < 7; i++) CPU.R_fiq[i] = CPU.R[i+8];
            CPU.SPSR_fiq = CPU.SPSR;
            break;
        }
        case CPU_SUPERVISOR:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R_user[i] = CPU.R[i+8];
            CPU.R13_svc = CPU.R[13];
            CPU.R14_svc = CPU.R[14];
            CPU.SPSR_svc = CPU.SPSR;
            break;
        }
        case CPU_ABORT:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R_user[i] = CPU.R[i+8];
            CPU.R13_abt = CPU.R[13];
            CPU.R14_abt = CPU.R[14];
            CPU.SPSR_abt = CPU.SPSR;
            break;
        }
        case CPU_IRQ:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R_user[i] = CPU.R[i+8];
            CPU.R13_irq = CPU.R[13];
            CPU.R14_irq = CPU.R[14];
            CPU.SPSR_irq = CPU.SPSR;
            break;
        }
        case CPU_UNDEFINED:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R_user[i] = CPU.R[i+8];
            CPU.R13_und = CPU.R[13];
            CPU.R14_und = CPU.R[14];
            CPU.SPSR_und = CPU.SPSR;
            break;
        }
        default:
            return;
    }

    if(value == mode_value[CPU_USER]) CPU.MODE = CPU_USER;
    else if(value == mode_value[CPU_FIQ]) CPU.MODE = CPU_FIQ;
    else if(value == mode_value[CPU_IRQ]) CPU.MODE = CPU_IRQ;
    else if(value == mode_value[CPU_SUPERVISOR]) CPU.MODE = CPU_SUPERVISOR;
    else if(value == mode_value[CPU_ABORT]) CPU.MODE = CPU_ABORT;
    else if(value == mode_value[CPU_UNDEFINED]) CPU.MODE = CPU_UNDEFINED;
    else if(value == mode_value[CPU_SYSTEM]) CPU.MODE = CPU_SYSTEM;
    else
    {
        DebugMessage("Trying to change to CPU mode 0x%02X (invalid).",value);
        GBA_ExecutionBreak();
        return; //ERROR
    }

    switch(CPU.MODE) //load new registers
    {
        case CPU_USER: case CPU_SYSTEM:
        {
            int i;
            for(i = 0; i < 7; i++) CPU.R[i+8] = CPU.R_user[i];
            break;
        }
        case CPU_FIQ:
        {
            int i;
            for(i = 0; i < 7; i++) CPU.R[i+8] = CPU.R_fiq[i];
            CPU.SPSR = CPU.SPSR_fiq;
            break;
        }
        case CPU_SUPERVISOR:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R[i+8] = CPU.R_user[i];
            CPU.R[13] = CPU.R13_svc;
            CPU.R[14] = CPU.R14_svc;
            CPU.SPSR = CPU.SPSR_svc;
            break;
        }
        case CPU_ABORT:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R[i+8] = CPU.R_user[i];
            CPU.R[13] = CPU.R13_abt;
            CPU.R[14] = CPU.R14_abt;
            CPU.SPSR = CPU.SPSR_abt;
            break;
        }
        case CPU_IRQ:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R[i+8] = CPU.R_user[i];
            CPU.R[13] = CPU.R13_irq;
            CPU.R[14] = CPU.R14_irq;
            CPU.SPSR = CPU.SPSR_irq;
            break;
        }
        case CPU_UNDEFINED:
        {
            int i;
            for(i = 0; i < 5; i++) CPU.R[i+8] = CPU.R_user[i];
            CPU.R[13] = CPU.R13_und;
            CPU.R[14] = CPU.R14_und;
            CPU.SPSR = CPU.SPSR_und;
            break;
        }
        default:
            return;
    }

    return;
}

static s32 halt;

inline void GBA_CPUSetHalted(s32 value)
{
    if(REG_IE) halt = 1+(value>>7); //don't halt if IE = 0 or it will never exit from halt
    //else
    //{
    //    DebugMessage("STOP/HALT mode with IE=0: infinite loop.\nSTOP/HALT ignored.");
    //    GBA_ExecutionBreak();
    //}
    //halt = 1 => halt
    //halt = 2 => stop
}

inline s32 GBA_CPUGetHalted(void)
{
    return halt;
}

inline void GBA_CPUClearHalted(void)
{
    halt = 0;
}

inline s32 GBA_Execute(s32 clocks)
{
    if(GBA_CPUGetHalted()) return 0;

    if(CPU.EXECUTION_MODE == EXEC_ARM) return GBA_ExecuteARM(clocks);
    else return GBA_ExecuteTHUMB(clocks);
}

inline void GBA_ExecutionBreak(void)
{
    cpu_loop_break = 1;
}
