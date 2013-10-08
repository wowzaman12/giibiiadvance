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


#include "../build_options.h"

#include "gameboy.h"
#include "cpu.h"
#include "debug.h"
#include "memory.h"
#include "interrupts.h"
#include "sound.h"
#include "general.h"
#include "sgb.h"
#include "serial.h"

#include "../main.h"
#include "gb_main.h"

extern _GB_CONTEXT_ GameBoy;

/*
Sprite RAM Bug
--------------

There is a flaw in the GameBoy hardware that causes trash to be written to OAM
RAM if the following commands are used while their 16-bit content is in the
range of $FE00 to $FEFF:
  inc rr        dec rr          ;rr = bc,de, or hl
  ldi a,(hl)    ldd a,(hl)
  ldi (hl),a    ldd (hl),a
Only sprites 1 & 2 ($FE00 & $FE04) are not affected by these instructions.

*/

//Clocks in normal GB mode. If 0, clocks are increased in the switch (or undefined opcode).
const u32 clocks_table[256] = {
	4,12, 8, 8, 4, 4, 8, 4, 20, 8, 8, 8, 4, 4, 8, 4,
	4,12, 8, 8, 4, 4, 8, 4, 12, 8, 8, 8, 4, 4, 8, 4,
	0,12, 8, 8, 4, 4, 8, 4,  0, 8, 8, 8, 4, 4, 8, 4,
	0,12, 8, 8,12,12,12, 4,  0, 8, 8, 8, 4, 4, 8, 4,

	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	8, 8, 8, 8, 8, 8, 4, 8,  4, 4, 4, 4, 4, 4, 8, 4,

	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,
	4, 4, 4, 4, 4, 4, 8, 4,  4, 4, 4, 4, 4, 4, 8, 4,

	 0,12, 0,16, 0,16, 8,16,  0,16, 0, 0, 0,24, 8,16,
	 0,12,12, 0, 0,16, 8,16,  0,16, 0, 0, 0, 0, 8,16,
	12,12, 8, 0, 0,16, 8,16, 16, 4,16, 0, 0, 0, 8,16,
	12,12, 8, 4, 0,16, 8,16, 12, 8,16, 4, 0, 0, 8,16
	} ;

const u32 clocks_table_cb[256] = {
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,

	8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8,
	8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8,
	8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8,
	8, 8, 8, 8, 8, 8, 12, 8, 8, 8, 8, 8, 8, 8, 12, 8,

	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,

	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8,
	8, 8, 8, 8, 8, 8, 16, 8, 8, 8, 8, 8, 8, 8, 16, 8
	} ;

void GB_CPUInit(void)
{
	GB_CPUInterruptsInit();

	GameBoy.Emulator.ScreenMode = 2;
	GameBoy.Memory.IO_Ports[STAT_REG-0xFF00] = 0x02;
	GameBoy.Emulator.CurrentScanLine = 0;
	GameBoy.Emulator.CPUHalt = 0;
	GameBoy.Emulator.DoubleSpeed = 0;
	GameBoy.Emulator.halt_not_executed = 0;

	if(GameBoy.Emulator.CGBEnabled == 1)
	{
		GameBoy.Emulator.ScreenMode = 1;
		GameBoy.Memory.IO_Ports[STAT_REG-0xFF00] = 0x01;
		GameBoy.Emulator.CurrentScanLine = 0x90;
	}

	//Registers
	if(GameBoy.Emulator.boot_rom_loaded == 0)
	{
        GameBoy.CPU.Reg8.F = 0xB0;

        if(GameBoy.Emulator.CGBEnabled)
        {
            GameBoy.CPU.Reg16.BC = 0x0000;
            GameBoy.CPU.Reg16.DE = 0xFF56;
            GameBoy.CPU.Reg16.HL = 0x000D;
        }
        else
        {
            GameBoy.CPU.Reg16.BC = 0x0013;
            GameBoy.CPU.Reg16.DE = 0x00D8;
            GameBoy.CPU.Reg16.HL = 0x014D;
        }

        GameBoy.CPU.Reg16.PC = 0x0100;
        GameBoy.CPU.Reg16.SP = 0xFFFE;

        switch(GameBoy.Emulator.HardwareType)
        {
            case HW_GB:
            case HW_SGB:
                GameBoy.CPU.Reg8.A = 0x01; // SGB or Normal Gameboy
                break;
            case HW_GBP:
            case HW_SGB2:
                GameBoy.CPU.Reg8.A = 0xFF; // SGB2 or Pocket Gameboy
                break;
            case HW_GBA:
                GameBoy.CPU.Reg8.B |= 0x01; // GBA
                //NO BREAK
            case HW_GBC:
                GameBoy.CPU.Reg8.A = 0x11; // CGB or GBA
                break;
        }
	}
	else
	{
	    // No idea of the real initial values (except the PC one, it must be 0x0000).
	    // Maybe they are random?
		GameBoy.CPU.Reg16.AF = 0x0000;
		GameBoy.CPU.Reg16.BC = 0x0000;
		GameBoy.CPU.Reg16.DE = 0x0000;
		GameBoy.CPU.Reg16.HL = 0x0000;
		GameBoy.CPU.Reg16.PC = 0x0000;
		GameBoy.CPU.Reg16.SP = 0x0000;
	}
}

inline void GB_CPUClock(int clocks)
{
	GameBoy.Emulator.Clocks += clocks;
	if(GameBoy.Emulator.timer_enabled) GameBoy.Emulator.TimerClocks += clocks;
	GameBoy.Emulator.DivClocks += clocks;
	GB_SoundUpdate(clocks);
	GB_SerialClocks(clocks);
//	SGB_Clock(clocks);

	if(GameBoy.Emulator.VBL_clocks_delay)
	{
		GameBoy.Emulator.VBL_clocks_delay -= clocks;

		if(GameBoy.Emulator.VBL_clocks_delay <= 0)
		{
			GB_SetInterrupt(I_VBLANK);
			GameBoy.Emulator.VBL_clocks_delay = 0;
		}
	}
}

inline void __gb_break_to_debugger(void)
{/*
    GameBoy.Emulator.FrameDrawn = 1; // This is a hack, but it works...
    gb_set_pause();
    GB_UpdateDebugger();
    PresentMainWindow();*/
}

void GB_CPUStep(void)
{
	u32 temp, temp2, temp3;

	_GB_CPU_ * cpu = &GameBoy.CPU;
	_GB_MEMORY_ * mem = &GameBoy.Memory;

	if(GameBoy.Emulator.CGBEnabled)
	{
	    switch(GBC_HDMAcopy())
	    {
	        default:
	        case 0: //nothing
                break;
            case 1: //transfering data...
                GB_CPUClock(8); //This is 8 even in double speed mode
                GB_CPUHandleClock();
                return;
            case 2: //preparation time...
                //~231 clocks, but we rather use 232.
                //minus 8 because 16 bytes are transfered during this (?)
                //(232-8)/8 = 28 times to wait...
                //in double speed mode, though, this will (wrongly) use 4 clocks more, fix in "case 3"
                GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
                GB_CPUHandleClock();
                return;
            case 3:
                GB_CPUClock(GameBoy.Emulator.DoubleSpeed ? 4 : 8);
                GB_CPUHandleClock();
                return;
		}
	}

	if(GameBoy.Emulator.halt_not_executed && GameBoy.Memory.InterruptMasterEnable)
	{
		GameBoy.Emulator.CPUHalt = 1;
		GameBoy.Emulator.halt_not_executed = 0;
	}

	GB_CPUHandleClock();

	if(GB_CPUHandleInterrupts())
        GB_CPUClock(14 >> GameBoy.Emulator.DoubleSpeed); //should be 13... ?
/*
    if(gb_is_paused() == 0) //If gb is paused and this is executed, it's because of the debugger
    {
        if(GB_Debug_Breakpoint_Check(cpu->Reg16.PC))
        {
            __gb_break_to_debugger();
            return;
        }
    }
*/
	if(GameBoy.Emulator.CPUHalt) //if halt or stop...
	{
		if(GameBoy.Emulator.CPUHalt == 1) //halt
		{
			GB_CPUClock(4 >> GameBoy.Emulator.DoubleSpeed);
			GB_CPUHandleClock();
			return;
		}
		else //if(GameBoy.Emulator.CPUHalt == 2) //stop
		{
			//GB_CPUClock(80);
            GameBoy.Emulator.FrameDrawn = 1; // The GB does nothing in stop mode, so
            GameBoy.Emulator.FrameCount ++; //let's just skip frame...

            GameBoy.Emulator.CPUHalt = 2; //Interrupts doesn't work, at least, VBL.

			u32 i = mem->IO_Ports[P1_REG-0xFF00];
			u32 result = 0;

			//if((i & 0x30) == 0x30)
			//{
			//	DebugMessage("STOP with no possible exit, ignored.\nPC: %04x\nROM: %d",
            //           GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			//	GameBoy.Emulator.CPUHalt = 0;
			//	return;
			//}
            int Keys = GB_Input_Get(0);
			if((i & (1<<5)) == 0) //A-B-SEL-STA
				result |= Keys & (KEY_A|KEY_B|KEY_SELECT|KEY_START);
			if((i & (1<<4)) == 0) //PAD
				result |= Keys & (KEY_UP|KEY_DOWN|KEY_LEFT|KEY_RIGHT);

			if(result) GameBoy.Emulator.CPUHalt = 0;

			return;
		}
	}

	if(GameBoy.Memory.interrupts_enable_count)
	{
		GameBoy.Memory.interrupts_enable_count = 0;
		GameBoy.Memory.InterruptMasterEnable = 1;
	}

	u32 opcode = GB_MemRead8(cpu->Reg16.PC++);

	switch(opcode)
	{
		case 0x00: //NOP
			break;
		case 0x01: //LD BC,nn
			cpu->Reg16.BC = GB_MemRead16(cpu->Reg16.PC);
			cpu->Reg16.PC +=2;
			break;
		case 0x02: //LD (BC),A
			GB_MemWrite8(cpu->Reg16.BC,cpu->Reg8.A);
			break;
		case 0x03: //INC BC
			cpu->Reg16.BC = (cpu->Reg16.BC+1) & 0xFFFF;
			break;
		case 0x04: //INC B
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.B & 0xF) == 0xF);
			cpu->Reg8.B ++;
			cpu->Flags.Zero = (cpu->Reg8.B == 0);
			break;
		case 0x05: //DEC B
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.B & 0xF) == 0x0);
			cpu->Reg8.B --;
			cpu->Flags.Zero = (cpu->Reg8.B == 0);
			break;
		case 0x06: //LD B,n
			cpu->Reg8.B = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x07: //RLCA
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_ZERO);
			cpu->Flags.Carry = (cpu->Reg8.A & 0x80) != 0;
			cpu->Reg8.A = (cpu->Reg8.A << 1) | cpu->Flags.Carry;
			break;
		case 0x08: //LD (nn),SP
			GB_MemWrite16(GB_MemRead16(cpu->Reg16.PC),cpu->Reg16.SP);
			cpu->Reg16.PC +=2;
			break;
		case 0x09: //ADD HL,BC
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = cpu->Reg16.HL + cpu->Reg16.BC;
			cpu->Flags.Carry = (temp > 0xFFFF);
			cpu->Flags.HalfCarry = ( ( (cpu->Reg16.HL & 0x0FFF) + (cpu->Reg16.BC & 0x0FFF) ) > 0x0FFF );
			cpu->Reg16.HL = temp & 0xFFFF;
			break;
		case 0x0A: //LD A,(BC)
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.BC);
			break;
		case 0x0B: //DEC BC
			cpu->Reg16.BC = (cpu->Reg16.BC-1) & 0xFFFF;
			break;
		case 0x0C: //INC C
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.C & 0xF) == 0xF);
			cpu->Reg8.C ++;
			cpu->Flags.Zero = (cpu->Reg8.C == 0);
			break;
		case 0x0D: //DEC C
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.C & 0xF) == 0x0);
			cpu->Reg8.C --;
			cpu->Flags.Zero = (cpu->Reg8.C == 0);
			break;
		case 0x0E: //LD C,n
			cpu->Reg8.C = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x0F: //RRCA
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_ZERO);
			cpu->Flags.Carry = (cpu->Reg8.A & 0x01) != 0;
			cpu->Reg8.A = (cpu->Reg8.A >> 1) | (cpu->Flags.Carry << 7);
			break;
		case 0x10: //STOP

			if(GB_MemRead8(cpu->Reg16.PC++) != 0)
				DebugMessage("Corrupted stop.\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);

			if(GameBoy.Emulator.CGBEnabled == 0)
			{
				GameBoy.Emulator.CPUHalt = 2;
			}
			else //Switch to double speed mode (CGB)
			{
				if(mem->IO_Ports[KEY1_REG-0xFF00]&1)
				{
					GameBoy.Emulator.DoubleSpeed ^= 1;
					mem->IO_Ports[KEY1_REG-0xFF00] = GameBoy.Emulator.DoubleSpeed<<7;
				}
				else
				{
					GameBoy.Emulator.CPUHalt = 2;
				}
			}

			break;
		case 0x11: //LD DE,nn
			cpu->Reg16.DE = GB_MemRead16(cpu->Reg16.PC);
			cpu->Reg16.PC +=2;
			break;
		case 0x12: //LD (DE),A
			GB_MemWrite8(cpu->Reg16.DE,cpu->Reg8.A);
			break;
		case 0x13: //INC DE
			cpu->Reg16.DE = (cpu->Reg16.DE+1) & 0xFFFF;
			break;
		case 0x14: //INC D
			cpu->Reg16.AF &= ~(F_SUBSTRACT);
			cpu->Flags.HalfCarry = ( (cpu->Reg8.D & 0xF) == 0xF);
			cpu->Reg8.D ++;
			cpu->Flags.Zero = (cpu->Reg8.D == 0);
			break;
		case 0x15: //DEC D
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.D & 0xF) == 0x0);
			cpu->Reg8.D --;
			cpu->Flags.Zero = (cpu->Reg8.D == 0);
			break;
		case 0x16: //LD D,n
			cpu->Reg8.D = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x17: //RLA
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_ZERO);
			temp = cpu->Flags.Carry; //old carry flag
			cpu->Flags.Carry = (cpu->Reg8.A & 0x80) != 0;
			cpu->Reg8.A = (cpu->Reg8.A << 1) | temp;
			break;
		case 0x18: //JR n
			cpu->Reg16.PC = (cpu->Reg16.PC + 1 + (s8)GB_MemRead8(cpu->Reg16.PC)) & 0xFFFF;
			break;
		case 0x19: //ADD HL,DE
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = cpu->Reg16.HL + cpu->Reg16.DE;
			cpu->Flags.Carry = ( temp > 0xFFFF );
			cpu->Flags.HalfCarry = ( ( (cpu->Reg16.HL & 0xFFF) + (cpu->Reg16.DE & 0xFFF) ) > 0xFFF );
			cpu->Reg16.HL = temp & 0xFFFF;
			break;
		case 0x1A: //LD A,(DE)
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.DE);
			break;
		case 0x1B: //DEC DE
			cpu->Reg16.DE = (cpu->Reg16.DE-1) & 0xFFFF;
			break;
		case 0x1C: //INC E
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.E & 0xF) == 0xF);
			cpu->Reg8.E ++;
			cpu->Flags.Zero = (cpu->Reg8.E == 0);
			break;
		case 0x1D: //DEC E
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.E & 0xF) == 0x0);
			cpu->Reg8.E --;
			cpu->Flags.Zero = (cpu->Reg8.E == 0);
			break;
		case 0x1E: //LD E,n
			cpu->Reg8.E = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x1F: //RRA
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_ZERO);
			temp = cpu->Flags.Carry; //old carry flag
			cpu->Flags.Carry = cpu->Reg8.A & 0x01;
			cpu->Reg8.A = (cpu->Reg8.A >> 1) | (temp << 7);
			break;
		case 0x20: //JR NZ,n
			if(cpu->Flags.Zero == 0)
			{
				cpu->Reg16.PC = (cpu->Reg16.PC + 1 + (s8)GB_MemRead8(cpu->Reg16.PC)) & 0xFFFF;
				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC ++;
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0x21: //LD HL,nn
			cpu->Reg16.HL = GB_MemRead16(cpu->Reg16.PC);
			cpu->Reg16.PC +=2;
			break;
		case 0x22: //LD (HLI),A | LD (HL+),A | LDI (HL),A
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.A);
			cpu->Reg16.HL = (cpu->Reg16.HL+1) & 0xFFFF;
			break;
		case 0x23: //INC HL
			cpu->Reg16.HL = (cpu->Reg16.HL+1) & 0xFFFF;
			break;
		case 0x24: //INC H
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.H & 0xF) == 0xF);
			cpu->Reg8.H ++;
			cpu->Flags.Zero = (cpu->Reg8.H == 0);
			break;
		case 0x25: //DEC H
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.H & 0xF) == 0x0);
			cpu->Reg8.H --;
			cpu->Flags.Zero = (cpu->Reg8.H == 0);
			break;
		case 0x26: //LD H,n
			cpu->Reg8.H = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x27: //DAA
/*
            // http://www.z80.info/z80syntx.htm#DAA
--------------------------------------------------------------------------------
|           | C Flag  | HEX value in | H Flag | HEX value in | Number  | C flag|
| Operation | Before  | upper digit  | Before | lower digit  | added   | After |
|           | DAA     | (bit 7-4)    | DAA    | (bit 3-0)    | to byte | DAA   |
|------------------------------------------------------------------------------|
|           |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
|   ADD     |    0    |     0-8      |   0    |     A-F      |   06    |   0   |
|           |    0    |     0-9      |   1    |     0-3      |   06    |   0   |
|   ADC     |    0    |     A-F      |   0    |     0-9      |   60    |   1   |
|           |    0    |     9-F      |   0    |     A-F      |   66    |   1   |
|   INC     |    0    |     A-F      |   1    |     0-3      |   66    |   1   |
|           |    1    |     0-2      |   0    |     0-9      |   60    |   1   |
|           |    1    |     0-2      |   0    |     A-F      |   66    |   1   |
|           |    1    |     0-3      |   1    |     0-3      |   66    |   1   |
|------------------------------------------------------------------------------|
|   SUB     |    0    |     0-9      |   0    |     0-9      |   00    |   0   |
|   SBC     |    0    |     0-8      |   1    |     6-F      |   FA    |   0   |
|   DEC     |    1    |     7-F      |   0    |     0-9      |   A0    |   1   |
|   NEG     |    1    |     6-F      |   1    |     6-F      |   9A    |   1   |
|------------------------------------------------------------------------------|
*/
			temp3 = cpu->Reg8.A;
			temp = temp3 & 0x0F;
			temp2 = (temp3 & 0xF0) >> 4;

			if(cpu->Reg16.AF & F_SUBSTRACT) //Substraction
			{
				if(cpu->Reg16.AF & F_CARRY)
				{
					if( (temp2 > 6) && (!cpu->Flags.HalfCarry) && (temp < 10) )
					{
						temp3 += 0xA0;
				//		cpu->Reg16.AF |= F_CARRY;
						//cpu->Reg16.AF &= ~(F_HALFCARRY);
					}
					else if( (temp2 > 5) && (cpu->Flags.HalfCarry) && (temp > 5) )
					{
						temp3 += 0x9A;
						cpu->Reg16.AF |= F_CARRY; //(F_CARRY|F_HALFCARRY);
					}
				}
				else
				{
					if( (temp2 < 10) && (!cpu->Flags.HalfCarry) && (temp < 10) )
					{
						cpu->Reg16.AF &= ~F_CARRY; //(F_CARRY|F_HALFCARRY);
					}
					else if( (temp2 < 9) && (cpu->Flags.HalfCarry) && (temp > 5) )
					{
						temp3 += 0xFA;
				//		cpu->Reg16.AF &= ~F_CARRY;
						//cpu->Reg16.AF |= F_HALFCARRY;
					}
				}
			}
			else //Addition
			{
				if(cpu->Reg16.AF & F_CARRY)
				{
					if( (temp2 < 3) && (!cpu->Flags.HalfCarry) && (temp < 10))
					{
						temp3 += 0x60;
				//		cpu->Reg16.AF |= F_CARRY;
						//cpu->Reg16.AF &= ~F_HALFCARRY;
					}
					else if( (temp2 < 3) && (!cpu->Flags.HalfCarry) && (temp > 9))
					{
						temp3 += 0x66;
						cpu->Reg16.AF |= F_CARRY; //F_CARRY|F_HALFCARRY;
					}
					else if( (temp2 < 4) && (cpu->Flags.HalfCarry) && (temp < 4))
					{
						temp3 += 0x66;
				//		cpu->Reg16.AF |= F_CARRY;
						//cpu->Reg16.AF &= ~F_HALFCARRY;
					}
				}
				else
				{
					if( (temp2 < 10) && (!cpu->Flags.HalfCarry) && (temp < 10))
					{
						cpu->Reg16.AF &= ~F_CARRY; //(F_CARRY|F_HALFCARRY);
					}
					else if( (temp2 < 9) && (!cpu->Flags.HalfCarry) && (temp > 9))
					{
						temp3 += 0x06;
				//		cpu->Reg16.AF &= ~F_CARRY;
						//cpu->Reg16.AF |= F_HALFCARRY;
					}
					else if( (temp2 < 10) && (cpu->Flags.HalfCarry) && (temp < 4))
					{
						temp3 += 0x06;
						cpu->Reg16.AF &= ~F_CARRY; //(F_CARRY|F_HALFCARRY);
					}
					else if( (temp2 > 9) && (!cpu->Flags.HalfCarry) && (temp < 10))
					{
						temp3 += 0x60;
						cpu->Reg16.AF |= F_CARRY;
						//cpu->Reg16.AF &= ~F_HALFCARRY;
					}
					else if( (temp2 > 8) && (!cpu->Flags.HalfCarry) && (temp > 9))
					{
						temp3 += 0x66;
						cpu->Reg16.AF |= F_CARRY; //(F_CARRY|F_HALFCARRY);
					}
					else if( (temp2 > 9) && (cpu->Flags.HalfCarry) && (temp < 4))
					{
						temp3 += 0x66;
						cpu->Reg16.AF |= F_CARRY;
						//cpu->Reg16.AF &= ~F_HALFCARRY;
					}
				}
			}

			temp3 &= 0xFF;

			cpu->Reg8.A = temp3;
			cpu->Flags.Zero = (temp3 == 0);
			break;
		case 0x28: //JR Z,n
			if(cpu->Flags.Zero)
			{
				cpu->Reg16.PC = (cpu->Reg16.PC + 1 + (s8)GB_MemRead8(cpu->Reg16.PC)) & 0xFFFF;
				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC ++;
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0x29: //ADD HL,HL
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.Carry = (cpu->Reg16.HL & 0x8000) != 0;
			cpu->Flags.HalfCarry = (cpu->Reg16.HL & 0x0800) != 0;
			cpu->Reg16.HL = (cpu->Reg16.HL << 1) & 0xFFFF;
			break;
		case 0x2A: //LD A,(HLI) | LD A,(HL+) | LDI A,(HL)
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.HL);
			cpu->Reg16.HL = (cpu->Reg16.HL+1) & 0xFFFF;
			break;
		case 0x2B: //DEC HL
			cpu->Reg16.HL = (cpu->Reg16.HL-1) & 0xFFFF;
			break;
		case 0x2C: //INC L
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.L & 0xF) == 0xF);
			cpu->Reg8.L ++;
			cpu->Flags.Zero = (cpu->Reg8.L == 0);
			break;
		case 0x2D: //DEC L
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.L & 0xF) == 0x0);
			cpu->Reg8.L --;
			cpu->Flags.Zero = (cpu->Reg8.L == 0);
			break;
		case 0x2E: //LD L,n
			cpu->Reg8.L = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x2F: //CPL
			cpu->Reg16.AF |= (F_SUBSTRACT|F_HALFCARRY);
			cpu->Reg8.A = ~cpu->Reg8.A;
			break;
		case 0x30: //JR NC,n
			if(cpu->Flags.Carry == 0)
			{
				cpu->Reg16.PC = (cpu->Reg16.PC + 1 + (s8)GB_MemRead8(cpu->Reg16.PC)) & 0xFFFF;
				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC ++;
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0x31: //LD SP,nn
			cpu->Reg16.SP = GB_MemRead16(cpu->Reg16.PC);
			cpu->Reg16.PC +=2;
			break;
		case 0x32: //LD (HLD),A | LD (HL-),A | LDD (HL),A
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.A);
			cpu->Reg16.HL = (cpu->Reg16.HL-1) & 0xFFFF;
			break;
		case 0x33: //INC SP
			cpu->Reg16.SP = (cpu->Reg16.SP+1) & 0xFFFF;
			break;
		case 0x34: //INC (HL)
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.HL);
			cpu->Flags.HalfCarry = ( (temp & 0xF) == 0xF);
			temp = (temp + 1) & 0xFF;
			cpu->Flags.Zero = (temp == 0);
			GB_MemWrite8(cpu->Reg16.HL,temp);
			break;
		case 0x35: //DEC (HL)
			cpu->Reg16.AF |= F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.HL);
			cpu->Flags.HalfCarry = ( (temp & 0xF) == 0x0);
			temp  = (temp - 1) & 0xFF;
			cpu->Flags.Zero = (temp == 0);
			GB_MemWrite8(cpu->Reg16.HL,temp);
			break;
		case 0x36: //LD (HL),n
			GB_MemWrite8(cpu->Reg16.HL,GB_MemRead8(cpu->Reg16.PC++));
			break;
		case 0x37: //SCF
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
			cpu->Reg16.AF |= F_CARRY;
			break;
		case 0x38: //JR C,n
			if(cpu->Flags.Carry == 1)
			{
				cpu->Reg16.PC = (cpu->Reg16.PC + 1 + (s8)GB_MemRead8(cpu->Reg16.PC)) & 0xFFFF;
				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC ++;
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0x39: //ADD HL,SP
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = cpu->Reg16.HL + cpu->Reg16.SP;
			cpu->Flags.Carry = (temp > 0xFFFF);
			cpu->Flags.HalfCarry = ( ( (cpu->Reg16.HL & 0x0FFF) + (cpu->Reg16.SP & 0x0FFF) ) > 0x0FFF );
			cpu->Reg16.HL = temp & 0xFFFF;
			break;
		case 0x3A: //LD A,(HLD) | LD A,(HL-) | LDD A,(HL)
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.HL);
			cpu->Reg16.HL = (cpu->Reg16.HL-1) & 0xFFFF;
			break;
		case 0x3B: //DEC SP
			cpu->Reg16.SP = (cpu->Reg16.SP-1) & 0xFFFF;
			break;
		case 0x3C: //INC A
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A & 0xF) == 0xF);
			cpu->Reg8.A ++;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x3D: //DEC A
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A & 0xF) == 0x0);
			cpu->Reg8.A --;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x3E: //LD A,n
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.PC++);
			break;
		case 0x3F: //CCF
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
			cpu->Flags.Carry = !cpu->Flags.Carry;
			break;
		case 0x40: //LD B,B
			//cpu->Reg8.B = cpu->Reg8.B;
			break;
		case 0x41: //LD B,C
			cpu->Reg8.B = cpu->Reg8.C;
			break;
		case 0x42: //LD B,D
			cpu->Reg8.B = cpu->Reg8.D;
			break;
		case 0x43: //LD B,E
			cpu->Reg8.B = cpu->Reg8.E;
			break;
		case 0x44: //LD B,H
			cpu->Reg8.B = cpu->Reg8.H;
			break;
		case 0x45: //LD B,L
			cpu->Reg8.B = cpu->Reg8.L;
			break;
		case 0x46: //LD B,(HL)
			cpu->Reg8.B = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x47: //LD B,A
			cpu->Reg8.B = cpu->Reg8.A;
			break;
		case 0x48: //LD C,B
			cpu->Reg8.C = cpu->Reg8.B;
			break;
		case 0x49: //LD C,C
			//cpu->Reg8.C = cpu->Reg8.C;
			break;
		case 0x4A: //LD C,D
			cpu->Reg8.C = cpu->Reg8.D;
			break;
		case 0x4B: //LD C,E
			cpu->Reg8.C = cpu->Reg8.E;
			break;
		case 0x4C: //LD C,H
			cpu->Reg8.C = cpu->Reg8.H;
			break;
		case 0x4D: //LD C,L
			cpu->Reg8.C = cpu->Reg8.L;
			break;
		case 0x4E: //LD C,(HL)
			cpu->Reg8.C = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x4F: //LD C,A
			cpu->Reg8.C = cpu->Reg8.A;
			break;
		case 0x50: //LD D,B
			cpu->Reg8.D = cpu->Reg8.B;
			break;
		case 0x51: //LD D,C
			cpu->Reg8.D = cpu->Reg8.C;
			break;
		case 0x52: //LD D,D
			//cpu->Reg8.D = cpu->Reg8.D;
			break;
		case 0x53: //LD D,E
			cpu->Reg8.D = cpu->Reg8.E;
			break;
		case 0x54: //LD D,H
			cpu->Reg8.D = cpu->Reg8.H;
			break;
		case 0x55: //LD D,L
			cpu->Reg8.D = cpu->Reg8.L;
			break;
		case 0x56: //LD D,(HL)
			cpu->Reg8.D = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x57: //LD D,A
			cpu->Reg8.D = cpu->Reg8.A;
			break;
		case 0x58: //LD E,B
			cpu->Reg8.E = cpu->Reg8.B;
			break;
		case 0x59: //LD E,C
			cpu->Reg8.E = cpu->Reg8.C;
			break;
		case 0x5A: //LD E,D
			cpu->Reg8.E = cpu->Reg8.D;
			break;
		case 0x5B: //LD E,E
			//cpu->Reg8.E = cpu->Reg8.E;
			break;
		case 0x5C: //LD E,H
			cpu->Reg8.E = cpu->Reg8.H;
			break;
		case 0x5D: //LD E,L
			cpu->Reg8.E = cpu->Reg8.L;
			break;
		case 0x5E: //LD E,(HL)
			cpu->Reg8.E = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x5F: //LD E,A
			cpu->Reg8.E = cpu->Reg8.A;
			break;
		case 0x60: //LD H,B
			cpu->Reg8.H = cpu->Reg8.B;
			break;
		case 0x61: //LD H,C
			cpu->Reg8.H = cpu->Reg8.C;
			break;
		case 0x62: //LD H,D
			cpu->Reg8.H = cpu->Reg8.D;
			break;
		case 0x63: //LD H,E
			cpu->Reg8.H = cpu->Reg8.E;
			break;
		case 0x64: //LD H,H
			//cpu->Reg8.H = cpu->Reg8.H;
			break;
		case 0x65: //LD H,L
			cpu->Reg8.H = cpu->Reg8.L;
			break;
		case 0x66: //LD H,(HL)
			cpu->Reg8.H = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x67: //LD H,A
			cpu->Reg8.H = cpu->Reg8.A;
			break;
		case 0x68: //LD L,B
			cpu->Reg8.L = cpu->Reg8.B;
			break;
		case 0x69: //LD L,C
			cpu->Reg8.L = cpu->Reg8.C;
			break;
		case 0x6A: //LD L,D
			cpu->Reg8.L = cpu->Reg8.D;
			break;
		case 0x6B: //LD L,E
			cpu->Reg8.L = cpu->Reg8.E;
			break;
		case 0x6C: //LD L,H
			cpu->Reg8.L = cpu->Reg8.H;
			break;
		case 0x6D: //LD L,L
			//cpu->Reg8.L = cpu->Reg8.L;
			break;
		case 0x6E: //LD L,(HL)
			cpu->Reg8.L = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x6F: //LD L,A
			cpu->Reg8.L = cpu->Reg8.A;
			break;
		case 0x70: //LD (HL),B
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.B);
			break;
		case 0x71: //LD (HL),C
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.C);
			break;
		case 0x72: //LD (HL),D
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.D);
			break;
		case 0x73: //LD (HL),E
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.E);
			break;
		case 0x74: //LD (HL),H
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.H);
			break;
		case 0x75: //LD (HL),L
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.L);
			break;
		case 0x76: //HALT
			if(GameBoy.Memory.InterruptMasterEnable == 1)
			{
			//	if(GameBoy.Emulator.HDMAenabled == HDMA_NONE)
			//	{
					GameBoy.Emulator.CPUHalt = 1;
			//	}
			}
			else
			{
		/*		if(mem->IO_Ports[IF_REG-0xFF00] & mem->HighRAM[IE_REG-0xFF80] & 0x1F)
				{

					The first byte of the next instruction
				after HALT is read. The Program Counter (PC) fails to
				increment to the next memory location. As a results, the
				first byte after HALT is read again. From this point on
				the Program Counter once again operates normally

				}
				else
				{
					//Nothing
				}
		*/
				GameBoy.Emulator.halt_not_executed = 1;
			}
			break;
		case 0x77: //LD (HL),A
			GB_MemWrite8(cpu->Reg16.HL,cpu->Reg8.A);
			break;
		case 0x78: //LD A,B
			cpu->Reg8.A = cpu->Reg8.B;
			break;
		case 0x79: //LD A,C
			cpu->Reg8.A = cpu->Reg8.C;
			break;
		case 0x7A: //LD A,D
			cpu->Reg8.A = cpu->Reg8.D;
			break;
		case 0x7B: //LD A,E
			cpu->Reg8.A = cpu->Reg8.E;
			break;
		case 0x7C: //LD A,H
			cpu->Reg8.A = cpu->Reg8.H;
			break;
		case 0x7D: //LD A,L
			cpu->Reg8.A = cpu->Reg8.L;
			break;
		case 0x7E: //LD A,(HL)
			cpu->Reg8.A = GB_MemRead8(cpu->Reg16.HL);
			break;
		case 0x7F: //LD A,A
			//cpu->Reg8.A = cpu->Reg8.A;
			break;
		case 0x80: //ADD A,B
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.B & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.B;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x81: //ADD A,C
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.C & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.C;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x82: //ADD A,D
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.D & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.D;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x83: //ADD A,E
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.E & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.E;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x84: //ADD A,H
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.H & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.H;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x85: //ADD A,L
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;

			cpu->Flags.HalfCarry = ( (temp & 0xF) + ((u32)cpu->Reg8.L & 0xF) ) > 0xF;

			cpu->Reg8.A += cpu->Reg8.L;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x86: //ADD A,(HL)
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;
			temp2 = GB_MemRead16(cpu->Reg16.HL);

			cpu->Flags.HalfCarry = ( (temp & 0xF) + (temp2 & 0xF) ) > 0xF;

			cpu->Reg8.A += temp2;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0x87: //ADD A,A
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & BIT(3)) != 0;
			cpu->Flags.Carry = (cpu->Reg8.A & BIT(7)) != 0;

			cpu->Reg8.A += cpu->Reg8.A;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x88: //ADC A,B
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.B + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.B & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x89: //ADC A,C
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.C + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.C & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x8A: //ADC A,D
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.D + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.D & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x8B: //ADC A,E
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.E + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.E & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x8C: //ADC A,H
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.H + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.H & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x8D: //ADC A,L
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A + cpu->Reg8.L + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (cpu->Reg8.L & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x8E: //ADC A,(HL)
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.HL);
			temp2 = cpu->Reg8.A + temp + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (temp & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp2 > 0xFF);

			temp2 &= 0xFF;

			cpu->Reg8.A = temp2;
			cpu->Flags.Zero = (temp2 == 0);
			break;
		case 0x8F: //ADC A,A
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = ( ((u32)cpu->Reg8.A) << 1 ) + cpu->Flags.Carry;

			//Carry flag not needed to test this
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0x08) != 0;

			cpu->Flags.Carry = (temp > 0xFF);

			temp &= 0xFF;

			cpu->Reg8.A = temp;
			cpu->Flags.Zero = (temp == 0);
			break;
		case 0x90: //SUB B
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.B & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.B;

			cpu->Reg8.A -= cpu->Reg8.B;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x91: //SUB C
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.C & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.C;

			cpu->Reg8.A -= cpu->Reg8.C;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x92: //SUB D
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.D & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.D;

			cpu->Reg8.A -= cpu->Reg8.D;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x93: //SUB E
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.E & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.E;

			cpu->Reg8.A -= cpu->Reg8.E;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x94: //SUB H
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.H & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.H;

			cpu->Reg8.A -= cpu->Reg8.H;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x95: //SUB L
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.L & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.L;

			cpu->Reg8.A -= cpu->Reg8.L;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x96: //SUB (HL)
			temp = GB_MemRead8(cpu->Reg16.HL);
			cpu->Reg8.F = F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (temp & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)temp;

			cpu->Reg8.A -= temp;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0x97: //SUB A
			cpu->Reg8.F = F_SUBSTRACT|F_ZERO;
			cpu->Reg8.A = 0;
			break;
		case 0x98: //SBC A,B
			temp = cpu->Reg8.A - cpu->Reg8.B - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.B^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x99: //SBC A,C
            temp = cpu->Reg8.A - cpu->Reg8.C - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.C^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9A: //SBC A,D
			temp = cpu->Reg8.A - cpu->Reg8.D - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.D^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9B: //SBC A,E
			temp = cpu->Reg8.A - cpu->Reg8.E - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.E^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9C: //SBC A,H
			temp = cpu->Reg8.A - cpu->Reg8.H - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.H^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9D: //SBC A,L
			temp = cpu->Reg8.A - cpu->Reg8.L - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^cpu->Reg8.L^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9E: //SBC A,(HL)
            temp2 = GB_MemRead8(cpu->Reg16.HL);
            temp = cpu->Reg8.A - temp2 - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^temp2^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0x9F: //SBC A,A
            cpu->Reg16.AF = cpu->Reg8.F&F_CARRY ?
                    ( (0xFF<<8)|F_CARRY|F_HALFCARRY|F_SUBSTRACT ) : (F_ZERO|F_SUBSTRACT) ;
			break;
		case 0xA0: //AND B
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.B;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA1: //AND C
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.C;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA2: //AND D
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.D;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA3: //AND E
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.E;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA4: //AND H
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.H;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA5: //AND L
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= cpu->Reg8.L;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA6: //AND (HL)
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg8.A &= GB_MemRead8(cpu->Reg16.HL);
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA7: //AND A
			cpu->Reg16.AF |= F_HALFCARRY;
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			//cpu->Reg8.A &= cpu->Reg8.A;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA8: //XOR B
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.B;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xA9: //XOR C
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.C;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAA: //XOR D
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.D;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAB: //XOR E
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.E;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAC: //XOR H
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.H;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAD: //XOR L
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= cpu->Reg8.L;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAE: //XOR (HL)
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= GB_MemRead8(cpu->Reg16.HL);

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xAF: //XOR A
			cpu->Reg16.AF = F_ZERO;
			break;
		case 0xB0: //OR B
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.B;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB1: //OR C
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.C;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB2: //OR D
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.D;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB3: //OR E
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.E;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB4: //OR H
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.H;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB5: //OR L
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= cpu->Reg8.L;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB6: //OR (HL)
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A |= GB_MemRead8(cpu->Reg16.HL);

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB7: //OR A
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			//cpu->Reg8.A |= cpu->Reg8.A;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xB8: //CP B
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.B & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.B;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.B);
			break;
		case 0xB9: //CP C
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.C & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.C;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.C);
			break;
		case 0xBA: //CP D
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.D & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.D;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.D);
			break;
		case 0xBB: //CP E
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.E & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.E;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.E);
			break;
		case 0xBC: //CP H
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.H & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.H;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.H);
			break;
		case 0xBD: //CP L
			cpu->Reg16.AF |= F_SUBSTRACT;
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (cpu->Reg8.L & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < (u32)cpu->Reg8.L;
			cpu->Flags.Zero = (cpu->Reg8.A == cpu->Reg8.L);
			break;
		case 0xBE: //CP (HL)
			cpu->Reg16.AF |= F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.HL);
			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (temp & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < temp;
			cpu->Flags.Zero = (cpu->Reg8.A == temp);
			break;
		case 0xBF: //CP A
			cpu->Reg16.AF |= (F_SUBSTRACT|F_ZERO);
			cpu->Reg16.AF &= ~(F_HALFCARRY|F_CARRY);
			break;
		case 0xC0: //RET NZ
			if(cpu->Flags.Zero == 0)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
				cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;

				GB_CPUClock(20 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xC1: //POP BC
			cpu->Reg16.BC = GB_MemRead16(cpu->Reg16.SP);
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			break;
		case 0xC2: //JP NZ,nn
			if(cpu->Flags.Zero == 0)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(16 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xC3: //JP nn
			cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);
			break;
		case 0xC4: //CALL NZ,nn
			if(cpu->Flags.Zero == 0)
			{
				cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
				GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC+2);
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(24 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC  += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xC5: //PUSH BC
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.BC);
			break;
		case 0xC6: //ADD A,n
			cpu->Reg16.AF &= ~F_SUBSTRACT;

			temp = cpu->Reg8.A;
			temp2 = GB_MemRead8(cpu->Reg16.PC++);

			cpu->Flags.HalfCarry = ( (temp & 0xF) + (temp2 & 0xF) ) > 0xF;

			cpu->Reg8.A += temp2;
			cpu->Flags.Zero = (cpu->Reg8.A == 0);

			cpu->Flags.Carry = (temp > cpu->Reg8.A);
			break;
		case 0xC7: //RST 0x00
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0000;
			break;
		case 0xC8: //RET Z
			if(cpu->Flags.Zero == 1)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
				cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;

				GB_CPUClock(20 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xC9: //RET
			cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			break;
		case 0xCA: //JP Z,nn
			if(cpu->Flags.Zero == 1)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(16 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xCB:
			//Get real opcode
			temp3 = GB_MemRead8(cpu->Reg16.PC++);

			switch(temp3)
			{
				case 0x00: //RLC B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.B & 0x80) != 0;
					cpu->Reg8.B = (cpu->Reg8.B << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x01: //RLC C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.C & 0x80) != 0;
					cpu->Reg8.C = (cpu->Reg8.C << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x02: //RLC D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.D & 0x80) != 0;
					cpu->Reg8.D = (cpu->Reg8.D << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x03: //RLC E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.E & 0x80) != 0;
					cpu->Reg8.E = (cpu->Reg8.E << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x04: //RLC H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.H & 0x80) != 0;
					cpu->Reg8.H = (cpu->Reg8.H << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x05: //RLC L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.L & 0x80) != 0;
					cpu->Reg8.L = (cpu->Reg8.L << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x06: //RLC (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp & 0x80) != 0;
					temp = (temp << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (temp == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp);
					break;
				case 0x07: //RLC A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.A & 0x80) != 0;
					cpu->Reg8.A = (cpu->Reg8.A << 1) | cpu->Flags.Carry;
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x08: //RRC B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.B & 0x01) != 0;
					cpu->Reg8.B = (cpu->Reg8.B >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x09: //RRC C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.C & 0x01) != 0;
					cpu->Reg8.C = (cpu->Reg8.C >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x0A: //RRC D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.D & 0x01) != 0;
					cpu->Reg8.D = (cpu->Reg8.D >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x0B: //RRC E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.E & 0x01) != 0;
					cpu->Reg8.E = (cpu->Reg8.E >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x0C: //RRC H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.H & 0x01) != 0;
					cpu->Reg8.H = (cpu->Reg8.H >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x0D: //RRC L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.L & 0x01) != 0;
					cpu->Reg8.L = (cpu->Reg8.L >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x0E: //RRC (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp & 0x01) != 0;
					temp = (temp >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (temp == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp);
					break;
				case 0x0F: //RRC A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.A & 0x01) != 0;
					cpu->Reg8.A = (cpu->Reg8.A >> 1) | (cpu->Flags.Carry << 7);
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x10: //RL B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.B & 0x80) != 0;
					cpu->Reg8.B = (cpu->Reg8.B << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x11: //RL C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.C & 0x80) != 0;
					cpu->Reg8.C = (cpu->Reg8.C << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x12: //RL D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.D & 0x80) != 0;
					cpu->Reg8.D = (cpu->Reg8.D << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x13: //RL E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.E & 0x80) != 0;
					cpu->Reg8.E = (cpu->Reg8.E << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x14: //RL H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.H & 0x80) != 0;
					cpu->Reg8.H = (cpu->Reg8.H << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x15: //RL L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.L & 0x80) != 0;
					cpu->Reg8.L = (cpu->Reg8.L << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x16: //RL (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					temp2 = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp2 & 0x80) != 0;
					temp2 = ((temp2 << 1) | temp) & 0xFF;
					cpu->Flags.Zero = (temp2 == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp2);
					break;
				case 0x17: //RL A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.A & 0x80) != 0;
					cpu->Reg8.A = (cpu->Reg8.A << 1) | temp;
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x18: //RR B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.B & 0x01) != 0;
					cpu->Reg8.B = (cpu->Reg8.B >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x19: //RR C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.C & 0x01) != 0;
					cpu->Reg8.C = (cpu->Reg8.C >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x1A: //RR D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.D & 0x01) != 0;
					cpu->Reg8.D = (cpu->Reg8.D >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x1B: //RR E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.E & 0x01) != 0;
					cpu->Reg8.E = (cpu->Reg8.E >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x1C: //RR H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.H & 0x01) != 0;
					cpu->Reg8.H = (cpu->Reg8.H >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x1D: //RR L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.L & 0x01) != 0;
					cpu->Reg8.L = (cpu->Reg8.L >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x1E: //RR (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					temp2 = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp2 & 0x01) != 0;
					temp2 = (temp2 >> 1) | (temp << 7);
					cpu->Flags.Zero = (temp2 == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp2);
					break;
				case 0x1F: //RR A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = cpu->Flags.Carry; //old carry flag
					cpu->Flags.Carry = (cpu->Reg8.A & 0x01) != 0;
					cpu->Reg8.A = (cpu->Reg8.A >> 1) | (temp << 7);
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x20: //SLA B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.B & 0x80) != 0;
					cpu->Reg8.B = cpu->Reg8.B << 1;
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x21: //SLA C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.C & 0x80) != 0;
					cpu->Reg8.C = cpu->Reg8.C << 1;
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x22: //SLA D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.D & 0x80) != 0;
					cpu->Reg8.D = cpu->Reg8.D << 1;
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x23: //SLA E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.E & 0x80) != 0;
					cpu->Reg8.E = cpu->Reg8.E << 1;
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x24: //SLA H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.H & 0x80) != 0;
					cpu->Reg8.H = cpu->Reg8.H << 1;
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x25: //SLA L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.L & 0x80) != 0;
					cpu->Reg8.L = cpu->Reg8.L << 1;
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x26: //SLA (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp & 0x80) != 0;
					temp = (temp << 1) & 0xFF;
					cpu->Flags.Zero = (temp == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp);
					break;
				case 0x27: //SLA A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.A & 0x80) != 0;
					cpu->Reg8.A = cpu->Reg8.A << 1;
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x28: //SRA B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.B & 0x01) != 0;
					cpu->Reg8.B = (cpu->Reg8.B & 0x80) | (cpu->Reg8.B >> 1);
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x29: //SRA C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.C & 0x01) != 0;
					cpu->Reg8.C = (cpu->Reg8.C & 0x80) | (cpu->Reg8.C >> 1);
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x2A: //SRA D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.D & 0x01) != 0;
					cpu->Reg8.D = (cpu->Reg8.D & 0x80) | (cpu->Reg8.D >> 1);
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x2B: //SRA E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.E & 0x01) != 0;
					cpu->Reg8.E = (cpu->Reg8.E & 0x80) | (cpu->Reg8.E >> 1);
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x2C: //SRA H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.H & 0x01) != 0;
					cpu->Reg8.H = (cpu->Reg8.H & 0x80) | (cpu->Reg8.H >> 1);
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x2D: //SRA L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.L & 0x01) != 0;
					cpu->Reg8.L = (cpu->Reg8.L & 0x80) | (cpu->Reg8.L >> 1);
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x2E: //SRA (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp & 0x01) != 0;
					temp = (temp & 0x80) | (temp >> 1);
					cpu->Flags.Zero = (temp == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp);
					break;
				case 0x2F: //SRA A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.A & 0x01) != 0;
					cpu->Reg8.A = (cpu->Reg8.A & 0x80) | (cpu->Reg8.A >> 1);
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x30: //SWAP B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.B = ((cpu->Reg8.B >> 4) | (cpu->Reg8.B << 4));
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x31: //SWAP C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.C = ((cpu->Reg8.C >> 4) | (cpu->Reg8.C << 4));
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x32: //SWAP D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.D = ((cpu->Reg8.D >> 4) | (cpu->Reg8.D << 4));
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x33: //SWAP E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.E = ((cpu->Reg8.E >> 4) | (cpu->Reg8.E << 4));
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x34: //SWAP H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.H = ((cpu->Reg8.H >> 4) | (cpu->Reg8.H << 4));
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x35: //SWAP L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.L = ((cpu->Reg8.L >> 4) | (cpu->Reg8.L << 4));
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x36: //SWAP (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					temp = ((temp >> 4) | (temp << 4)) & 0xFF;
					GB_MemWrite8(cpu->Reg16.HL,temp);
					cpu->Flags.Zero = (temp == 0);
					break;
				case 0x37: //SWAP A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY|F_CARRY);
					cpu->Reg8.A = ((cpu->Reg8.A & 0xF0) >> 4) | ((cpu->Reg8.A & 0x0F) << 4);
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x38: //SRL B
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.B & 0x01) != 0;
					cpu->Reg8.B = cpu->Reg8.B >> 1;
					cpu->Flags.Zero = (cpu->Reg8.B == 0);
					break;
				case 0x39: //SRL C
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.C & 0x01) != 0;
					cpu->Reg8.C = cpu->Reg8.C >> 1;
					cpu->Flags.Zero = (cpu->Reg8.C == 0);
					break;
				case 0x3A: //SRL D
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.D & 0x01) != 0;
					cpu->Reg8.D = cpu->Reg8.D >> 1;
					cpu->Flags.Zero = (cpu->Reg8.D == 0);
					break;
				case 0x3B: //SRL E
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.E & 0x01) != 0;
					cpu->Reg8.E = cpu->Reg8.E >> 1;
					cpu->Flags.Zero = (cpu->Reg8.E == 0);
					break;
				case 0x3C: //SRL H
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.H & 0x01) != 0;
					cpu->Reg8.H = cpu->Reg8.H >> 1;
					cpu->Flags.Zero = (cpu->Reg8.H == 0);
					break;
				case 0x3D: //SRL L
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.L & 0x01) != 0;
					cpu->Reg8.L = cpu->Reg8.L >> 1;
					cpu->Flags.Zero = (cpu->Reg8.L == 0);
					break;
				case 0x3E: //SRL (HL)
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					temp = GB_MemRead8(cpu->Reg16.HL);
					cpu->Flags.Carry = (temp & 0x01) != 0;
					temp = temp >> 1;
					cpu->Flags.Zero = (temp == 0);
					GB_MemWrite8(cpu->Reg16.HL, temp);
					break;
				case 0x3F: //SRL A
					cpu->Reg16.AF &= ~(F_SUBSTRACT|F_HALFCARRY);
					cpu->Flags.Carry = (cpu->Reg8.A & 0x01) != 0;
					cpu->Reg8.A = cpu->Reg8.A >> 1;
					cpu->Flags.Zero = (cpu->Reg8.A == 0);
					break;

				case 0x40: //BIT 0,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<0)) == 0;
					break;
				case 0x41: //BIT 0,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<0)) == 0;
					break;
				case 0x42: //BIT 0,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<0)) == 0;
					break;
				case 0x43: //BIT 0,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<0)) == 0;
					break;
				case 0x44: //BIT 0,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<0)) == 0;
					break;
				case 0x45: //BIT 0,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<0)) == 0;
					break;
				case 0x46: //BIT 0,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<0)) == 0;
					break;
				case 0x47: //BIT 0,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<0)) == 0;
					break;

				case 0x48: //BIT 1,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<1)) == 0;
					break;
				case 0x49: //BIT 1,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<1)) == 0;
					break;
				case 0x4A: //BIT 1,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<1)) == 0;
					break;
				case 0x4B: //BIT 1,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<1)) == 0;
					break;
				case 0x4C: //BIT 1,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<1)) == 0;
					break;
				case 0x4D: //BIT 1,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<1)) == 0;
					break;
				case 0x4E: //BIT 1,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<1)) == 0;
					break;
				case 0x4F: //BIT 1,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<1)) == 0;
					break;
				case 0x50: //BIT 2,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<2)) == 0;
					break;
				case 0x51: //BIT 2,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<2)) == 0;
					break;
				case 0x52: //BIT 2,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<2)) == 0;
					break;
				case 0x53: //BIT 2,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<2)) == 0;
					break;
				case 0x54: //BIT 2,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<2)) == 0;
					break;
				case 0x55: //BIT 2,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<2)) == 0;
					break;
				case 0x56: //BIT 2,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<2)) == 0;
					break;
				case 0x57: //BIT 2,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<2)) == 0;
					break;
				case 0x58: //BIT 3,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<3)) == 0;
					break;
				case 0x59: //BIT 3,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<3)) == 0;
					break;
				case 0x5A: //BIT 3,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<3)) == 0;
					break;
				case 0x5B: //BIT 3,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<3)) == 0;
					break;
				case 0x5C: //BIT 3,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<3)) == 0;
					break;
				case 0x5D: //BIT 3,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<3)) == 0;
					break;
				case 0x5E: //BIT 3,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<3)) == 0;
					break;
				case 0x5F: //BIT 3,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<3)) == 0;
					break;
				case 0x60: //BIT 4,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<4)) == 0;
					break;
				case 0x61: //BIT 4,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<4)) == 0;
					break;
				case 0x62: //BIT 4,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<4)) == 0;
					break;
				case 0x63: //BIT 4,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<4)) == 0;
					break;
				case 0x64: //BIT 4,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<4)) == 0;
					break;
				case 0x65: //BIT 4,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<4)) == 0;
					break;
				case 0x66: //BIT 4,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<4)) == 0;
					break;
				case 0x67: //BIT 4,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<4)) == 0;
					break;
				case 0x68: //BIT 5,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<5)) == 0;
					break;
				case 0x69: //BIT 5,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<5)) == 0;
					break;
				case 0x6A: //BIT 5,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<5)) == 0;
					break;
				case 0x6B: //BIT 5,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<5)) == 0;
					break;
				case 0x6C: //BIT 5,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<5)) == 0;
					break;
				case 0x6D: //BIT 5,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<5)) == 0;
					break;
				case 0x6E: //BIT 5,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<5)) == 0;
					break;
				case 0x6F: //BIT 5,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<5)) == 0;
					break;
				case 0x70: //BIT 6,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<6)) == 0;
					break;
				case 0x71: //BIT 6,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<6)) == 0;
					break;
				case 0x72: //BIT 6,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<6)) == 0;
					break;
				case 0x73: //BIT 6,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<6)) == 0;
					break;
				case 0x74: //BIT 6,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<6)) == 0;
					break;
				case 0x75: //BIT 6,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<6)) == 0;
					break;
				case 0x76: //BIT 6,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<6)) == 0;
					break;
				case 0x77: //BIT 6,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<6)) == 0;
					break;
				case 0x78: //BIT 7,B
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.B & (1<<7)) == 0;
					break;
				case 0x79: //BIT 7,C
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.C & (1<<7)) == 0;
					break;
				case 0x7A: //BIT 7,D
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.D & (1<<7)) == 0;
					break;
				case 0x7B: //BIT 7,E
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.E & (1<<7)) == 0;
					break;
				case 0x7C: //BIT 7,H
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.H & (1<<7)) == 0;
					break;
				case 0x7D: //BIT 7,L
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.L & (1<<7)) == 0;
					break;
				case 0x7E: //BIT 7,(HL)
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (GB_MemRead8(cpu->Reg16.HL) & (1<<7)) == 0;
					break;
				case 0x7F: //BIT 7,A
					cpu->Reg16.AF &= ~F_SUBSTRACT;
					cpu->Reg16.AF |= F_HALFCARRY;
					cpu->Flags.Zero = (cpu->Reg8.A & (1<<7)) == 0;
					break;

				case 0x80: //RES 0,B
					cpu->Reg8.B &= ~(1 << 0);
					break;
				case 0x81: //RES 0,C
					cpu->Reg8.C &= ~(1 << 0);
					break;
				case 0x82: //RES 0,D
					cpu->Reg8.D &= ~(1 << 0);
					break;
				case 0x83: //RES 0,E
					cpu->Reg8.E &= ~(1 << 0);
					break;
				case 0x84: //RES 0,H
					cpu->Reg8.H &= ~(1 << 0);
					break;
				case 0x85: //RES 0,L
					cpu->Reg8.L &= ~(1 << 0);
					break;
				case 0x86: //RES 0,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 0)));
					break;
				case 0x87: //RES 0,A
					cpu->Reg8.A &= ~(1 << 0);
					break;
				case 0x88: //RES 1,B
					cpu->Reg8.B &= ~(1 << 1);
					break;
				case 0x89: //RES 1,C
					cpu->Reg8.C &= ~(1 << 1);
					break;
				case 0x8A: //RES 1,D
					cpu->Reg8.D &= ~(1 << 1);
					break;
				case 0x8B: //RES 1,E
					cpu->Reg8.E &= ~(1 << 1);
					break;
				case 0x8C: //RES 1,H
					cpu->Reg8.H &= ~(1 << 1);
					break;
				case 0x8D: //RES 1,L
					cpu->Reg8.L &= ~(1 << 1);
					break;
				case 0x8E: //RES 1,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 1)));
					break;
				case 0x8F: //RES 1,A
					cpu->Reg8.A &= ~(1 << 1);
					break;
				case 0x90: //RES 2,B
					cpu->Reg8.B &= ~(1 << 2);
					break;
				case 0x91: //RES 2,C
					cpu->Reg8.C &= ~(1 << 2);
					break;
				case 0x92: //RES 2,D
					cpu->Reg8.D &= ~(1 << 2);
					break;
				case 0x93: //RES 2,E
					cpu->Reg8.E &= ~(1 << 2);
					break;
				case 0x94: //RES 2,H
					cpu->Reg8.H &= ~(1 << 2);
					break;
				case 0x95: //RES 2,L
					cpu->Reg8.L &= ~(1 << 2);
					break;
				case 0x96: //RES 2,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 2)));
					break;
				case 0x97: //RES 2,A
					cpu->Reg8.A &= ~(1 << 2);
					break;
				case 0x98: //RES 3,B
					cpu->Reg8.B &= ~(1 << 3);
					break;
				case 0x99: //RES 3,C
					cpu->Reg8.C &= ~(1 << 3);
					break;
				case 0x9A: //RES 3,D
					cpu->Reg8.D &= ~(1 << 3);
					break;
				case 0x9B: //RES 3,E
					cpu->Reg8.E &= ~(1 << 3);
					break;
				case 0x9C: //RES 3,H
					cpu->Reg8.H &= ~(1 << 3);
					break;
				case 0x9D: //RES 3,L
					cpu->Reg8.L &= ~(1 << 3);
					break;
				case 0x9E: //RES 3,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 3)));
					break;
				case 0x9F: //RES 3,A
					cpu->Reg8.A &= ~(1 << 3);
					break;
				case 0xA0: //RES 4,B
					cpu->Reg8.B &= ~(1 << 4);
					break;
				case 0xA1: //RES 4,C
					cpu->Reg8.C &= ~(1 << 4);
					break;
				case 0xA2: //RES 4,D
					cpu->Reg8.D &= ~(1 << 4);
					break;
				case 0xA3: //RES 4,E
					cpu->Reg8.E &= ~(1 << 4);
					break;
				case 0xA4: //RES 4,H
					cpu->Reg8.H &= ~(1 << 4);
					break;
				case 0xA5: //RES 4,L
					cpu->Reg8.L &= ~(1 << 4);
					break;
				case 0xA6: //RES 4,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 4)));
					break;
				case 0xA7: //RES 4,A
					cpu->Reg8.A &= ~(1 << 4);
					break;
				case 0xA8: //RES 5,B
					cpu->Reg8.B &= ~(1 << 5);
					break;
				case 0xA9: //RES 5,C
					cpu->Reg8.C &= ~(1 << 5);
					break;
				case 0xAA: //RES 5,D
					cpu->Reg8.D &= ~(1 << 5);
					break;
				case 0xAB: //RES 5,E
					cpu->Reg8.E &= ~(1 << 5);
					break;
				case 0xAC: //RES 5,H
					cpu->Reg8.H &= ~(1 << 5);
					break;
				case 0xAD: //RES 5,L
					cpu->Reg8.L &= ~(1 << 5);
					break;
				case 0xAE: //RES 5,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 5)));
					break;
				case 0xAF: //RES 5,A
					cpu->Reg8.A &= ~(1 << 5);
					break;
				case 0xB0: //RES 6,B
					cpu->Reg8.B &= ~(1 << 6);
					break;
				case 0xB1: //RES 6,C
					cpu->Reg8.C &= ~(1 << 6);
					break;
				case 0xB2: //RES 6,D
					cpu->Reg8.D &= ~(1 << 6);
					break;
				case 0xB3: //RES 6,E
					cpu->Reg8.E &= ~(1 << 6);
					break;
				case 0xB4: //RES 6,H
					cpu->Reg8.H &= ~(1 << 6);
					break;
				case 0xB5: //RES 6,L
					cpu->Reg8.L &= ~(1 << 6);
					break;
				case 0xB6: //RES 6,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 6)));
					break;
				case 0xB7: //RES 6,A
					cpu->Reg8.A &= ~(1 << 6);
					break;
				case 0xB8: //RES 7,B
					cpu->Reg8.B &= ~(1 << 7);
					break;
				case 0xB9: //RES 7,C
					cpu->Reg8.C &= ~(1 << 7);
					break;
				case 0xBA: //RES 7,D
					cpu->Reg8.D &= ~(1 << 7);
					break;
				case 0xBB: //RES 7,E
					cpu->Reg8.E &= ~(1 << 7);
					break;
				case 0xBC: //RES 7,H
					cpu->Reg8.H &= ~(1 << 7);
					break;
				case 0xBD: //RES 7,L
					cpu->Reg8.L &= ~(1 << 7);
					break;
				case 0xBE: //RES 7,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) & (~(1 << 7)));
					break;
				case 0xBF: //RES 7,A
					cpu->Reg8.A &= ~(1 << 7);
					break;

				case 0xC0: //SET 0,B
					cpu->Reg8.B |= (1 << 0);
					break;
				case 0xC1: //SET 0,C
					cpu->Reg8.C |= (1 << 0);
					break;
				case 0xC2: //SET 0,D
					cpu->Reg8.D |= (1 << 0);
					break;
				case 0xC3: //SET 0,E
					cpu->Reg8.E |= (1 << 0);
					break;
				case 0xC4: //SET 0,H
					cpu->Reg8.H |= (1 << 0);
					break;
				case 0xC5: //SET 0,L
					cpu->Reg8.L |= (1 << 0);
					break;
				case 0xC6: //SET 0,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 0));
					break;
				case 0xC7: //SET 0,A
					cpu->Reg8.A |= (1 << 0);
					break;
				case 0xC8: //SET 1,B
					cpu->Reg8.B |= (1 << 1);
					break;
				case 0xC9: //SET 1,C
					cpu->Reg8.C |= (1 << 1);
					break;
				case 0xCA: //SET 1,D
					cpu->Reg8.D |= (1 << 1);
					break;
				case 0xCB: //SET 1,E
					cpu->Reg8.E |= (1 << 1);
					break;
				case 0xCC: //SET 1,H
					cpu->Reg8.H |= (1 << 1);
					break;
				case 0xCD: //SET 1,L
					cpu->Reg8.L |= (1 << 1);
					break;
				case 0xCE: //SET 1,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 1));
					break;
				case 0xCF: //SET 1,A
					cpu->Reg8.A |= (1 << 1);
					break;
				case 0xD0: //SET 2,B
					cpu->Reg8.B |= (1 << 2);
					break;
				case 0xD1: //SET 2,C
					cpu->Reg8.C |= (1 << 2);
					break;
				case 0xD2: //SET 2,D
					cpu->Reg8.D |= (1 << 2);
					break;
				case 0xD3: //SET 2,E
					cpu->Reg8.E |= (1 << 2);
					break;
				case 0xD4: //SET 2,H
					cpu->Reg8.H |= (1 << 2);
					break;
				case 0xD5: //SET 2,L
					cpu->Reg8.L |= (1 << 2);
					break;
				case 0xD6: //SET 2,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 2));
					break;
				case 0xD7: //SET 2,A
					cpu->Reg8.A |= (1 << 2);
					break;
				case 0xD8: //SET 3,B
					cpu->Reg8.B |= (1 << 3);
					break;
				case 0xD9: //SET 3,C
					cpu->Reg8.C |= (1 << 3);
					break;
				case 0xDA: //SET 3,D
					cpu->Reg8.D |= (1 << 3);
					break;
				case 0xDB: //SET 3,E
					cpu->Reg8.E |= (1 << 3);
					break;
				case 0xDC: //SET 3,H
					cpu->Reg8.H |= (1 << 3);
					break;
				case 0xDD: //SET 3,L
					cpu->Reg8.L |= (1 << 3);
					break;
				case 0xDE: //SET 3,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 3));
					break;
				case 0xDF: //SET 3,A
					cpu->Reg8.A |= (1 << 3);
					break;
				case 0xE0: //SET 4,B
					cpu->Reg8.B |= (1 << 4);
					break;
				case 0xE1: //SET 4,C
					cpu->Reg8.C |= (1 << 4);
					break;
				case 0xE2: //SET 4,D
					cpu->Reg8.D |= (1 << 4);
					break;
				case 0xE3: //SET 4,E
					cpu->Reg8.E |= (1 << 4);
					break;
				case 0xE4: //SET 4,H
					cpu->Reg8.H |= (1 << 4);
					break;
				case 0xE5: //SET 4,L
					cpu->Reg8.L |= (1 << 4);
					break;
				case 0xE6: //SET 4,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 4));
					break;
				case 0xE7: //SET 4,A
					cpu->Reg8.A |= (1 << 4);
					break;
				case 0xE8: //SET 5,B
					cpu->Reg8.B |= (1 << 5);
					break;
				case 0xE9: //SET 5,C
					cpu->Reg8.C |= (1 << 5);
					break;
				case 0xEA: //SET 5,D
					cpu->Reg8.D |= (1 << 5);
					break;
				case 0xEB: //SET 5,E
					cpu->Reg8.E |= (1 << 5);
					break;
				case 0xEC: //SET 5,H
					cpu->Reg8.H |= (1 << 5);
					break;
				case 0xED: //SET 5,L
					cpu->Reg8.L |= (1 << 5);
					break;
				case 0xEE: //SET 5,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 5));
					break;
				case 0xEF: //SET 5,A
					cpu->Reg8.A |= (1 << 5);
					break;
				case 0xF0: //SET 6,B
					cpu->Reg8.B |= (1 << 6);
					break;
				case 0xF1: //SET 6,C
					cpu->Reg8.C |= (1 << 6);
					break;
				case 0xF2: //SET 6,D
					cpu->Reg8.D |= (1 << 6);
					break;
				case 0xF3: //SET 6,E
					cpu->Reg8.E |= (1 << 6);
					break;
				case 0xF4: //SET 6,H
					cpu->Reg8.H |= (1 << 6);
					break;
				case 0xF5: //SET 6,L
					cpu->Reg8.L |= (1 << 6);
					break;
				case 0xF6: //SET 6,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 6));
					break;
				case 0xF7: //SET 6,A
					cpu->Reg8.A |= (1 << 6);
					break;
				case 0xF8: //SET 7,B
					cpu->Reg8.B |= (1 << 7);
					break;
				case 0xF9: //SET 7,C
					cpu->Reg8.C |= (1 << 7);
					break;
				case 0xFA: //SET 7,D
					cpu->Reg8.D |= (1 << 7);
					break;
				case 0xFB: //SET 7,E
					cpu->Reg8.E |= (1 << 7);
					break;
				case 0xFC: //SET 7,H
					cpu->Reg8.H |= (1 << 7);
					break;
				case 0xFD: //SET 7,L
					cpu->Reg8.L |= (1 << 7);
					break;
				case 0xFE: //SET 7,(HL)
					GB_MemWrite8(cpu->Reg16.HL, GB_MemRead8(cpu->Reg16.HL) | (1 << 7));
					break;
				case 0xFF: //SET 7,A
					cpu->Reg8.A |= (1 << 7);
					break;

				default:
					// ...wtf??
                    __gb_break_to_debugger();
					ErrorMessage("Unidentified opcode. CB %x\nPC: %04x\nROM: %d",
                        temp3,GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
					break;
			} //end of inner switch

			GB_CPUClock(clocks_table_cb[temp3] >> GameBoy.Emulator.DoubleSpeed);

			return;
		case 0xCC: //CALL Z,nn
			if(cpu->Flags.Zero == 1)
			{
				cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
				GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC+2);
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(24 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC  += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xCD: //CALL nn
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC+2);
			cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);
			break;
		case 0xCE: //ADC A,n
			cpu->Reg16.AF &= ~F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.PC++);
			temp2 = cpu->Reg8.A + temp + cpu->Flags.Carry;

			cpu->Flags.HalfCarry = ( ((cpu->Reg8.A & 0xF) + (temp & 0xF) ) + cpu->Flags.Carry) > 0xF;
			cpu->Flags.Carry = (temp2 > 0xFF);

			cpu->Reg8.A = (temp2 & 0xFF);
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xCF: //RST 0x08
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0008;
			break;
		case 0xD0: //RET NC
			if(cpu->Flags.Carry == 0)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
				cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;

				GB_CPUClock(20 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xD1: //POP DE
			cpu->Reg16.DE = GB_MemRead16(cpu->Reg16.SP);
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			break;
		case 0xD2: //JP NC,nn
			if(cpu->Flags.Carry == 0)
			{
				cpu->Reg16.PC =  GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(16 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xD3:
            __gb_break_to_debugger();
			ErrorMessage("Unidentified opcode. D3\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xD4: //CALL NC,nn
			if(cpu->Flags.Carry == 0)
			{
				cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
				GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC+2);
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(24 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC  += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xD5: //PUSH DE
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.DE);
			break;
		case 0xD6:  //SUB n
			temp = GB_MemRead8(cpu->Reg16.PC++);
			cpu->Reg16.AF |= F_SUBSTRACT;

			cpu->Flags.HalfCarry = (cpu->Reg8.A & 0xF) < (temp & 0xF);
			cpu->Flags.Carry = (u32)cpu->Reg8.A < temp;

			cpu->Reg8.A -= temp;

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xD7: //RST 0x10
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0010;
			break;
		case 0xD8: //RET C
			if(cpu->Flags.Carry == 1)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
				cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;

				GB_CPUClock(20 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				GB_CPUClock(8 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xD9: //RETI
			cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.SP);
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			GameBoy.Memory.InterruptMasterEnable = 1;
			break;
		case 0xDA: //JP C,nn
			if(cpu->Flags.Carry == 1)
			{
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(16 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;

		case 0xDB:
            __gb_break_to_debugger();
			ErrorMessage("Unidentified opcode. DB\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;

		case 0xDC: //CALL C,nn
			if(cpu->Flags.Carry == 1)
			{
				cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
				GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC+2);
				cpu->Reg16.PC = GB_MemRead16(cpu->Reg16.PC);

				GB_CPUClock(24 >> GameBoy.Emulator.DoubleSpeed);
			}
			else
			{
				cpu->Reg16.PC  += 2;

				GB_CPUClock(12 >> GameBoy.Emulator.DoubleSpeed);
			}
			return;
		case 0xDD:
            __gb_break_to_debugger();
			ErrorMessage("Unidentified opcode. DD\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xDE: //SBC A,n
			temp2 = GB_MemRead8(cpu->Reg16.PC++);
			temp = cpu->Reg8.A - temp2 - ((cpu->Reg8.F&F_CARRY)?1:0);
            cpu->Reg8.F = ((temp & ~0xFF)?F_CARRY:0)|((temp&0xFF)?0:F_ZERO)|F_SUBSTRACT;
			cpu->Flags.HalfCarry = ( (cpu->Reg8.A^temp2^temp) & 0x10 ) != 0 ;
			cpu->Reg8.A = temp;
			break;
		case 0xDF: //RST 0x18
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0018;
			break;
		case 0xE0: //LDH (n),A
			GB_MemWrite8(0xFF00 + (u32)GB_MemRead8(cpu->Reg16.PC++),cpu->Reg8.A);
			break;
		case 0xE1: //POP HL
			cpu->Reg16.HL = GB_MemRead16(cpu->Reg16.SP);
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			break;
		case 0xE2: //LD (C),A
			GB_MemWrite8(0xFF00 + (u32)cpu->Reg8.C,cpu->Reg8.A);
			break;
		case 0xE3:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. E3\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
            break;
		case 0xE4:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. E4\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xE5: //PUSH HL
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.HL);
			break;
		case 0xE6: //AND n
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY);
			cpu->Reg16.AF |= F_HALFCARRY;

			cpu->Reg8.A &= GB_MemRead8(cpu->Reg16.PC++);

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xE7: //RST 0x20
			cpu->Reg16.SP = (cpu->Reg16.SP- 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0020;
			break;
		case 0xE8: //ADD SP,n
            temp = ( GB_MemRead8(cpu->Reg16.PC++) ^ 0x80) - 0x80; //expand sign

            cpu->Reg8.F = 0;
            cpu->Flags.Carry = ( (cpu->Reg16.SP + temp) > 0xFFFF);
            cpu->Flags.HalfCarry = ( ( (cpu->Reg16.SP & 0x000F) + (temp & 0x000F) ) > 0x000F );

            cpu->Reg16.SP = (cpu->Reg16.SP + temp) & 0xFFFF;
			break;
		case 0xE9: //JP HL
			cpu->Reg16.PC = cpu->Reg16.HL;
			break;
		case 0xEA: //LD (nn),A
			GB_MemWrite8(GB_MemRead16(cpu->Reg16.PC),cpu->Reg8.A);
			cpu->Reg16.PC += 2;
			break;
		case 0xEB:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. EB\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
            break;
		case 0xEC:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. EC\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xED:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. ED\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xEE: //XOR n
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);

			cpu->Reg8.A ^= GB_MemRead8(cpu->Reg16.PC++);

			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xEF: //RST 0x28
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0028;
			break;
		case 0xF0: //LDH A,(n)
			cpu->Reg8.A = GB_MemRead8(0xFF00 + (u32)GB_MemRead8(cpu->Reg16.PC++));
			break;
		case 0xF1: //POP AF
			cpu->Reg16.AF = GB_MemRead16(cpu->Reg16.SP) & 0xFFF0;
			cpu->Reg16.SP = (cpu->Reg16.SP + 2) & 0xFFFF;
			break;
		case 0xF2: //LD A,(C)
			cpu->Reg8.A = GB_MemRead8(0xFF00 + (u32)cpu->Reg8.C);
			break;
		case 0xF3: //DI
			GameBoy.Memory.InterruptMasterEnable = 0;
			GameBoy.Memory.interrupts_enable_count = 0;
			break;
		case 0xF4:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. F4\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xF5: //PUSH AF
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.AF);
			break;
		case 0xF6: //OR n
			cpu->Reg16.AF &= ~(F_SUBSTRACT|F_CARRY|F_HALFCARRY);
			cpu->Reg8.A |= GB_MemRead8(cpu->Reg16.PC++);
			cpu->Flags.Zero = (cpu->Reg8.A == 0);
			break;
		case 0xF7: //RST 0x30
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0030;
			break;
		case 0xF8: //LD HL,SP+n | LDHL SP,n
			temp = ( GB_MemRead8(cpu->Reg16.PC++) ^ 0x80) - 0x80; //expand sign

            cpu->Reg16.HL = cpu->Reg16.SP + temp;

            cpu->Reg8.F = 0;
            cpu->Flags.Carry = (cpu->Reg16.HL > 0xFFFF);
            cpu->Flags.HalfCarry = ( ( (cpu->Reg16.SP & 0x000F) + (temp & 0x000F) ) > 0x000F );

            cpu->Reg16.HL &= 0xFFFF;
			break;
		case 0xF9: //LD SP,HL
			cpu->Reg16.SP = cpu->Reg16.HL;
			break;
		case 0xFA: //LD A,(nn)
			cpu->Reg8.A = GB_MemRead8(GB_MemRead16(cpu->Reg16.PC));
			cpu->Reg16.PC += 2;
			break;
		case 0xFB: //EI
			GameBoy.Memory.interrupts_enable_count = 1;
		//	GameBoy.Memory.InterruptMasterEnable = 1;
			break;
		case 0xFC:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. FC\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
            break;
		case 0xFD:
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. FD\nPC: %04x\nROM: %d",
                        GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
		case 0xFE: //CP n
			cpu->Reg16.AF |= F_SUBSTRACT;
			temp = GB_MemRead8(cpu->Reg16.PC++);
			temp2 = cpu->Reg8.A;
			cpu->Flags.HalfCarry = (temp2 & 0xF) < (temp & 0xF);
			cpu->Flags.Carry = (temp2 < temp);
			cpu->Flags.Zero = (temp2 == temp);
			break;
		case 0xFF: //RST 0x38
			cpu->Reg16.SP = (cpu->Reg16.SP - 2) & 0xFFFF;
			GB_MemWrite16(cpu->Reg16.SP,cpu->Reg16.PC);
			cpu->Reg16.PC = 0x0038;
			break;

		default: //wtf...?
            __gb_break_to_debugger();
            ErrorMessage("Unidentified opcode. %x\nPC: %04x\nROM: %d",
                        opcode,GameBoy.CPU.Reg16.PC,GameBoy.Memory.selected_rom);
			break;
	} //end switch

	//cpu->Reg16.PC &= 0xFFFF;

	GB_CPUClock(clocks_table[opcode] >> GameBoy.Emulator.DoubleSpeed);
}

