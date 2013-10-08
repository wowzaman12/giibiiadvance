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

#ifndef __GAMEBOY__
#define __GAMEBOY__

#include <stdbool.h>
#include "../main.h"

//------------------------------------------------------------------------------
//--                                                                          --
//--                                 HARDWARE                                 --
//--                                                                          --
//------------------------------------------------------------------------------

#define P1_REG		(0xFF00) /* Joystick: 1.1.P15.P14.P13.P12.P11.P10 */
#define SB_REG		(0xFF01) /* Serial IO data buffer */
#define SC_REG		(0xFF02) /* Serial IO control register */
#define DIV_REG		(0xFF04) /* Divider register */
#define TIMA_REG	(0xFF05) /* Timer counter */
#define TMA_REG		(0xFF06) /* Timer modulo */
#define TAC_REG		(0xFF07) /* Timer control */
#define IF_REG		(0xFF0F) /* Interrupt flags: 0.0.0.JOY.SIO.TIM.LCD.VBL */
#define NR10_REG	(0xFF10) /* Sound register */
#define NR11_REG	(0xFF11) /* Sound register */
#define NR12_REG	(0xFF12) /* Sound register */
#define NR13_REG	(0xFF13) /* Sound register */
#define NR14_REG	(0xFF14) /* Sound register */
#define NR21_REG	(0xFF16) /* Sound register */
#define NR22_REG	(0xFF17) /* Sound register */
#define NR23_REG	(0xFF18) /* Sound register */
#define NR24_REG	(0xFF19) /* Sound register */
#define NR30_REG	(0xFF1A) /* Sound register */
#define NR31_REG	(0xFF1B) /* Sound register */
#define NR32_REG	(0xFF1C) /* Sound register */
#define NR33_REG	(0xFF1D) /* Sound register */
#define NR34_REG	(0xFF1E) /* Sound register */
#define NR41_REG	(0xFF20) /* Sound register */
#define NR42_REG	(0xFF21) /* Sound register */
#define NR43_REG	(0xFF22) /* Sound register */
#define NR44_REG	(0xFF23) /* Sound register */
#define NR50_REG	(0xFF24) /* Sound register */
#define NR51_REG	(0xFF25) /* Sound register */
#define NR52_REG	(0xFF26) /* Sound register */
#define LCDC_REG	(0xFF40) /* LCD control */
#define STAT_REG	(0xFF41) /* LCD status */
#define SCY_REG		(0xFF42) /* Scroll Y */
#define SCX_REG		(0xFF43) /* Scroll X */
#define LY_REG		(0xFF44) /* LCDC Y-coordinate */
#define LYC_REG		(0xFF45) /* LY compare */
#define DMA_REG		(0xFF46) /* DMA transfer */
#define BGP_REG		(0xFF47) /* BG palette data */
#define OBP0_REG	(0xFF48) /* OBJ palette 0 data */
#define OBP1_REG	(0xFF49) /* OBJ palette 1 data */
#define WY_REG		(0xFF4A) /* Window Y coordinate */
#define WX_REG		(0xFF4B) /* Window X coordinate */
#define KEY1_REG	(0xFF4D) /* CPU speed */
#define VBK_REG		(0xFF4F) /* VRAM bank */
#define HDMA1_REG	(0xFF51) /* DMA control 1 */
#define HDMA2_REG	(0xFF52) /* DMA control 2 */
#define HDMA3_REG	(0xFF53) /* DMA control 3 */
#define HDMA4_REG	(0xFF54) /* DMA control 4 */
#define HDMA5_REG	(0xFF55) /* DMA control 5 */
#define RP_REG		(0xFF56) /* IR port */
#define BCPS_REG	(0xFF68) /* BG color palette specification */
#define BCPD_REG	(0xFF69) /* BG color palette data */
#define OCPS_REG	(0xFF6A) /* OBJ color palette specification */
#define OCPD_REG	(0xFF6B) /* OBJ color palette data */
#define SVBK_REG	(0xFF70) /* WRAM bank */
#define IE_REG		(0xFFFF) /* Interrupt enable */

//------------------------------------------------------------------------------
//--                                                                          --
//--                                   CPU                                    --
//--                                                                          --
//------------------------------------------------------------------------------

//compiler doesn't like to have the same names here than in gba
#ifdef F_CARRY
#undef F_CARRY
#endif
#ifdef F_ZERO
#undef F_ZERO
#endif

#define F_CARRY     (1<<4)
#define F_HALFCARRY (1<<5)
#define F_SUBSTRACT (1<<6)
#define F_ZERO      (1<<7)

#ifndef PACKED
#define PACKED __attribute__ ((packed))
#endif

#define KEY_A      BIT(0)
#define KEY_B      BIT(1)
#define KEY_SELECT BIT(2)
#define KEY_START  BIT(3)
#define KEY_UP     BIT(4)
#define KEY_RIGHT  BIT(5)
#define KEY_DOWN   BIT(6)
#define KEY_LEFT   BIT(7)

#define KEY_SPEEDUP BIT(10)

typedef struct PACKED {
	u8 F, A; //F can't be accesed by CPU in a normal way
	u8 dummy1[2];
	u8 C, B;
	u8 dummy2[2];
	u8 E, D;
	u8 dummy3[2];
	u8 L, H;
	u8 dummy4[8];
	} _GB_CPU_8BIT_;

typedef struct PACKED {
	u32 AF;
	u32 BC;
	u32 DE;
	u32 HL;
	u32 SP; //Stack Pointer
	u32 PC; //Program Counter
	} _GB_CPU_16BIT_;

typedef struct {
	u8   allwayszero  :4;
	bool Carry        :1;
	bool HalfCarry    :1;
	bool Substract    :1;
	bool Zero         :1;
	u8 dummy[21];
	} _GB_CPU_FLAGS_;


typedef union {
	_GB_CPU_8BIT_  Reg8;
	_GB_CPU_16BIT_ Reg16;
	_GB_CPU_FLAGS_ Flags;
	} _GB_CPU_;

//------------------------------------------------------------------------------
//--                                                                          --
//--                                 MEMORY                                   --
//--                                                                          --
//------------------------------------------------------------------------------

typedef struct PACKED {
	u8 Y;
	u8 X;
	u8 Tile;
	u8 Info;
	} _GB_OAM_ENTRY_;

typedef struct PACKED {
	_GB_OAM_ENTRY_ Sprite[40];
	} _GB_OAM_ ;

typedef void (*mapper_write_fn)(u32,u32);
typedef u32 (*mapper_read_fn)(u32);

typedef struct {
	u8 * ROM_Base;          //0000 | 16KB
	u8 * ROM_Switch[512];   //4000 | 16KB
	u8 VideoRAM[0x4000];    //8000 | 8KB -- 2 banks in GBC - Only 0x2000 needed in gb mode, but
	u8 * ExternRAM[16];     //A000 | 8KB                   | let's allocate that anyway...
	u8 WorkRAM[0x1000];     //C000 | 4KB
	u8 * WorkRAM_Switch[7]; //D000 | 4KB -- 8 banks in GBC -- 0 only accesible from C000-CFFF
	                         //E000 -- ram echo
	u8 ObjAttrMem[0xA0];     //FE00 | (40 * 4) B
	u8 StrangeRAM[0x30];     //FEA0 -- strange RAM - (only GBC, see memory.c for the reason of
	u8 IO_Ports[0x80];       //FF00 | 128B         | 0x30 instead of 0x60)
	u8 HighRAM[0x80];        //FF80 | 128B


	u32 selected_rom, selected_ram;
	u32 selected_wram, selected_vram; //gbc only

	u32 mbc_mode;

	u8 * VideoRAM_Curr; //
	u8 * ROM_Curr;      // Pointers to current banks
	u8 * RAM_Curr;      //
	u8 * WorkRAM_Curr;  // -> only gbc
	u32 RAMEnabled;

	u32 interrupts_enable_count;
	u32 InterruptMasterEnable;

	mapper_write_fn MapperWrite;
    mapper_read_fn MapperRead;
	} _GB_MEMORY_;

//------------------------------------------------------------------------------
//--                                                                          --
//--                             EMULATOR INFO                                --
//--                                                                          --
//------------------------------------------------------------------------------

#define MEM_NONE   (0)
#define MEM_MBC1   (1)
#define MEM_MBC2   (2)
#define MEM_MBC3   (3)
#define MEM_MBC4   (4) //I've never seen a game that uses it...
#define MEM_MBC5   (5)
#define MEM_MBC6   (6)
#define MEM_MBC7   (7)
#define MEM_MMM01  (8)
#define MEM_RUMBLE (9)
#define MEM_HUC1   (10)
#define MEM_HUC3   (11)
#define MEM_CAMERA (12)
#define MEM_TAMA5  (13)

#define JOY_RIGHT  (1<<0)
#define JOY_LEFT   (1<<1)
#define JOY_UP     (1<<2)
#define JOY_DOWN   (1<<3)
#define JOY_A      (1<<0)
#define JOY_B      (1<<1)
#define JOY_SELECT (1<<2)
#define JOY_START  (1<<3)

#define HW_GB   (0)
#define HW_GBP  (1)
#define HW_SGB  (2)
#define HW_SGB2 (3)
#define HW_GBC  (4)
#define HW_GBA  (5)

#define HDMA_NONE    (0)
#define HDMA_GENERAL (1)
#define HDMA_HBLANK  (2)

#define SERIAL_NONE      (0)
#define SERIAL_GBPRINTER (1)
#define SERIAL_GAMEBOY   (2)

typedef void (*draw_scanline_fn_ptr)(s32);

typedef void (*serial_send_fn_ptr)(u32);
typedef u32 (*serial_recv_fn_ptr)(void);

typedef struct {
	u32 sec;
	u32 min;
	u32 hour;
	u32 days;
	u32 carry;
	u32 halt;
	} _GB_MB3_TIMER_;

typedef struct {
	u32 cs; // chip select
	u32 sk; // ?
	u32 state; // mapper state
	u32 buffer; // buffer for receiving serial data
	u32 idle; // idle state
	u32 count; // count of bits received
	u32 code; // command received
	u32 address; // address received
	u32 writeEnable; // write enable
	u32 value; // value to return on ram

	u32 sensorX;
	u32 sensorY;
	} _GB_MB7_CART_;

typedef struct {
	u32 mask;
	u32 offset;
	} _GB_MMM01_CART_;

typedef struct {
	u8 reg[0x36];
	} _GB_CAMERA_CART_;

typedef struct {
	u32 selected_hardware; //HW_*** defines, -1 = auto

	u32 HardwareType; //HW_*** defines
	char Title[17];
	u32 ROM_Banks, RAM_Banks;
	u32 MemoryController; //MBCn, etc...
	u32 HasBattery;
	u32 HasTimer;
	u32 EnableBank0Switch;
	_GB_MB3_TIMER_ Timer;
	_GB_MB3_TIMER_ LatchedTime;
	_GB_MB7_CART_ MBC7;
	_GB_MMM01_CART_ MMM01;
	_GB_CAMERA_CART_ CAM;
	u32 rumble; //rumble enabled
	u32 * Rom_Pointer;
	char * save_filename;

	u8 * boot_rom;
	u32 boot_rom_loaded;
	u32 enable_boot_rom;
    u32 gbc_in_gb_mode;

	//CGB only
	u32 spr_pal[64];
	u32 bg_pal[64];

	u32 lcd_on;
	draw_scanline_fn_ptr DrawScanlineFn;

	u32 HDMAenabled; //HDMA_*** defines
	u32 HBlankHDMAdone; //To limit to 0x10 bytes per HBlank
    int gdma_preparation_time_countdown;

	u32 halt_not_executed;
	u32 CPUHalt;

	u32 SGBEnabled;
	u32 wait_cycles;

	u32 CGBEnabled;
	u32 DoubleSpeed;

	s32 Clocks; //for screen.

	u32 TimerClocks;
	u32 timer_total_clocks;
	u32 timer_enabled;

	u32 DivClocks; // for DIV_REG
	u32 ScreenMode; //for vblank, hblank...
	u32 CurrentScanLine;
	int VBL_clocks_delay; //wait 24 clocks of vblank before vblank interrupt
    //int mode0len; //they depend on the number of sprites displayed in the current scanline
    //int mode3len;

	u32 serial_clocks; //count
	u32 serial_enabled;
	u32 serial_total_clocks;
	u32 serial_device;
	serial_send_fn_ptr SerialSend_Fn;
	serial_recv_fn_ptr SerialRecv_Fn;

	u32 stat_signal; //when this goes from 0 to 1, STAT interrupt is triggered.

	u32 FrameDrawn;
	u32 FrameCount; //This counts the number of frames drawn in this second.
	u32 FPS; //This holds the current framerate
	} _EMULATOR_INFO_;

//------------------------------------------------------------------------------
//--                                                                          --
//--                                CONTEXT                                   --
//--                                                                          --
//------------------------------------------------------------------------------

typedef struct {
	_GB_CPU_ CPU;
	_GB_MEMORY_ Memory;
	_EMULATOR_INFO_ Emulator;
	} _GB_CONTEXT_;

#endif //__GAMEBOY__

