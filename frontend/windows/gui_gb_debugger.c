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

#include <windows.h>
#include <commctrl.h>

#include <stdio.h>
#include <ctype.h>

#include "../../build_options.h"
#include "resource.h"

#include "gui_mainloop.h"
#include "gui_main.h"
#include "gui_gb_debugger.h"
#include "../../gb_core/gameboy.h"
#include "../../gb_core/memory.h"
#include "../../gb_core/cpu.h"
#include "../../gb_core/debug.h"

extern _GB_CONTEXT_ GameBoy;

//-----------------------------------------------------------------------------------------
//                                   DISASSEMBLY
//-----------------------------------------------------------------------------------------

static HWND hWndDisassemblyWindow;
static int DisassemblerCreated = 0;
static HWND hwndDisassembly, hwndRegisters, hwndStack;

#define CPU_DISASSEMBLER_MAX_INSTRUCTIONS (30)
#define CPU_STACK_MAX_LINES               (18)

#define IDC_CPU_DISASSEMBLY  1
#define IDC_CPU_REGISTERS    2
#define IDC_CPU_STACK        3

#define ID_CPU_STEP 20 //button

void GLWindow_GBDisassemblerStep(void)
{
    if(RUNNING != RUN_GB) return;

    GB_CPUStep();
}

void GLWindow_GBDisassemblerUpdate(void)
{
    if(DisassemblerCreated == 0) return;

    if(RUNNING != RUN_GB) return;

    //REGISTERS
    char text[32];
    SendMessage(hwndRegisters, LB_RESETCONTENT, 0, 0);
    sprintf(text,"AF: %04X",GameBoy.CPU.Reg16.AF);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"BC: %04X",GameBoy.CPU.Reg16.BC);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"DE: %04X",GameBoy.CPU.Reg16.DE);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"HL: %04X",GameBoy.CPU.Reg16.HL);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"PC: %04X",GameBoy.CPU.Reg16.PC);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"SP: %04X",GameBoy.CPU.Reg16.SP);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);

    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)" ");
    int flags = GameBoy.CPU.Reg16.AF;
    sprintf(text,"C:%d H:%d", (flags&F_CARRY) != 0, (flags&F_HALFCARRY) != 0);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);
    sprintf(text,"N:%d Z:%d", (flags&F_SUBSTRACT) != 0, (flags&F_ZERO) != 0);
    SendMessage(hwndRegisters, LB_ADDSTRING, 0, (LPARAM)text);

    //DISASSEMBLER
#if 0
    //DOESN'T WORK
    address = GameBoy.CPU.Reg16.PC;
    while(address > 0x0001)
    {
        //If there are 2 possible instructions that are 1 byte long, the next byte is an instruction.
        if(debug_command_param_size[GB_MemRead8(address-1)] == 1)
        {
            if(debug_command_param_size[GB_MemRead8(address-2)] == 1) break;
            address -= 2;
        }
        else address -= 1;
    }
#endif

    u16 address = 0x0000; //Dissasemble everytime from the beggining... :S
    if(GameBoy.CPU.Reg16.PC > 0x0120) address = 0x0100;
    if(GameBoy.CPU.Reg16.PC > 0x4020) address = 0x4000;

    if(GameBoy.CPU.Reg16.PC > 10)
    {
        int start_address = GameBoy.CPU.Reg16.PC - 24;
        if(start_address < 0) start_address = 0;

        while(address < start_address)
            address += gb_debug_get_address_increment(address);

        while(1) //To fix cursor at one line.
        {
            int tempaddr = address, commands = 0;
            while(tempaddr < GameBoy.CPU.Reg16.PC)
            {
                commands ++;
                tempaddr += gb_debug_get_address_increment(tempaddr);
            }

            if(commands < 15) break;
            address += gb_debug_get_address_increment(address);
        }
    }

    SendMessage(hwndDisassembly, LB_RESETCONTENT, 0, 0);

    char opcode_text[128];
    char final_text[156];

    int i;
    for(i = 0; i < CPU_DISASSEMBLER_MAX_INSTRUCTIONS; i++)
    {
        int step;
        strcpy(opcode_text,GB_Dissasemble(address,&step));
        sprintf(final_text,"%04X:%s",address,opcode_text);
        SendMessage(hwndDisassembly, LB_ADDSTRING, 0, (LPARAM)final_text);

        if(address == GameBoy.CPU.Reg16.PC) SendMessage(hwndDisassembly, LB_SETCURSEL, i, 0);

        address += step;
    }

    SendMessage(hwndStack, LB_RESETCONTENT, 0, 0);
    address = GameBoy.CPU.Reg16.SP-16;
    for(i = 0; i < CPU_STACK_MAX_LINES; i++)
    {
        sprintf(final_text,"%04X:%04X",address,GB_MemRead16(address));
        SendMessage(hwndStack, LB_ADDSTRING, 0, (LPARAM)final_text);

        if(address == GameBoy.CPU.Reg16.SP) SendMessage(hwndStack, LB_SETCURSEL, i, 0);

        address += 2;
    }
}

static LRESULT CALLBACK DisassemblerProcedure(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    static HFONT hFont, hFontFixed;

    switch(Msg)
    {
        case WM_CREATE:
        {
            hFont = CreateFont(15,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, DEFAULT_PITCH, NULL);

            HWND hBtnStep = CreateWindow(TEXT("button"), TEXT("Step F7"),
                WS_CHILD | WS_VISIBLE,
                257, 8, 55, 25, hWnd, (HMENU)ID_CPU_STEP , hInstance, NULL);
            SendMessage(hBtnStep, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));

            hwndDisassembly = CreateWindow(TEXT("listbox"), NULL,
                WS_CHILD|WS_VISIBLE|WS_BORDER|LBS_NOTIFY,
                9, 8, 240, 470, hWnd,(HMENU) IDC_CPU_DISASSEMBLY, hInstance, NULL);

            hwndRegisters = CreateWindow(TEXT("listbox"), NULL,
                WS_CHILD|WS_VISIBLE|WS_BORDER|LBS_NOTIFY,
                257, 40, 80, 150, hWnd,(HMENU)IDC_CPU_REGISTERS, hInstance, NULL);

            hwndStack = CreateWindow(TEXT("listbox"), NULL,
                WS_CHILD|WS_VISIBLE|WS_BORDER|LBS_NOTIFY,
                257, 186, 80, 275, hWnd,(HMENU)IDC_CPU_STACK, hInstance, NULL);

            hFontFixed = CreateFont(15,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, FIXED_PITCH, NULL);

            SendMessage(hwndDisassembly, WM_SETFONT, (WPARAM)hFontFixed, MAKELPARAM(1, 0));
            SendMessage(hwndRegisters, WM_SETFONT, (WPARAM)hFontFixed, MAKELPARAM(1, 0));
            SendMessage(hwndStack, WM_SETFONT, (WPARAM)hFontFixed, MAKELPARAM(1, 0));
            //SendMessage(hwndRegisters, WM_SETFONT, GetStockObject(ANSI_FIXED_FONT), 0);

            GLWindow_GBDisassemblerUpdate();
            break;
        }
        case WM_KEYDOWN:
        {
            //if(GetAsyncKeyState(VK_F7) & 0x8000){
            if(wParam == VK_F7) GLWindow_GBDisassemblerStep();

            GLWindow_GBDisassemblerUpdate();
            break;
        }
        case WM_COMMAND:
        {
            switch(HIWORD(wParam))
            {
                case BN_CLICKED:
                    if(LOWORD(wParam) == ID_CPU_STEP)
                    {
                        GLWindow_GBDisassemblerStep();
                    }
                    SetFocus(hWnd);
                    GLWindow_GBDisassemblerUpdate();
                    break;
                //case LBN_DBLCLK:
                //case LBN_SELCHANGE:
                //    if(LOWORD(wParam) == IDC_CPU_REGISTERS)
                //    {
                //        char text[12];
                //        int sel = SendMessage(hwndRegisters, LB_GETCURSEL, 0, 0);
                //        sprintf(text,"%d",sel);
                //        MessageBox(NULL, text, "asd", MB_OK);
                //    }
                //    break;
                case LBN_SELCHANGE:
                    SetFocus(hWnd);
                case LBN_SETFOCUS:
                    GLWindow_GBDisassemblerUpdate();
                    break;
                default:
                    break;
            }
            break;
        }
        case WM_LBUTTONDOWN:
        case WM_SETFOCUS:
            GLWindow_GBDisassemblerUpdate();
            break;
        //case WM_MOUSEWHEEL:
        //{
        //    short zDelta = (short)HIWORD(wParam);
        //    int increment = -(zDelta/WHEEL_DELTA);
        //    disassembler_start_address += increment * 2;
        //    GLWindow_GBDisassemblerUpdate();
        //    break;
        //}
        case WM_DESTROY:
            DisassemblerCreated = 0;
            DeleteObject(hFont);
            DeleteObject(hFontFixed);
            break;
        default:
            return DefWindowProc(hWnd, Msg, wParam, lParam);
    }

    return 0;
}

void GLWindow_GBCreateDissasembler(void)
{
    if(DisassemblerCreated) { SetActiveWindow(hWndDisassemblyWindow); return; }
    DisassemblerCreated = 1;

	HWND    hWnd;
	WNDCLASSEX  WndClsEx;

	// Create the application window
	WndClsEx.cbSize        = sizeof(WNDCLASSEX);
	WndClsEx.style         = CS_HREDRAW | CS_VREDRAW;
	WndClsEx.lpfnWndProc   = DisassemblerProcedure;
	WndClsEx.cbClsExtra    = 0;
	WndClsEx.cbWndExtra    = 0;
	WndClsEx.hIcon         = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));
	WndClsEx.hCursor       = LoadCursor(NULL, IDC_ARROW);
	WndClsEx.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1);
	WndClsEx.lpszMenuName  = NULL;
	WndClsEx.lpszClassName = "Class_GBDisassembly";
	WndClsEx.hInstance     = hInstance;
	WndClsEx.hIconSm       = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));

	// Register the application
	RegisterClassEx(&WndClsEx);

	// Create the window object
	hWnd = CreateWindow("Class_GBDisassembly", "Disassembly",
			  WS_BORDER | WS_CAPTION | WS_SYSMENU,
			  CW_USEDEFAULT,CW_USEDEFAULT, //Position
			  350, 500, //Size
			  hWndMain, NULL, hInstance, NULL);

	if(!hWnd)
	{
	    DisassemblerCreated = 0;
	    return;
	}

	ShowWindow(hWnd, SW_SHOWNORMAL);
	UpdateWindow(hWnd);

	hWndDisassemblyWindow = hWnd;
}

void GLWindow_GBCloseDissasembler(void)
{
    if(DisassemblerCreated) SendMessage(hWndDisassemblyWindow, WM_CLOSE, 0, 0);
}

//-----------------------------------------------------------------------------------------
//                                   MEMORY VIEWER
//-----------------------------------------------------------------------------------------

static int MemViewerCreated = 0;
static HWND hWndMemList;
static HWND hWndMemListViewer;
u16 MemStartAddress = 0;

#define MEM_LINES (16)
#define MEM_ADDRESS_JUMP_LINE (32)

#define IDC_MEM_VIEW          1
#define IDC_MEM_START_ADDRES  2

#define ID_8BIT   20
#define ID_16BIT  21
u32 mem_view_mode;

const RECT rc_list_8 = { 5, 37, 760, 280 };
const RECT rc_list_16 = { 5, 37, 640, 280 };

void GLWindow_GBMemViewerUpdate(void)
{
    if(MemViewerCreated == 0) return;

    if(RUNNING != RUN_GB) return;

    RECT rc;
    GetWindowRect(hWndMemListViewer, &rc);
    rc.bottom = rc.top + 315;

    u16 addr = MemStartAddress - (MemStartAddress%MEM_ADDRESS_JUMP_LINE);

    SendMessage(hWndMemList, LB_RESETCONTENT, 0, 0);
    if(mem_view_mode == ID_16BIT)
    {
        rc.right = rc.left + 650;
        MoveWindow(hWndMemList, rc_list_16.left, rc_list_16.top, rc_list_16.right-rc_list_16.left, rc_list_16.bottom-rc_list_16.top, TRUE);
        int i;
        for(i = 0; i < MEM_LINES; i++)
        {
            char text[128];
            snprintf(text,256,"%04X : %04X %04X %04X %04X %04X %04X %04X %04X  %04X %04X %04X %04X %04X %04X %04X %04X",addr,
                GB_MemRead16(addr),GB_MemRead16(addr+2),GB_MemRead16(addr+4),
                GB_MemRead16(addr+6),GB_MemRead16(addr+8),GB_MemRead16(addr+10),
                GB_MemRead16(addr+12),GB_MemRead16(addr+14),GB_MemRead16(addr+16),
                GB_MemRead16(addr+18),GB_MemRead16(addr+20),GB_MemRead16(addr+22),
                GB_MemRead16(addr+24),GB_MemRead16(addr+26),GB_MemRead16(addr+28),
                GB_MemRead16(addr+30));
            addr += MEM_ADDRESS_JUMP_LINE;
            SendMessage(hWndMemList, LB_ADDSTRING, 0, (LPARAM)text);
        }
    }
    else //if(mem_view_mode == ID_8BIT)
    {
        rc.right = rc.left + 770;
        MoveWindow(hWndMemList, rc_list_8.left, rc_list_8.top, rc_list_8.right-rc_list_8.left, rc_list_8.bottom-rc_list_8.top, TRUE);
        int i;
        for(i = 0; i < MEM_LINES; i++)
        {
            char text[128];
            snprintf(text,256,"%04X : %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X"
            "   %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X",addr,
                GB_MemRead8(addr),GB_MemRead8(addr+1),GB_MemRead8(addr+2),
                GB_MemRead8(addr+3),GB_MemRead8(addr+4),GB_MemRead8(addr+5),
                GB_MemRead8(addr+6),GB_MemRead8(addr+7),GB_MemRead8(addr+8),
                GB_MemRead8(addr+9),GB_MemRead8(addr+10),GB_MemRead8(addr+11),
                GB_MemRead8(addr+12),GB_MemRead8(addr+13),GB_MemRead8(addr+14),
                GB_MemRead8(addr+15),GB_MemRead8(addr+16),GB_MemRead8(addr+17),
                GB_MemRead8(addr+18),GB_MemRead8(addr+19),GB_MemRead8(addr+20),
                GB_MemRead8(addr+21),GB_MemRead8(addr+22),GB_MemRead8(addr+23),
                GB_MemRead8(addr+24),GB_MemRead8(addr+25),GB_MemRead8(addr+26),
                GB_MemRead8(addr+27),GB_MemRead8(addr+28),GB_MemRead8(addr+29),
                GB_MemRead8(addr+30),GB_MemRead8(addr+31));
            addr += MEM_ADDRESS_JUMP_LINE;
            SendMessage(hWndMemList, LB_ADDSTRING, 0, (LPARAM)text);
        }
    }
    MoveWindow(hWndMemListViewer, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, TRUE);
    //ShowWindow(hWndMemListViewer, SW_SHOWNORMAL);
    //UpdateWindow(hWndMemListViewer);
}

static LRESULT CALLBACK MemViewerProcedure(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    static HWND hEdit, hwndStatic;
    static HFONT hFont, hFontFixed;
    switch(Msg)
    {
        case WM_CREATE:
        {
            HWND hGroup = CreateWindow(TEXT("button"), NULL,
                WS_CHILD | BS_GROUPBOX, 0, 0, 0, 0, hWnd, (HMENU) 0, hInstance, NULL);
            HWND hBtn1 = CreateWindow(TEXT("button"), TEXT("8 bit"),
                WS_CHILD | WS_VISIBLE | BS_AUTORADIOBUTTON,
                200, 5, 50, 30, hWnd, (HMENU)ID_8BIT , hInstance, NULL);
            HWND hBtn2 = CreateWindow(TEXT("button"), TEXT("16 bit"),
                WS_CHILD | WS_VISIBLE | BS_AUTORADIOBUTTON,
                260, 5, 50, 30, hWnd, (HMENU)ID_16BIT , hInstance, NULL);

            CheckDlgButton(hWnd,ID_16BIT,BST_CHECKED);
            mem_view_mode = ID_16BIT;

            hFont = CreateFont(15,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, DEFAULT_PITCH, NULL);

            SendMessage(hGroup, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));
            SendMessage(hBtn1, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));
            SendMessage(hBtn2, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));

            hEdit = CreateWindow("edit",NULL,WS_CHILD | WS_VISIBLE | WS_BORDER, //ES_READONLY
                55, 9, 50, 20, hWnd,(HMENU)IDC_MEM_START_ADDRES, hInstance, NULL);
            SendMessage(hEdit, WM_SETTEXT, 0, (LPARAM)"0000");
            SetFocus(hEdit);

            SendMessage(hEdit, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));

            hwndStatic = CreateWindow(TEXT("static"), TEXT("Go to:"),
                  WS_CHILD | WS_VISIBLE,
                  10, 11, 40, 20, hWnd, NULL, hInstance, NULL);
            //SetWindowText(hwndStatic, buff);
            SendMessage(hwndStatic, WM_SETFONT, (WPARAM)hFont, MAKELPARAM(1, 0));

            hWndMemList = CreateWindow(TEXT("listbox") , NULL,
                WS_CHILD|WS_VISIBLE|WS_BORDER|LBS_NOTIFY,//WS_VSCROLL
                5, 37, 590, 280, hWnd,(HMENU)IDC_MEM_VIEW, hInstance, NULL);

            SendMessage(hWndMemList, LB_SETCURSEL, -1, 0);

            hFontFixed = CreateFont(15,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, FIXED_PITCH, NULL);

            SendMessage(hWndMemList, WM_SETFONT, (WPARAM)hFontFixed, MAKELPARAM(1, 0));

            GLWindow_GBMemViewerUpdate();
            break;
        }
        case WM_KEYDOWN:
        {
            int increment = 0;

            if(wParam == VK_DOWN)
            {
                MemStartAddress += MEM_ADDRESS_JUMP_LINE;
            }
            else if(wParam == VK_UP)
            {
                MemStartAddress -= MEM_ADDRESS_JUMP_LINE;
            }

            MemStartAddress += increment;
            char addrtext[10];
            sprintf(addrtext,"%04X",MemStartAddress);
            SendMessage(hEdit, WM_SETTEXT, 0, (LPARAM)addrtext);
            break;
        }
        case WM_COMMAND:
        {
            switch(HIWORD(wParam))
            {
                case BN_CLICKED:
                    mem_view_mode = LOWORD(wParam);
                    GLWindow_GBMemViewerUpdate();
                    break;
                case EN_CHANGE:
                    if(LOWORD(wParam) == IDC_MEM_START_ADDRES)
                    {
                        TCHAR text[32];
                        GetWindowText(hEdit, text, 32);
                        u64 val = asciihextoint(text);
                        if(val != 0xFFFFFFFFFFFFFFFFULL) MemStartAddress = val;
                        GLWindow_GBMemViewerUpdate();
                    }
                    break;
                case LBN_SELCHANGE:
                case LBN_SETFOCUS:
                    SetFocus(hWnd);
                    GLWindow_GBMemViewerUpdate();
                    break;
            }
            //SendMessage(hWndMemList, LB_SETCURSEL, -1, 0);
            break;
        }
        case WM_LBUTTONDOWN:
        case WM_SETFOCUS:
            GLWindow_GBMemViewerUpdate();
            break;
        case WM_MOUSEWHEEL:
        {
            short zDelta = (short)HIWORD(wParam);
            int increment = -(zDelta/WHEEL_DELTA);
            MemStartAddress += increment * MEM_ADDRESS_JUMP_LINE;
            char addrtext[10];
            sprintf(addrtext,"%04X",MemStartAddress);
            SendMessage(hEdit, WM_SETTEXT, 0, (LPARAM)addrtext);
            break;
        }
        case WM_DESTROY:
            MemViewerCreated = 0;
            DeleteObject(hFont);
            DeleteObject(hFontFixed);
            break;
        default:
            return DefWindowProc(hWnd, Msg, wParam, lParam);
    }

    return 0;
}

void GLWindow_GBCreateMemViewer(void)
{
    if(MemViewerCreated) { SetActiveWindow(hWndMemListViewer); return; }
    MemViewerCreated = 1;

    HWND    hWnd;
	WNDCLASSEX  WndClsEx;

	// Create the application window
	WndClsEx.cbSize        = sizeof(WNDCLASSEX);
	WndClsEx.style         = CS_HREDRAW | CS_VREDRAW;
	WndClsEx.lpfnWndProc   = MemViewerProcedure;
	WndClsEx.cbClsExtra    = 0;
	WndClsEx.cbWndExtra    = 0;
	WndClsEx.hIcon         = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));
	WndClsEx.hCursor       = LoadCursor(NULL, IDC_ARROW);
	WndClsEx.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1);
	WndClsEx.lpszMenuName  = NULL;
	WndClsEx.lpszClassName = "Class_GBMemView";
	WndClsEx.hInstance     = hInstance;
	WndClsEx.hIconSm       = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));

	// Register the application
	RegisterClassEx(&WndClsEx);

	// Create the window object
	hWnd = CreateWindow("Class_GBMemView",
			  "Memory Viewer",
			  WS_BORDER | WS_CAPTION | WS_SYSMENU,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  650,
			  315,
			  hWndMain,
			  NULL,
			  hInstance,
			  NULL);

	if(!hWnd)
	{
	    MemViewerCreated = 0;
	    return;
	}

	ShowWindow(hWnd, SW_SHOWNORMAL);
	UpdateWindow(hWnd);

	hWndMemListViewer = hWnd;
}

void GLWindow_GBCloseMemViewer(void)
{
    if(MemViewerCreated) SendMessage(hWndMemListViewer, WM_CLOSE, 0, 0);
}

//-----------------------------------------------------------------------------------------
//                                   IO REGISTERS
//-----------------------------------------------------------------------------------------

static HWND hWndIOViewer;
#define IO_VIEWER_NUM_ITEMS 200
static int IOViewerCreated = 0;
static HWND hItem[IO_VIEWER_NUM_ITEMS];
static HFONT hFontIO, hFontNormalIO, hFontFixedIO;

static LRESULT CALLBACK IOViewerProcedure(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

void GLWindow_GBIOViewerUpdate(void)
{
    if(IOViewerCreated == 0) return;

    if(RUNNING != RUN_GB) return;

    #define SET_CHECK(item,condition) \
    SendMessage(hItem[item], BM_SETCHECK, ((condition)?BST_CHECKED:BST_UNCHECKED), 0)

    #define SET_REG(item,value) \
    { sprintf(text,"%02X",value); SetWindowText(hItem[item],(LPCTSTR)text); }

    char text[5];

    //SCREEN
    SET_REG(2,GB_MemReadReg8(0xFF40)); SET_REG(4,GB_MemReadReg8(0xFF41));
    SET_REG(6,GB_MemReadReg8(0xFF42)); SET_REG(8,GB_MemReadReg8(0xFF43));
    SET_REG(10,GB_MemReadReg8(0xFF44)); SET_REG(12,GB_MemReadReg8(0xFF45));
    SET_REG(14,GB_MemReadReg8(0xFF46)); SET_REG(16,GB_MemReadReg8(0xFF4A));
    SET_REG(18,GB_MemReadReg8(0xFF4B));

    //GB PALETTES
    SET_REG(21,GB_MemReadReg8(0xFF47)); SET_REG(23,GB_MemReadReg8(0xFF48));
    SET_REG(25,GB_MemReadReg8(0xFF49));

    //GBC PALETTES
    SET_REG(28,GB_MemReadReg8(0xFF68)); SET_REG(30,GB_MemReadReg8(0xFF69));
    SET_REG(32,GB_MemReadReg8(0xFF6A)); SET_REG(34,GB_MemReadReg8(0xFF6B));

    //OTHER
    SET_REG(37,GB_MemReadReg8(0xFF00)); SET_REG(39,GB_MemReadReg8(0xFF01));
    SET_REG(41,GB_MemReadReg8(0xFF02)); SET_REG(43,GB_MemReadReg8(0xFF04));
    SET_REG(45,GB_MemReadReg8(0xFF05)); SET_REG(47,GB_MemReadReg8(0xFF06));
    SET_REG(49,GB_MemReadReg8(0xFF07)); SET_REG(51,GB_MemReadReg8(0xFF0F));
    SET_REG(53,GB_MemReadReg8(0xFF4D)); SET_REG(55,GB_MemReadReg8(0xFF4F));
    SET_REG(57,GB_MemReadReg8(0xFF56)); SET_REG(59,GB_MemReadReg8(0xFF70));
    SET_REG(61,GB_MemRead8(0xFFFF));

    //GBC DMA
    SET_REG(64,GB_MemReadReg8(0xFF51)); SET_REG(66,GB_MemReadReg8(0xFF52));
    SET_REG(68,GB_MemReadReg8(0xFF53)); SET_REG(70,GB_MemReadReg8(0xFF54));
    SET_REG(72,GB_MemReadReg8(0xFF55));

    //SND CHANNEL 1
    SET_REG(75,GB_MemReadReg8(0xFF10)); SET_REG(77,GB_MemReadReg8(0xFF11));
    SET_REG(79,GB_MemReadReg8(0xFF12)); SET_REG(81,GB_MemReadReg8(0xFF13));
    SET_REG(83,GB_MemReadReg8(0xFF14));

    //SND CHANNEL 2
    SET_REG(86,GB_MemReadReg8(0xFF16)); SET_REG(88,GB_MemReadReg8(0xFF17));
    SET_REG(90,GB_MemReadReg8(0xFF18)); SET_REG(92,GB_MemReadReg8(0xFF19));

    //SND CHANNEL 3
    SET_REG(95,GB_MemReadReg8(0xFF1A)); SET_REG(97,GB_MemReadReg8(0xFF1B));
    SET_REG(99,GB_MemReadReg8(0xFF1C)); SET_REG(101,GB_MemReadReg8(0xFF1D));
    SET_REG(103,GB_MemReadReg8(0xFF1E));

    //SND CHANNEL 4
    SET_REG(106,GB_MemReadReg8(0xFF20)); SET_REG(108,GB_MemReadReg8(0xFF21));
    SET_REG(110,GB_MemReadReg8(0xFF22)); SET_REG(112,GB_MemReadReg8(0xFF23));

    //SND CONTROL
    SET_REG(115,GB_MemReadReg8(0xFF24)); SET_REG(117,GB_MemReadReg8(0xFF25));
    SET_REG(119,GB_MemReadReg8(0xFF26));

    //DMA INFORMATION
    u8 * __io = GameBoy.Memory.IO_Ports;
    u32 source = (__io[HDMA1_REG-0xFF00]<<8) | __io[HDMA2_REG-0xFF00];
	u32 dest = ((__io[HDMA3_REG-0xFF00]<<8) | __io[HDMA4_REG-0xFF00]) + 0x8000;
	u32 size = (((__io[HDMA5_REG-0xFF00]<<8)+1)&0x7F)<<4;
    SetWindowText(hItem[122],(LPCTSTR)((GameBoy.Emulator.HDMAenabled==0) ? "NONE" :
                    ( (GameBoy.Emulator.HDMAenabled==1) ? "GDMA" : "HDMA" )) );
    sprintf(text,"%04X",source); SetWindowText(hItem[124],(LPCTSTR)text);
    sprintf(text,"%04X",dest); SetWindowText(hItem[126],(LPCTSTR)text);
    sprintf(text,"%04X",size); SetWindowText(hItem[128],(LPCTSTR)text);

    //MBC
    sprintf(text,"%d",GameBoy.Emulator.MemoryController);
    SetWindowText(hItem[131],(LPCTSTR)text);
    sprintf(text,"%d(%x)",GameBoy.Memory.selected_rom,GameBoy.Memory.selected_rom);
    SetWindowText(hItem[133],(LPCTSTR)text);
    sprintf(text,"%d(%x)-%s",GameBoy.Memory.selected_ram,GameBoy.Memory.selected_ram,
            GameBoy.Memory.RAMEnabled?"E":"D");
    SetWindowText(hItem[135],(LPCTSTR)text);
    sprintf(text,"%d",GameBoy.Memory.mbc_mode); SetWindowText(hItem[137],(LPCTSTR)text);

    //CPU SPEED
    SetWindowText(hItem[139],(LPCTSTR)
        ((GameBoy.Emulator.DoubleSpeed == 1) ? "Speed: 8Mhz (double)" : "Speed: 4Mhz(normal)") );

    //IRQS
    sprintf(text,"%d",GameBoy.Memory.InterruptMasterEnable); SetWindowText(hItem[142],(LPCTSTR)text);
    int __ie = GB_MemRead8(IE_REG);
    int __if = GB_MemRead8(IF_REG);
    SET_CHECK(146,__ie&1); SET_CHECK(147,__if&1);
    SET_CHECK(149,__ie&2); SET_CHECK(150,__if&2);
    SET_CHECK(152,__ie&4); SET_CHECK(153,__if&4);
    SET_CHECK(155,__ie&8); SET_CHECK(156,__if&8);
    SET_CHECK(158,__ie&16); SET_CHECK(159,__if&16);

    //CLOCKS
    sprintf(text,"%d",GameBoy.Emulator.Clocks); SetWindowText(hItem[162],(LPCTSTR)text);
    sprintf(text,"%d",GameBoy.Emulator.TimerClocks); SetWindowText(hItem[164],(LPCTSTR)text);
    sprintf(text,"%d-%s",GameBoy.Emulator.timer_total_clocks,GameBoy.Emulator.timer_enabled ? "E":"D");
    SetWindowText(hItem[166],(LPCTSTR)text);
    sprintf(text,"%d",GameBoy.Emulator.DivClocks); SetWindowText(hItem[168],(LPCTSTR)text);
    sprintf(text,"%d",GameBoy.Emulator.serial_clocks); SetWindowText(hItem[170],(LPCTSTR)text);
    sprintf(text,"%d-%s",GameBoy.Emulator.serial_total_clocks,GameBoy.Emulator.serial_enabled ? "E":"D");
    SetWindowText(hItem[172],(LPCTSTR)text);
}

static void GLWindow_IOAtCreation(HWND hWnd)
{
    #define CREATE_REG_STATIC_TEXT(baseid,basex,basey,text) \
    { \
        hItem[baseid] = CreateWindow(TEXT("static"), TEXT(text), WS_CHILD | WS_VISIBLE, \
                        basex+24, basey+1, 70, 13, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0)); \
        hItem[baseid+1] = CreateWindow(TEXT("static"), TEXT("FF"), \
                        WS_CHILD | WS_VISIBLE | SS_SUNKEN | BS_CENTER, \
                        basex, basey, 20, 17, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid+1], WM_SETFONT, (WPARAM)hFontFixedIO, MAKELPARAM(1, 0)); \
    }

    #define CREATE_MEDIUM_STATIC_TEXT(baseid,basex,basey,text) \
    { \
        hItem[baseid] = CreateWindow(TEXT("static"), TEXT(text), WS_CHILD | WS_VISIBLE, \
                        basex+46, basey+1, 30, 13, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0)); \
        hItem[baseid+1] = CreateWindow(TEXT("static"), TEXT("----"), \
                        WS_CHILD | WS_VISIBLE | SS_SUNKEN | BS_CENTER, \
                        basex, basey, 40, 17, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid+1], WM_SETFONT, (WPARAM)hFontFixedIO, MAKELPARAM(1, 0)); \
    }

    #define CREATE_LONG_STATIC_TEXT(baseid,basex,basey,text) \
    { \
        hItem[baseid] = CreateWindow(TEXT("static"), TEXT(text), WS_CHILD | WS_VISIBLE, \
                        basex+64, basey+1, 30, 13, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0)); \
        hItem[baseid+1] = CreateWindow(TEXT("static"), TEXT("------"), \
                        WS_CHILD | WS_VISIBLE | SS_SUNKEN | BS_CENTER, \
                        basex, basey, 60, 17, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid+1], WM_SETFONT, (WPARAM)hFontFixedIO, MAKELPARAM(1, 0)); \
    }

    //SCREEN
    hItem[0] = CreateWindow(TEXT("button"), TEXT("Screen"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                5, 2, 105, 183, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[0], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(1, 11, 19, "FF40h LCDC");
    CREATE_REG_STATIC_TEXT(3, 11, 37, "FF41h STAT");
    CREATE_REG_STATIC_TEXT(5, 11, 55, "FF42h SCY");
    CREATE_REG_STATIC_TEXT(7, 11, 73, "FF43h SCX");
    CREATE_REG_STATIC_TEXT(9, 11, 91, "FF44h LY");
    CREATE_REG_STATIC_TEXT(11, 11, 109, "FF45h LYC");
    CREATE_REG_STATIC_TEXT(13, 11, 127, "FF46h DMA");
    CREATE_REG_STATIC_TEXT(15, 11, 145, "FF4Ah WY");
    CREATE_REG_STATIC_TEXT(17, 11, 163, "FF4Bh WX");

    //GB PALETTES
    hItem[19] = CreateWindow(TEXT("button"), TEXT("GB Palettes"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                5, 185, 105, 75, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[19], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(20, 11, 202, "FF47h BGP");
    CREATE_REG_STATIC_TEXT(22, 11, 220, "FF48h OBP0");
    CREATE_REG_STATIC_TEXT(24, 11, 238, "FF49h OBP1");

    //GBC PALETTES
    hItem[26] = CreateWindow(TEXT("button"), TEXT("GBC Palettes"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                5, 260, 105, 94, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[26], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(27, 11, 277, "FF68h BCPS");
    CREATE_REG_STATIC_TEXT(29, 11, 295, "FF69h BCPD");
    CREATE_REG_STATIC_TEXT(31, 11, 313, "FF6Ah OCPS");
    CREATE_REG_STATIC_TEXT(33, 11, 331, "FF6Bh OCPD");

    //OTHER
    hItem[35] = CreateWindow(TEXT("button"), TEXT("Other"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                116, 2, 105, 255, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[35], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(36, 122, 19, "FF00h P1");
    CREATE_REG_STATIC_TEXT(38, 122, 37, "FF01h SB");
    CREATE_REG_STATIC_TEXT(40, 122, 55, "FF02h SC");
    CREATE_REG_STATIC_TEXT(42, 122, 73, "FF04h DIV");
    CREATE_REG_STATIC_TEXT(44, 122, 91, "FF05h TIMA");
    CREATE_REG_STATIC_TEXT(46, 122, 109, "FF06h TMA");
    CREATE_REG_STATIC_TEXT(48, 122, 127, "FF07h TAC");
    CREATE_REG_STATIC_TEXT(50, 122, 145, "FF0Fh IF");
    CREATE_REG_STATIC_TEXT(52, 122, 163, "FF4Dh KEY1");
    CREATE_REG_STATIC_TEXT(54, 122, 181, "FF4Fh VBK");
    CREATE_REG_STATIC_TEXT(56, 122, 199, "FF56h RP");
    CREATE_REG_STATIC_TEXT(58, 122, 217, "FF70h SVBK");
    CREATE_REG_STATIC_TEXT(60, 122, 235, "FFFFh IE");

    //GBC DMA
    hItem[62] = CreateWindow(TEXT("button"), TEXT("GBC DMA"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                116, 257, 105, 107, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[62], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(63, 122, 274, "FF51h SRCH");
    CREATE_REG_STATIC_TEXT(65, 122, 291, "FF52h SRCL");
    CREATE_REG_STATIC_TEXT(67, 122, 308, "FF53h DSTH");
    CREATE_REG_STATIC_TEXT(69, 122, 325, "FF54h DSTL");
    CREATE_REG_STATIC_TEXT(71, 122, 342, "FF55h CTRL");

    //SND CHANNEL 1
    hItem[73] = CreateWindow(TEXT("button"), TEXT("Snd Channel 1"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                227, 2, 105, 111, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[73], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(74, 233, 19, "FF10h NR10");
    CREATE_REG_STATIC_TEXT(76, 233, 37, "FF11h NR11");
    CREATE_REG_STATIC_TEXT(78, 233, 55, "FF12h NR12");
    CREATE_REG_STATIC_TEXT(80, 233, 73, "FF13h NR13");
    CREATE_REG_STATIC_TEXT(82, 233, 91, "FF14h NR14");

    //SND CHANNEL 2
    hItem[84] = CreateWindow(TEXT("button"), TEXT("Snd Channel 2"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                338, 2, 105, 111, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[84], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(85, 344, 37, "FF16h NR21");
    CREATE_REG_STATIC_TEXT(87, 344, 55, "FF17h NR22");
    CREATE_REG_STATIC_TEXT(89, 344, 73, "FF18h NR23");
    CREATE_REG_STATIC_TEXT(91, 344, 91, "FF19h NR24");

    //SND CHANNEL 3
    hItem[93] = CreateWindow(TEXT("button"), TEXT("Snd Channel 3"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                227, 115, 105, 111, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[93], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(94, 233, 132, "FF1Ah NR30");
    CREATE_REG_STATIC_TEXT(96, 233, 150, "FF1Bh NR31");
    CREATE_REG_STATIC_TEXT(98, 233, 168, "FF1Ch NR32");
    CREATE_REG_STATIC_TEXT(100, 233, 186, "FF1Dh NR33");
    CREATE_REG_STATIC_TEXT(102, 233, 204, "FF1Eh NR34");

    //SND CHANNEL 4
    hItem[104] = CreateWindow(TEXT("button"), TEXT("Snd Channel 4"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                338, 115, 105, 111, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[104], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(105, 344, 150, "FF20h NR41");
    CREATE_REG_STATIC_TEXT(107, 344, 168, "FF21h NR42");
    CREATE_REG_STATIC_TEXT(109, 344, 186, "FF22h NR43");
    CREATE_REG_STATIC_TEXT(111, 344, 204, "FF23h NR44");

    //SND CONTROL
    hItem[113] = CreateWindow(TEXT("button"), TEXT("Snd Control"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                449, 2, 105, 75, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[113], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(114, 455, 19, "FF24h NR50");
    CREATE_REG_STATIC_TEXT(116, 455, 37, "FF25h NR51");
    CREATE_REG_STATIC_TEXT(118, 455, 55, "FF26h NR52");

    //DMA INFORMATION
    hItem[120] = CreateWindow(TEXT("button"), TEXT("DMA Info."),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                227, 228, 105, 93, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[120], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_MEDIUM_STATIC_TEXT(121,233,245,"MODE");
    CREATE_MEDIUM_STATIC_TEXT(123,233,263,"SRC");
    CREATE_MEDIUM_STATIC_TEXT(125,233,281,"DEST");
    CREATE_MEDIUM_STATIC_TEXT(127,233,299,"SIZE");

    //MBC
    hItem[129] = CreateWindow(TEXT("button"), TEXT("MBC"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                338, 228, 105, 93, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[129], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_LONG_STATIC_TEXT(130,344,245,"MBC");
    CREATE_LONG_STATIC_TEXT(132,344,263,"ROM");
    CREATE_LONG_STATIC_TEXT(134,344,281,"RAM");
    CREATE_LONG_STATIC_TEXT(136,344,299,"MODE");

    //CPU SPEED
    hItem[138] = CreateWindow(TEXT("button"), TEXT("CPU Speed"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                227, 322, 216, 42, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[138], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));
    hItem[139] = CreateWindow(TEXT("static"), TEXT("--------------"), \
                    WS_CHILD | WS_VISIBLE | SS_SUNKEN | BS_CENTER, \
                    233, 340, 204, 17, hWnd, NULL, hInstance, NULL); \
    SendMessage(hItem[139], WM_SETFONT, (WPARAM)hFontFixedIO, MAKELPARAM(1, 0)); \

    //IRQS
    #define CREATE_2_CHECK_STATIC(baseid, basex, basey, text) \
    { \
        hItem[baseid] = CreateWindow(TEXT("static"), TEXT(text), WS_CHILD | WS_VISIBLE, \
                        basex, basey+1, 35, 13, hWnd, NULL, hInstance, NULL); \
        hItem[baseid+1] = CreateWindow(TEXT("button"), NULL, WS_CHILD | WS_VISIBLE | BS_CHECKBOX, \
                        basex+45, basey, 12, 17, hWnd, NULL, hInstance, NULL); \
        hItem[baseid+2] = CreateWindow(TEXT("button"), NULL, WS_CHILD | WS_VISIBLE | BS_CHECKBOX, \
                        basex+65, basey, 12, 17, hWnd, NULL, hInstance, NULL); \
        SendMessage(hItem[baseid], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0)); \
    }

    hItem[140] = CreateWindow(TEXT("button"), TEXT("IRQs"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                449, 79, 105, 127, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[140], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_REG_STATIC_TEXT(141, 455,96,"IME");
    hItem[143] = CreateWindow(TEXT("static"), TEXT("IE"), WS_CHILD | WS_VISIBLE,
                    507, 112, 10, 13, hWnd, NULL, hInstance, NULL);
    SendMessage(hItem[143], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0));
    hItem[144] = CreateWindow(TEXT("static"), TEXT("IF"), WS_CHILD | WS_VISIBLE,
                    527, 112, 10, 13, hWnd, NULL, hInstance, NULL);
    SendMessage(hItem[144], WM_SETFONT, (WPARAM)hFontIO, MAKELPARAM(1, 0));

    CREATE_2_CHECK_STATIC(145,460,125,"VBL");
    CREATE_2_CHECK_STATIC(148,460,140,"STAT");
    CREATE_2_CHECK_STATIC(151,460,155,"TIMER");
    CREATE_2_CHECK_STATIC(154,460,170,"SERIAL");
    CREATE_2_CHECK_STATIC(157,460,185,"JOY");

    //CLOCKS
    hItem[160] = CreateWindow(TEXT("button"), TEXT("Clocks"),
                WS_CHILD | WS_VISIBLE | BS_GROUPBOX,
                449, 208, 105, 154, hWnd, (HMENU) 0, hInstance, NULL);
    SendMessage(hItem[160], WM_SETFONT, (WPARAM)hFontNormalIO, MAKELPARAM(1, 0));

    CREATE_LONG_STATIC_TEXT(161,455,225,"SCR");
    CREATE_LONG_STATIC_TEXT(163,455,252,"Timer");
    CREATE_LONG_STATIC_TEXT(165,455,269,"  total");
    CREATE_LONG_STATIC_TEXT(167,455,296,"DIV");
    CREATE_LONG_STATIC_TEXT(169,455,323,"Serial");
    CREATE_LONG_STATIC_TEXT(171,455,340,"  total");
}

static LRESULT CALLBACK IOViewerProcedure(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    switch(Msg)
    {
        case WM_CREATE:
        {
            hFontIO = CreateFont(14,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, DEFAULT_PITCH, NULL);
            hFontNormalIO = CreateFont(15,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, DEFAULT_PITCH, NULL);
            hFontFixedIO = CreateFont(14,0,0,0, FW_REGULAR, 0, 0, 0, ANSI_CHARSET,
                OUT_DEFAULT_PRECIS,CLIP_DEFAULT_PRECIS,PROOF_QUALITY, FIXED_PITCH, NULL);

            GLWindow_IOAtCreation(hWnd);

            GLWindow_GBIOViewerUpdate();
            break;
        }
        case WM_LBUTTONDOWN:
        case WM_SETFOCUS:
            GLWindow_GBIOViewerUpdate();
            break;
        case WM_DESTROY:
        {
            IOViewerCreated = 0;
            DeleteObject(hFontIO);
            DeleteObject(hFontNormalIO);
            DeleteObject(hFontFixedIO);
            break;
        }
        default:
            return DefWindowProc(hWnd, Msg, wParam, lParam);
    }

    return 0;
}

void GLWindow_GBCreateIOViewer(void)
{
    if(IOViewerCreated) { SetActiveWindow(hWndIOViewer); return; }
    IOViewerCreated = 1;

    HWND    hWnd;
	WNDCLASSEX  WndClsEx;

	// Create the application window
	WndClsEx.cbSize        = sizeof(WNDCLASSEX);
	WndClsEx.style         = CS_HREDRAW | CS_VREDRAW;
	WndClsEx.lpfnWndProc   = IOViewerProcedure;
	WndClsEx.cbClsExtra    = 0;
	WndClsEx.cbWndExtra    = 0;
	WndClsEx.hIcon         = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));
	WndClsEx.hCursor       = LoadCursor(NULL, IDC_ARROW);
	WndClsEx.hbrBackground = (HBRUSH)(COLOR_BTNFACE+1);
	WndClsEx.lpszMenuName  = NULL;
	WndClsEx.lpszClassName = "Class_GBIOView";
	WndClsEx.hInstance     = hInstance;
	WndClsEx.hIconSm       = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));

	// Register the application
	RegisterClassEx(&WndClsEx);

	// Create the window object
	hWnd = CreateWindow("Class_GBIOView",
			  "I/O Viewer",
			  WS_BORDER | WS_CAPTION | WS_SYSMENU,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  565,
			  398,
			  hWndMain,
			  NULL,
			  hInstance,
			  NULL);

	if(!hWnd)
	{
	    IOViewerCreated = 0;
	    return;
	}

	ShowWindow(hWnd, SW_SHOWNORMAL);
	UpdateWindow(hWnd);

	hWndIOViewer = hWnd;
}

void GLWindow_GBCloseIOViewer(void)
{
    if(IOViewerCreated) SendMessage(hWndIOViewer, WM_CLOSE, 0, 0);
}

