
#include <windows.h>
#include <commctrl.h>
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

#include <stdio.h>
#include <ctype.h>

#include "build_options.h"

#ifdef ENABLE_SPLASH

#include "resource.h"

#include "gui_main.h"
#include "splash.h"

#define IDT_TIMER 123
static LRESULT CALLBACK SplashProcedure(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam)
{
    static HBITMAP bitmap;

    switch(Msg)
    {
        case WM_CREATE:
        {
            bitmap = LoadBitmap(hInstance,MAKEINTRESOURCE(SPLASHDATA));
            break;
        }
        case WM_PAINT:
        {
            PAINTSTRUCT Ps;
            HDC hDC = BeginPaint(hWnd, &Ps);
            // Create a memory device compatible with the above DC variable
            HDC MemDC = CreateCompatibleDC(hDC);
            SelectObject(MemDC, bitmap); // Select the new bitmap
            // Copy the bits from the memory DC into the current dc
            BitBlt(hDC, 0, 0, 720, 405, MemDC, 0, 0, SRCCOPY);
            DeleteDC(MemDC); // Restore the old bitmap
            EndPaint(hWnd, &Ps);
            break;
        }
        case WM_TIMER:
        {
            switch (wParam)
            {
                case IDT_TIMER:
                    DestroyWindow(hWnd);
                    break;
                default:
                    break;
            }
            break;
        }
        case WM_LBUTTONDOWN:
        case WM_RBUTTONDOWN:
        {
            DestroyWindow(hWnd);
            break;
        }
        case WM_DESTROY:
        {
            DeleteObject(bitmap);
            break;
        }
        default:
            return DefWindowProc(hWnd, Msg, wParam, lParam);
    }
    return 0;
}

void GLWindow_SplashScreen(void)
{
    HWND    hWnd;
	WNDCLASSEX  WndClsEx;

	// Create the application window
	WndClsEx.cbSize        = sizeof(WNDCLASSEX);
	WndClsEx.style         = CS_HREDRAW | CS_VREDRAW;
	WndClsEx.lpfnWndProc   = SplashProcedure;
	WndClsEx.cbClsExtra    = 0;
	WndClsEx.cbWndExtra    = 0;
	WndClsEx.hIcon         = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));
	WndClsEx.hCursor       = LoadCursor(NULL, IDC_ARROW);
	WndClsEx.hbrBackground = NULL;
	WndClsEx.lpszMenuName  = NULL;
	WndClsEx.lpszClassName = "Class_SplashScreen";
	WndClsEx.hInstance     = hInstance;
	WndClsEx.hIconSm       = LoadIcon(hInstance, MAKEINTRESOURCE(MY_ICON));

	// Register the application
	RegisterClassEx(&WndClsEx);

	// Create the window object
	hWnd = CreateWindow("Class_SplashScreen",
			  SPLASH_WINDOW_CAPTION,
			  WS_POPUPWINDOW,
			  CW_USEDEFAULT,
			  CW_USEDEFAULT,
			  720,
			  405,
			  NULL,
			  NULL,
			  hInstance,
			  NULL);

	if(!hWnd) return;

	RECT rc;
    GetWindowRect(GetDesktopWindow(), &rc);
    rc.left = (rc.right-720)/2;
    rc.right = rc.left + 720;
    rc.top = (rc.bottom-405)/2;
    rc.bottom = rc.top + 405;
    MoveWindow(hWnd, rc.left, rc.top, rc.right-rc.left, rc.bottom-rc.top, FALSE);

    SetTimer(hWnd,IDT_TIMER,4000,(TIMERPROC) NULL); //4-second countdown

    ShowWindow(hWnd, SW_SHOWNORMAL);
	UpdateWindow(hWnd);
}

#endif // ENABLE_SPLASH
