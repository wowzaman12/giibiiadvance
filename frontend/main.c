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

//#ifdef WIN32
//#include <windows.h>
//#endif
#include <stdio.h>
#include <ctype.h>
#include <stdarg.h>

#include "../build_options.h"

#include "config.h"
#include "../gba_core/gba.h"
#include "../gba_core/cpu.h"
#include "../gba_core/interrupts.h"
#include "../gba_core/video.h"
#include "../gba_core/memory.h"
#include "../gba_core/sound.h"

//--------------------------------------------------
char console_buffer[5000];

void ConsoleReset(void)
{
    console_buffer[0] = '\0';
}

void ConsolePrint(const char * msg, ...)
{
    va_list args;
	char buffer[1024];

	va_start(args,msg);
	vsnprintf(buffer, sizeof(buffer), msg, args);
	va_end(args);

    strcat(console_buffer,buffer);
}

void ConsoleShow(void)
{
//#ifdef WIN32
//    MessageBox(NULL, console_buffer, "Console", MB_OK);
//#endif
}


//-------------------------------------------------



void memset_rand(u8 * start, u32 size)
{
	while(size--) *start++ = rand();
}

u64 asciihextoint(const char * text)
{
    long int value = 0, i = 0;
    while(1)
    {
        char char_ = toupper(text[i++]); //end of string
        if(char_ == '\0') return value;
        else if(char_ >= '0' && char_ <= '9') value = (value <<4) + (char_ - '0');
        else if(char_ >= 'A' && char_ <= 'F') value = (value <<4) +  (char_ - 'A' + 10);
        else return 0xFFFFFFFFFFFFFFFFULL;
    }
}

u64 asciidectoint(const char * text)
{
    long int value = 0, i = 0;
    while(1)
    {
        char char_ = toupper(text[i++]); //end of string
        if(char_ == '\0') return value;
        else if(char_ >= '0' && char_ <= '9') value = (value*10) + (char_ - '0');
        else return 0xFFFFFFFFFFFFFFFFULL;
    }
}

int MESSAGE_SHOWING = 0;

void DebugMessage(const char * msg, ...)
{
    va_list args;
	char buffer[1024];

	va_start(args,msg);
	vsnprintf(buffer, sizeof(buffer), msg, args);
	va_end(args);

    if(EmulatorConfig.debug_msg_enable) {
        MESSAGE_SHOWING = 1;
        //#ifdef WIN32
       // MessageBox(NULL, buffer, "Debug", MB_OK);
       // #endif
        MESSAGE_SHOWING = 0;
    }
}

void ErrorMessage(const char * msg, ...)
{
    va_list args;
	char buffer[1024];

	va_start(args,msg);
	vsnprintf(buffer, sizeof(buffer), msg, args);
	va_end(args);

    MESSAGE_SHOWING = 1;
    //MessageBox(NULL, buffer, "Error", MB_OK|MB_ICONSTOP);
    MESSAGE_SHOWING = 0;
}


void FileLoad_NoError(const char * filename, void ** buffer, unsigned int * size_)
{
	FILE * datafile = fopen(filename, "rb");
	unsigned int size;
	*buffer = NULL;
	if(size_) *size_ = 0;

	if(datafile == NULL)
		return;

	fseek(datafile, 0, SEEK_END);
	size = ftell(datafile);
	if(size_) *size_ = size;

	if(size == 0)
	{
		if (datafile != NULL)
			fclose(datafile);

		return;
	}

	rewind(datafile);
	*buffer = malloc(size);

	if(*buffer == NULL)
	{
		fclose(datafile);
		return;
	}

	if(fread(*buffer,size,1,datafile) != 1)
	{
		fclose(datafile);
		return;
	}

	fclose(datafile);
}


void FileLoad(const char * filename, void ** buffer, unsigned int * size_)
{
	FILE * datafile = fopen(filename, "rb");
	unsigned int size;
	*buffer = NULL;
	if(size_) *size_ = 0;

	if(datafile == NULL)
	{
	    char msg[2048];
	    sprintf(msg,"%s couldn't be opened!",filename);
	   // #ifdef WIN32
	  //  MessageBox(NULL, msg, "File open error", MB_OK);
	   // #endif
        return;
    }

    fseek(datafile, 0, SEEK_END);
	size = ftell(datafile);
	if(size_) *size_ = size;
	if(size == 0)
	{
	    char msg[2048];
	    sprintf(msg,"Size of %s is 0!",filename);
	    //MessageBox(NULL, msg, "File open error", MB_OK);
        fclose(datafile);
        return;
    }
	rewind(datafile);
	*buffer = malloc(size);
	if(*buffer == NULL)
	{
	    char msg[2048];
	    sprintf(msg,"Not enought memory to load %s!",filename);
	    //#ifdef WIN32
	    //MessageBox(NULL, msg, "File open error", MB_OK);
	    //#endif
        fclose(datafile);
        return;
    }
	if(fread(*buffer,size,1,datafile) != 1)
	{
	    //#ifdef WIN32
	   //MessageBox(NULL, "Error while reading.", "File open error", MB_OK);
	   // #endif
        fclose(datafile);
        return;
    }

	fclose(datafile);
}



/*

void Debug_Show_WINAPI_LastError(void)
{
    LPTSTR errorText = NULL;

    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER|FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,GetLastError(),MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),(LPTSTR)&errorText,0,NULL);

    if(errorText != NULL)
    {
        DebugMessage(errorText);
        LocalFree(errorText);
    }
}

//----------------------------------

char texto[128], cad[64];
case WM_KEYDOWN:
GetKeyNameText(lParam, cad, 64);
sprintf(texto, "Tecla %s (%d) pulsada", cad, (int)wParam);
break;
*/

