#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include "gui_screenshot.h"
#include "../../png/save_png.h"
#include "../../build_options.h"

static int screenshot_file_number = 0;

void GBA_Screenshot(void)
{
    mkdir(SCREENSHOT_OUTPUT_FOLDER);
    char completepath[MAXPATHLEN];

    if(GetRunningFolder())
	{
		sprintf(completepath,"%s/%s/",GetRunningFolder(),SCREENSHOT_OUTPUT_FOLDER);
	}
	else
	{
		strcpy(completepath,"./" SCREENSHOT_OUTPUT_FOLDER);
	}

	char filename[MAXPATHLEN];

	while(1)
	{

		sprintf(filename,"%sgba_screenshot%d.png",completepath,screenshot_file_number);

		FILE* file=fopen(filename, "rb");
		if(file == NULL) break; //Ok
		screenshot_file_number ++; //look for next free number
		fclose(file);
	}

    u32 * buffer = malloc(240*160*4);
    GBA_ConvertScreenBufferTo32RGB(buffer);
    Save_PNG(filename,240,160,buffer);
    free(buffer);
}
