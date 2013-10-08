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

#include <png.h>

#include <malloc.h>

#include "build_options.h"

#include "main.h"

void png_warn_fn_(png_structp sp, png_const_charp cp)
{
    ErrorMessage("libpng warning:\r\n\r\n%s",cp);
}

void png_err_fn_(png_structp sp, png_const_charp cp)
{
    ErrorMessage("libpng error:\r\n\r\n%s",cp);
}

int Save_PNG(const char * file_name, int width, int height, void * buffer) //buffer is 32 bit
{
    FILE *fp = fopen(file_name, "wb");
    if(fp == NULL)
    {
        ErrorMessage("Couldn't open file for writing: %s", file_name);
        return 1;
    }

    png_structp png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
                                                    NULL, png_err_fn_, png_warn_fn_);
    if(png_ptr == NULL)
    {
        ErrorMessage("libpng error: png_ptr == NULL");
        fclose(fp);
        return 1;
    }

    png_infop info_ptr = png_create_info_struct(png_ptr);
    if(info_ptr == NULL)
    {
        ErrorMessage("libpng error: info_ptr == NULL");
        fclose(fp);
        png_destroy_write_struct(&png_ptr,  NULL);
        return 1;
    }

    if(setjmp(png_jmpbuf(png_ptr)))
    {
        ErrorMessage("libpng error: setjmp(png_jmpbuf(png_ptr)) != 0");
        fclose(fp);
        png_destroy_write_struct(&png_ptr, &info_ptr);
        return 1;
    }

    png_init_io(png_ptr, fp);

    png_set_IHDR(png_ptr, info_ptr, width, height, 8, PNG_COLOR_TYPE_RGB,
        PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, PNG_FILTER_TYPE_BASE);

    png_write_info(png_ptr, info_ptr);

    png_bytep row_pointers[height];

    int k, i;
    for(k = 0; k < height; k++) row_pointers[k] = malloc(width*3);

    unsigned char * buf = (unsigned char*)buffer;
    for(k = 0; k < height; k++) for(i = 0; i < width; i++)
    {
        row_pointers[k][(i*3)+0] = *buf++;
        row_pointers[k][(i*3)+1] = *buf++;
        row_pointers[k][(i*3)+2] = *buf++;
        buf++; //ignore alpha
    }

    png_write_image(png_ptr, row_pointers);

    png_write_end(png_ptr, info_ptr);

    png_destroy_write_struct(&png_ptr, &info_ptr);

    for(k = 0; k < height; k++) free(row_pointers[k]);

    fclose(fp);

    return 0;
}

