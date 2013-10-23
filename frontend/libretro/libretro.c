#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "libretro.h"
#include "../../build_options.h"

#include "../main.h"
#include "../config.h"
#include "../../gba_core/gba.h"
#include "../../gba_core/save.h"
#include "../../gba_core/bios.h"
#include "../../gba_core/sound.h"

static retro_video_refresh_t video_cb;
static retro_input_poll_t input_poll_cb;
static retro_input_state_t input_state_cb;
static retro_environment_t environ_cb;
static retro_audio_sample_batch_t audio_batch_cb;

unsigned retro_api_version(void) { return RETRO_API_VERSION; }

void retro_set_video_refresh(retro_video_refresh_t cb) { video_cb = cb; }
void retro_set_audio_sample(retro_audio_sample_t cb) { (void)cb; }
void retro_set_audio_sample_batch(retro_audio_sample_batch_t cb) { audio_batch_cb = cb; }
void retro_set_input_poll(retro_input_poll_t cb) { input_poll_cb = cb; }
void retro_set_input_state(retro_input_state_t cb) { input_state_cb = cb; }

void retro_get_system_info(struct retro_system_info *info)
{
	info->library_name = "Giibii Advance";
	info->library_version = "v1.0.0";
	info->valid_extensions = "gba|bin|agp";
	info->block_extract = false;
	info->need_fullpath = true;
}

#define CHUNKSIZE   (0x10000)
#define DEFAULT_PATH "/"
static char g_rom_dir[1024];

static void extract_directory(char *buf, const char *path, size_t size)
{
	char *base;
	strncpy(buf, path, size - 1);
	buf[size - 1] = '\0';

	base = strrchr(buf, '/');
	if (!base)
		base = strrchr(buf, '\\');

	if (base)
		*base = '\0';
	else
		buf[0] = '\0';
}

int load_archive(char *filename, unsigned char *buffer, int maxsize, char *extension)
{
	int size = 0;
	char in[CHUNKSIZE];
	char msg[64] = "Unable to open file";

	/* Open file */
	FILE *fd = fopen(filename, "rb");


	if (!fd)
	{
		fprintf(stderr, "ERROR - %s.\n", msg);
		return 0;
	}

	/* Read first chunk */
	fread(in, CHUNKSIZE, 1, fd);

	{
	int left;
	/* Get file size */
	fseek(fd, 0, SEEK_END);
	size = ftell(fd);
	fseek(fd, 0, SEEK_SET);

	/* size limit */
	if(size > maxsize)
	{
		fclose(fd);
		fprintf(stderr, "ERROR - File is too large.\n");
		return 0;
	}

	sprintf((char *)msg,"Loading %d bytes ...", size);
	fprintf(stderr, "INFORMATION - %s\n", msg);

	/* filename extension */
	if (extension)
	{
		memcpy(extension, &filename[strlen(filename) - 3], 3);
		extension[3] = 0;
	}

	/* Read into buffer */
	left = size;
	while (left > CHUNKSIZE)
	{
		fread(buffer, CHUNKSIZE, 1, fd);
		buffer += CHUNKSIZE;
		left -= CHUNKSIZE;
	}

	/* Read remaining bytes */
		fread(buffer, left, 1, fd);
	}

	/* Close file */
	fclose(fd);

	/* Return loaded ROM size */
	return size;
}

bool retro_load_game(const struct retro_game_info *info)
{
	const char *full_path;
	const char *dir;
	char slash;

	extract_directory(g_rom_dir, info->path, sizeof(g_rom_dir));

	if (!environ_cb(RETRO_ENVIRONMENT_GET_SYSTEM_DIRECTORY, &dir) || !dir)
	{
		fprintf(stderr, "[giibiiadvance]: Defaulting system directory to %s.\n", g_rom_dir);
		dir = g_rom_dir;
	}
#if defined(_WIN32)
	slash = '\\';
#else
	slash = '/';
#endif

	snprintf(DEFAULT_PATH, sizeof(DEFAULT_PATH), "%s", g_rom_dir);
	//config_default();
	//init_bitmap();
	full_path = info->path;

	init_audio();
	
	system_init();
	system_reset();

	//return TRUE; 
} 

void retro_init(void)
{
#ifdef FRONTEND_SUPPORTS_RGB565
	rgb565 = RETRO_PIXEL_FORMAT_RGB565;
	if(environ_cb(RETRO_ENVIRONMENT_SET_PIXEL_FORMAT, &rgb565))
		fprintf(stderr, "Frontend supports RGB565 - will use that instead of XRGB1555.\n");
#endif
}

void retro_deinit(void)
{
	
}

void retro_reset(void) { system_reset(); }

void retro_run(void) 
{
	
}
