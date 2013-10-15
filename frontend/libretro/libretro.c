#include "libretro.h"
#include "../../build_options.h"

#include "../config.h"
#include "../../gba_core/gba.h"
#include "../../gba_core/cpu.h"
#include "../../gba_core/interrupts.h"
#include "../../gba_core/video.h"
#include "../../gba_core/memory.h"
#include "../../gba_core/sound.h"

unsigned retro_api_version(void) { return RETRO_API_VERSION; }

static retro_video_refresh_t video_cb;
static retro_input_poll_t input_poll_cb;
static retro_input_state_t input_state_cb;
static retro_environment_t environ_cb;
static retro_audio_sample_batch_t audio_batch_cb;

void retro_set_environment(retro_environment_t cb)
{
	environ_cb = cb;
	cb(RETRO_ENVIRONMENT_SET_VARIABLES, (void*)vars);
}

void retro_set_video_refresh(retro_video_refresh_t cb) { video_cb = cb; }
void retro_set_audio_sample(retro_audio_sample_t cb) { (void)cb; }
void retro_set_audio_sample_batch(retro_audio_sample_batch_t cb) { audio_batch_cb = cb; }
void retro_set_input_poll(retro_input_poll_t cb) { input_poll_cb = cb; }
void retro_set_input_state(retro_input_state_t cb) { input_state_cb = cb; }

struct bind_conv
{
	int retro;
	//int gba;
}

static struct bind_conv binds[] = {
/*	{ RETRO_DEVICE_ID_JOYPAD_B },
	{ RETRO_DEVICE_ID_JOYPAD_A },
	{ RETRO_DEVICE_ID_JOYPAD_START },
	{ RETRO_DEVICE_ID_JOYPAD_L },
	{ RETRO_DEVICE_ID_JOYPAD_R },
	{ RETRO_DEVICE_ID_JOYPAD_UP },
	{ RETRO_DEVICE_ID_JOYPAD_DOWN },
	{ RETRO_DEVICE_ID_JOYPAD_LEFT },
	{ RETRO_DEVICE_ID_JOYPAD_RIGHT },
	{ RETRO_DEVICE_ID_JOYPAD_SELECT }, */
};

void retro_get_system_info(struct retro_system_info *info)
{
/*   info->library_name = "Giibii Advance";
   info->library_version = "v1.0.0";
   info->valid_extensions = "gba|bin|agp";
   info->block_extract = false;
   info->need_fullpath = true; */
}

static bool LoadFile(char * filename)
{
	
}

bool retro_load_game(const struct retro_game_info *info)
{
	
}

