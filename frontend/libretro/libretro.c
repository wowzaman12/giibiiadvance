#include "libretro.h"
#include "../../build_options.h"

#include "../config.h"
#include "../../gba_core/gba.h"
#include "../../gba_core/cpu.h"
#include "../../gba_core/interrupts.h"
#include "../../gba_core/video.h"
#include "../../gba_core/memory.h"
#include "../../gba_core/sound.h"
#include "../../gba_core/dma.h"
#include "../../gba_core/disassembler.h"
#include "../../gba_core/rom.h"
#include "../../gba_core/save.h"
#include "../../gba_core/timers.h"

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

void retro_init(void)
{
}


void retro_deinit(void)
{}
unsigned retro_api_version(void)
{
	return RETRO_API_VERSION;
}

void retro_set_controller_port_device(unsigned port, unsigned device)
{
	(void)port;
	(void)device;
}
void retro_get_system_info(struct retro_system_info *info)
{
   info->library_name = "Giibii Advance";
   info->library_version = "v1.0.0";
   info->valid_extensions = "gba|bin|agp";
   info->block_extract = false;
   info->need_fullpath = true;
}

void retro_set_audio_sample(retro_audio_sample_t cb)
{
	audio_cb = cb;
}

void retro_set_audio_sample_batch(retro_audio_sample_batch_t cb)
{
	audio_batch_cb = cb;
}

void retro_set_input_poll(retro_input_poll_t cb)
{
	input_poll_cb = cb;
}

void retro_set_input_state(retro_input_state_t cb)
{
	input_state_cb = cb;
}

void retro_set_video_refresh(retro_video_refresh_t cb)
{
	video_cb = cb;
}

void retro_reset(void)
{}

void retro_run(void)
{
	
}

bool retro_load_game(const struct retro_game_info *info)
{
	
}

void retro_unload_game(void)
{
	
}

size_t retro_serialize_size(void)
{
	return 0;
}

bool retro_serialize(void *data, size_t size)
{
	(void)data;
	(void)size;
	return false;
}

bool retro_unserialize(const void *data, size_t size)
{
	(void)data;
	(void)size;
	return false;
}

void *retro_get_memory_data(unsigned id)
{
	(void)id;
	return NULL;
}

size_t retro_get_memory_size(unsigned id)
{
	(void)id;
	return 0;
}

void retro_cheat_reset(void)
{}

void retro_cheat_set(unsigned index, bool enabled, const char *code)
{
	(void)index;
	(void)enabled;
	(void)code;
}