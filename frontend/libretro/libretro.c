#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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


void retro_get_system_info(struct retro_system_info *info)
{
   info->library_name = "Giibii Advance";
   info->library_version = "v1.0.0";
   info->valid_extensions = "gba|bin|agp";
   info->block_extract = false;
   info->need_fullpath = true;
}
