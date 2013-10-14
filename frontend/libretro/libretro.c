#include "../../build_options.h"

#include "../config.h"
#include "../../gba_core/gba.h"
#include "../../gba_core/cpu.h"
#include "../../gba_core/interrupts.h"
#include "../../gba_core/video.h"
#include "../../gba_core/memory.h"
#include "../../gba_core/sound.h"

static retro_video_refresh_t video_cb;
static retro_input_poll_t poll_cb;
static retro_input_state_t input_cb;
retro_audio_sample_batch_t audio_batch_cb;
static retro_environment_t environ_cb;
