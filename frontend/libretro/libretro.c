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

unsigned retro_api_version(void) { return RETRO_API_VERSION; }

void retro_get_system_info(struct retro_system_info *info)
{
   info->library_name = "Giibii Advance";
   info->library_version = "v1.0.0";
   info->valid_extensions = "gba|bin|agp";
   info->block_extract = false;
   info->need_fullpath = true;
}
