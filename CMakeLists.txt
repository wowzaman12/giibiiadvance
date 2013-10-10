cmake_minimum_required(VERSION 2.8)
cmake_policy(VERSION 2.8)

# The project's name is giibii advance it uses Cs code
PROJECT(giibiiadvance C)

# CMake include path
#list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake )

# GCC doesn't enable C99 by default
IF(CMAKE_COMPILER_IS_GNUCC)
    SET(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 -fexpensive-optimizations -O3 -Wall -s")
ENDIF()

# Set the version number with -DVERSION=X.X.X-uber

# Version number
IF( NOT VERSION )
	SET( VERSION "2.0.0-git" )
ENDIF( NOT VERSION )

# Look for some dependencies using builtin CMake scripts
FIND_PACKAGE ( ZLIB REQUIRED )
FIND_PACKAGE ( PNG REQUIRED )

# Set the default install dir
IF( NOT DATA_INSTALL_DIR )
	SET( DATA_INSTALL_DIR "share/vba" )
ENDIF( NOT DATA_INSTALL_DIR )

SET( PKGDATADIR ${CMAKE_INSTALL_PREFIX}/${DATA_INSTALL_DIR} )

# C defines
ADD_DEFINITIONS (-DHAVE_NETINET_IN_H -DHAVE_ARPA_INET_H -DHAVE_ZLIB_H -DNO_VIS)
ADD_DEFINITIONS (-DVERSION="${VERSION}" -DPKGDATADIR="${PKGDATADIR}")
#ADD_DEFINITIONS (-DLINK_EMULATION)

# Source files definition
#SET(SRC_GBCORE
#	gb_core/cpu.c
#	gb_core/debug.c
#	gb_core/gb_main.c
#	gb_core/general.c
#	gb_core/interrupts.c
#	gb_core/licensees.c
#	gb_core/mbc.c
#	gb_core/memory.c
#	gb_core/noise.c
#	gb_core/rom.c
#	gb_core/serial.c
#	gb_core/sgb.c
#	gb_core/sound.c
#	gb_core/video.c
#)

SET(SRC_WIN32FRONT
	frontend/windows/config.c
	frontend/windows/gui_config.c
	frontend/windows/gui_gb_debugger.c
	frontend/windows/gui_gb_debugger_vram.c
	frontend/windows/gui_gba_debugger.c
	frontend/windows/gui_gba_debugger_vram.c
	frontend/windows/gui_main.c
	frontend/windows/gui_mainloop.c
	frontend/windows/gui_sgb_debugger.c
	frontend/windows/gui_text_windows.c
	frontend/windows/main.c
	frontend/windows/splash.c
	frontend/windows/text_data.c
	gba_core/arm.c
	gba_core/bios.c
	gba_core/cpu.c
	gba_core/disassembler.c
	gba_core/dma.c
	gba_core/gba.c
	gba_core/interrupts.c
	gba_core/memory.c
	gba_core/rom.c
	gba_core/save.c
	gba_core/sound.c
	gba_core/thumb.c
	gba_core/timers.c
	gba_core/video.c
)

SET(giibii_RESOURCES
	frontend/windows/resource.rc
)

SET(png_RESOURCES
	png/save_png.c
)

if(MINGW)
	set(CMAKE_RC_COMPILE_OBJECT
		"<CMAKE_RC_COMPILER> <FLAGS> -O coff <DEFINES> -i <SOURCE> -o <OBJECT>")
endif(MINGW)

if(MINGW AND CMAKE_CROSSCOMPILING)
	set_target_properties(giibiiadvance PROPERTIES LINK_FLAGS "-static-libgcc -static-libstdc++")
endif()

INCLUDE_DIRECTORIES(
	${ZLIB_INCLUDE_DIRS}
	${PNG_INCLUDE_DIR}
)

ADD_EXECUTABLE (
	giibiiadvance
	WIN32
	${png_RESOURCES}
	${SRC_WIN32FRONT}
	#${SRC_GBCORE}
	#${SRC_GBACORE}
	${giibii_RESOURCES}
)

TARGET_LINK_LIBRARIES (
	giibiiadvance
	${ZLIB_LIBRARIES}
	${PNG_LIBRARY}
)

# Installation
INSTALL(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/giibiiadvance DESTINATION bin)
