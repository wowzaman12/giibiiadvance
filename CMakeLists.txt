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

option( ENABLE_WINDOWS_FRONTEND "Build the Windows port" ON )
# Set the version number with -DVERSION=X.X.X-uber

# Version number
IF( NOT VERSION )
	SET( VERSION "2.0.0-git" )
ENDIF( NOT VERSION )

# Look for some dependencies using builtin CMake scripts
FIND_PACKAGE ( ZLIB REQUIRED )
FIND_PACKAGE ( PNG REQUIRED )

SET( PKGDATADIR ${CMAKE_INSTALL_PREFIX}/${DATA_INSTALL_DIR} )

# C defines
ADD_DEFINITIONS (-DHAVE_ZLIB_H -DNO_VIS -DWIN32)
ADD_DEFINITIONS (-DVERSION="${VERSION}" -DPKGDATADIR="${PKGDATADIR}")

# Source files definition

SET(png_RESOURCES
	png/save_png.c
)

SET(SRC_GBACORE
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
IF( ENABLE_WINDOWS_FRONTEND)
	SET(SRC_FRONTEND
		frontend/config.c
		frontend/main.c
		frontend/windows/gui_config.c
		frontend/windows/gui_gba_debugger.c
		frontend/windows/gui_gba_debugger_vram.c
		frontend/windows/gui_main.c
		frontend/windows/gui_mainloop.c
		frontend/windows/gui_text_windows.c
		frontend/windows/splash.c
		frontend/windows/text_data.c
	)
	SET(giibii_RESOURCES
		frontend/windows/resource.rc
	)
ENDIF( ENABLE_WINDOWS_FRONTEND)


INCLUDE_DIRECTORIES(
	${ZLIB_INCLUDE_DIRS}
	${PNG_INCLUDE_DIR}
)
if(MINGW AND CMAKE_CROSSCOMPILING)
	set_target_properties(giibiiadvance PROPERTIES LINK_FLAGS "-static-libgcc -static-libstdc++")
endif()

IF( ENABLE_WINDOWS_FRONTEND )
	set(CMAKE_RC_COMPILE_OBJECT
		"<CMAKE_RC_COMPILER> <FLAGS> -O coff <DEFINES> -i <SOURCE> -o <OBJECT>"
	)
	#set_target_properties(giibiiadvance PROPERTIES LINK_FLAGS "-static-libgcc -static-libstdc++")
	ADD_EXECUTABLE (
		giibiiadvance
		WIN32
		${png_RESOURCES}
		${SRC_GBACORE}
		${SRC_FRONTEND}
		${giibii_RESOURCES}
	)

	TARGET_LINK_LIBRARIES (
		giibiiadvance
		${ZLIB_LIBRARIES}
		${PNG_LIBRARY}
	)
ENDIF( ENABLE_WINDOWS_FRONTEND )
# Installation
INSTALL(PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/giibiiadvance DESTINATION bin)
