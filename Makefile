TARGET = giibiiadvance.exe
o = frontend/config.o gba_core/arm.o gba_core/bios.o gba_core/cpu.o gba_core/disassembler.o gba_core/dma.o gba_core/gba.o gba_core/interrupts.o gba_core/memory.o gba_core/rom.o gba_core/save.o gba_core/sound.o gba_core/thumb.o gba_core/timers.o gba_core/video.o frontend/windows/gui_config.o frontend/windows/gui_gba_debugger.o frontend/windows/gui_gba_debugger_vram.o frontend/windows/gui_main.o frontend/windows/gui_mainloop.o frontend/windows/gui_text_windows.o frontend/main.o ./png/save_png.o

CC = gcc
CXX = g++
RESOURCE = windres
STRIP = strip

LIBS = -lwininet -lgdi32 -lwinmm -lws2_32 -lcomctl32 -lcomdlg32 -lstdc++ -lz -lpng16 
DEFINES = -I. -DWIN32
LDFLAGS = -L. -static-libgcc -s -mwindows -ffast-math -fomit-frame-pointer


ifneq ($(V),1)
   Q := @
endif


CFLAGS = -std=c99 -Wall -O3 -s -I. -fexpensive-optimizations -ffast-math -fomit-frame-pointer -I./gba_core -I./png -I./frontend/ -I./frontend/windows

all: $(TARGET)

$(TARGET): $(o)
	$(Q)$(CC) -o $@ $(o) $(LIBS) $(LDFLAGS)
	@$(if $(Q), $(shell echo echo LD $@),) 
	$(STRIP) $(TARGET)
	
%.o: %.rc
	$(RESOURCE) $< $@

%.o: %.cpp
	$(Q)$(CC) $(CFLAGS) $(DEFINES) -c -o $@ $<
	@$(if $(Q), $(shell echo echo CC $<),)

clean:
	rm -f ./png/*.o
	rm -f ./frontend/*.o
	rm -f ./frontend/windows/*.o
	rm -f ./gba_core.o
	rm -f $(TARGET)

install:

.PHONY: all install uninstall clean