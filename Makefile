TARGET = giibiiadvance.exe
o = gba_core/arm.o gba_core/bios.o gba_core/cpu.o gba_core/disassembler.o gba_core/dma.o gba_core/gba.o gba_core/interrupts.o gba_core/memory.o gba_core/rom.o gba_core/save.o gba_core/sound.o gba_core/thumb.o gba_core/video.o

CC = gcc
CXX = g++
RESOURCE = windres
STRIP = strip

LIBS = -lwininet -lgdi32 -lwinmm -lws2_32 -lcomctl32 -lcomdlg32 -lstdc++ -lz -lpng16
DEFINES = -I. -DWIN32
LDFLAGS = -L. -static-libgcc -s -mwindows


ifneq ($(V),1)
   Q := @
endif


CFLAGS = -std=c99 -Wall -O3 -s -I. -fexpensive-optimizations

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