ifeq ($(platform),)
platform = unix
ifeq ($(shell uname -a),)
   platform = win
else ifneq ($(findstring MINGW,$(shell uname -a)),)
   platform = win
else ifneq ($(findstring win,$(shell uname -a)),)
   platform = win
else ifneq ($(findstring Darwin,$(shell uname -a)),)
   platform = osx
endif
endif

TARGET_NAME = giibiiadvance

ifeq ($(platform), unix)
   TARGET := $(TARGET_NAME)_libretro.so
   fpic := -fPIC
   SHARED := -shared
else ifeq ($(platform), osx)
   TARGET := $(TARGET_NAME)_libretro.dylib
   fpic := -fPIC
   SHARED := -dynamiclib
else
   TARGET := $(TARGET_NAME)
   LDFLAGS += -Wl,-no-undefined
   CC = i686-w64-mingw32-gcc
   SHARED := -shared -static-libgcc -static-libstdc++ -ffast-math -fomit-frame-pointer
endif

GIIBII_DIR := ./

GIIBII_SRC_DIRS := $(GIIBII_DIR)/gba_core  $(GIIBII_DIR)/frontend $(GIIBII_DIR)/frontend/windows

GIIBII_CSRCS := $(foreach dir,$(GIIBII_SRC_DIRS),$(wildcard $(dir)/*.c))
GIIBII_COBJ := $(GIIBII_CSRCS:.c=.o)

OBJS := $(GIIBII_COBJ) 

GIIBII_DEFINES += -DNO_VIZ
GIIBII_DEFINES += -DFRONTEND_SUPPORTS_RGB565

CFLAGS += -O3 -std=gnu99 $(fpic) $(GIIBII_DEFINES)


INCDIRS := -I$(GIIBII_DIR)
LIBS :=

$(TARGET): $(OBJS)
	$(CC) -o $@ $(SHARED) $(OBJS) $(LDFLAGS) $(LIBS)

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS) $(INCDIRS)

clean:
	rm -f $(OBJS)
	rm -f $(TARGET)

.PHONY: clean


