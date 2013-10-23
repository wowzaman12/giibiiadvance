
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

TARGET_NAME = Giibiiadvance

ifeq ($(platform), unix)
   TARGET := $(TARGET_NAME)_libretro.so
   fpic := -fPIC
   SHARED := -shared
else ifeq ($(platform), osx)
   TARGET := $(TARGET_NAME)_libretro.dylib
   fpic := -fPIC
   SHARED := -dynamiclib
else
   TARGET := $(TARGET_NAME)_libretro.dll
   LDFLAGS += -Wl,-no-undefined -Wl,--version-script=link.T -Wl,--unresolved-symbols=ignore-in-shared-libs
   CC = gcc
   SHARED := -shared -static-libgcc -static-libstdc++ -ffast-math -fomit-frame-pointer
endif

GIIBII_DIR := ../..

GIIBII_SRC_DIRS := $(GIIBII_DIR)/gba_core $(GIIBII_DIR)/frontend

GIIBII_CSRCS := $(foreach dir,$(GIIBII_SRC_DIRS),$(wildcard $(dir)/*.c))
GIIBII_COBJ := $(GIIBII_CSRCS:.c=.o)

OBJS := $(GIIBII_COBJ) libretro.o

GIIBII_DEFINES += -D__LIBRETRO__ -DNO_VIZ
#GIIBII_DEFINES += -DFRONTEND_SUPPORTS_RGB565

CFLAGS += -O3 -std=gnu99 -Wall $(fpic) $(GIIBII_DEFINES) 


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


