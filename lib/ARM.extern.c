// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifdef rtsARM

#undef TRUE
#undef FALSE
#include "lpc2468_registers.h"
#include "ea_lcd/lcd_hw.h"
#include "my9x10font.h"
#include "ARM.h"
#include "rts.h"

static int debug_fun(Env_ARM env, CONS x) 
{
	while (x) {
        debug_char((int)x->hd);
		x = (CONS)x->tl;
	}
	return 0;
}

/* EA QVGA portrait mode TFT display parameters */
static tLcdParams ea_QVGA_v2 =
{
    28,         /* Horizontal back porch */
    10,         /* Horizontal front porch */
    2,          /* HSYNC pulse width */
    240,        /* Pixels per line */
    3,          /* Vertical back porch */
    2,          /* Vertical front porch */
    2,          /* VSYNC pulse width */
    320,        /* Lines per panel */
    0,          /* Do not invert output enable */
    1,          /* Invert panel clock */
    1,          /* Invert HSYNC */
    1,          /* Invert VSYNC */
    1,          /* AC bias frequency (not used) */
    16,         /* Bits per pixel */
    LCD_ADTFT,  /* LCD panel type */
    0,          /* Single panel display */
};


/* fb needs to be double aligned */
volatile char screen[320][240] __attribute__ ((aligned (8)));

#define CHAR_WIDTH width
#define CHAR_HEIGHT (height/93)
#define SCREEN_YSIZE 320
#define SCREEN_XSIZE 240

#define WHITE 22
#define BLACK 12
#define BACKGROUND WHITE

static int drawbox(int env, int data, int x, int y, int xs, int ys, POLY something)
{
    int ly,lx;
	for(ly=y;((ly<(y+ys)) && (ly<SCREEN_YSIZE));ly++)
		for(lx=x;((lx<(x+xs)) && (lx<SCREEN_XSIZE));lx++)
			screen[ly][lx] = data; /* should be optimized with memset */
	return 0;
}

static int drawchar(int env, int chr, int x, int y, POLY something)
{
    int ly,lx;
	if ((chr>=33)&&(chr<=126)) {
		int chr_index = chr-33;
	
		for(ly=y;((ly<(y+CHAR_HEIGHT)) && (ly<SCREEN_YSIZE));ly++)
			for(lx=x;((lx<(x+CHAR_WIDTH)) && (lx<SCREEN_XSIZE));lx++){
				screen[ly][lx] = header_data[ (lx-x) + (ly-y)*CHAR_WIDTH + chr_index*CHAR_WIDTH*CHAR_HEIGHT];
			}
		return 1;
	} else if (chr==' ') {
		for(ly=y;((ly<(y+CHAR_HEIGHT)) && (ly<SCREEN_YSIZE));ly++)
			for(lx=x;((lx<(x+CHAR_WIDTH)) && (lx<SCREEN_XSIZE));lx++)
				screen[ly][lx] = BACKGROUND; 
		return 1;
	}
	return 0;
}

#define PAL(a) ( ((header_data_cmap[a][0]>>3) <<0) | ((header_data_cmap[a][1]>>3)<<5) | ((header_data_cmap[a][2]>>3)<<10)  ) 

void initfb(void){
	LCD_UPBASE = (int)screen;
	LCD_CTRL = 0x827; /* 8bit palette, TFT, little-endian */
	volatile unsigned int *palette = (volatile void*)&LCD_PAL;
	unsigned int i;
	
	for(i = 0; i<127;i++)
		*palette++ = PAL((i<<1)) | (PAL(((i<<1)+1))<<16);
}

// --------------------------------------------------------------------------------------------

extern WORD __ram_start, __ram_end;

static void portwrite(Env_ARM env, unsigned int addr, int data, int dummy)
{	
    if (addr >= (unsigned int)&__ram_end || addr < (unsigned int)&__ram_start)
	    *(int*)addr = data;
}

static void portset(Env_ARM env, unsigned int addr, unsigned int data, int dummy)
{	
	int status = ISPROTECTED();
	PROTECT(1);
    if (addr >= (unsigned int)&__ram_end || addr < (unsigned int)&__ram_start)
	    *(int*)addr = *(int*)addr | data;
	PROTECT(status);
}

static void portclear(Env_ARM env, unsigned int addr, unsigned int data, int dummy)
{	
	int status = ISPROTECTED();
	PROTECT(1);
    if (addr >= (unsigned int)&__ram_end || addr < (unsigned int)&__ram_start)
	    *(int*)addr = *(int*)addr & ~data;
	PROTECT(status);
}

static int portread(Env_ARM env, unsigned int addr, int dummy)
{
    if (addr >= (unsigned int)&__ram_end || addr < (unsigned int)&__ram_start)
	    return *(int*)addr;
    return 0;
}

static int valid_net_addr(unsigned int addr)
{
	if ((addr >= 0x7fe00000) && (addr < 0x7fe04000)) {
		return !0;
	} else {
		return !!0;
	}
}

static Array netmemread(Env_ARM env, unsigned int addr, unsigned int size, int dummy)
{
	int i;
	void **ptr = (void **)addr;
	Array array;

	NEW(Array, array, WORDS(sizeof(struct Array)+size));
	array->GCINFO = __GC__Array0;
	array->size   = WORDS(size);
	char *from = (char *)addr;
	char *to = (char *)array->elems;

	if (valid_net_addr(addr) && valid_net_addr(addr + size)) {
		array->elems[array->size-1] = (void*)0;
		for (i=0;i<size;i++) {
			*to++ = *from++;
		}
	} else {
		for (i=0;i<size;i++) {
			*to++ = 0;
		}
	}

	return array;
}

static void netmemwrite(Env_ARM env, Array array, unsigned int addr, unsigned int size, int dummy)
{
	int i;
	char *to = (char *)addr;
	char *from = (char *)array->elems;

	if (valid_net_addr(addr) && valid_net_addr(addr + size) && (WORDS(size) <= array->size)) {
		for (i=0;i<size;i++) {
			*to++ = *from++;
		}
	}
}


// Interrupt handling ------------------------------------------------------------------------

CLOS handler_table[32];

static inline void handler(int slot)
{
    IRQ_PROLOGUE();
	((void(*)(CLOS,int))(handler_table[slot]->Code))(handler_table[slot],0);
    IRQ_EPILOGUE();
}

void handler0(void)  { handler(0);  } 
void handler1(void)  { handler(1);  }
void handler2(void)  { handler(2);  }
void handler3(void)  { handler(3);  }
void handler4(void)  { handler(4);  }
void handler5(void)  { handler(5);  }
void handler6(void)  { handler(6);  }
void handler7(void)  { handler(7);  }
void handler8(void)  { handler(8);  }
void handler9(void)  { handler(9);  }
void handler10(void) { handler(10); }
void handler11(void) { handler(11); }
void handler12(void) { handler(12); }
void handler13(void) { handler(13); }
void handler14(void) { handler(14); }
void handler15(void) { handler(15); }
void handler16(void) { handler(16); }
void handler17(void) { handler(17); }
void handler18(void) { handler(18); }
void handler19(void) { handler(19); }
void handler20(void) { handler(20); }
void handler21(void) { handler(21); }
void handler22(void) { handler(22); }
void handler23(void) { handler(23); }
void handler24(void) { handler(24); }
void handler25(void) { handler(25); }
void handler26(void) { handler(26); }
void handler27(void) { handler(27); }
void handler28(void) { handler(28); }
void handler29(void) { handler(29); }
void handler30(void) { handler(30); }
void handler31(void) { handler(31); }

int install(Env_ARM env, int bits, int slot, CLOS h, int dummy)
{
	if (slot < 0 || slot >= 32 || slot == 4)
        return 0;

	int status = ISPROTECTED();
	PROTECT(1);

	handler_table[slot] = h;
	VICIntEnable |= (1<<slot);
    rootsDirty = 1;
    
	PROTECT(status);
	return 0;
}

struct Env_ARM envS = { .GCINFO          = __GC__Env_ARM,
                        .debug_ARM       = (void*)debug_fun,
                        .portwrite_ARM   = (void*)portwrite,
                        .portset_ARM     = (void*)portset,
                        .portclear_ARM   = (void*)portclear,
                        .portread_ARM    = (void*)portread,
                        .install_ARM     = (void*)install,
                        .netmemread_ARM  = (void*)netmemread,
                        .netmemwrite_ARM = (void*)netmemwrite};
                       
struct TFT_ARM tftS = { .GCINFO          = __GC__TFT_ARM, 
                        .drawchar_ARM    = (void*)drawchar, 
                        .drawbox_ARM     = (void*)drawbox, 
                        .xsize_ARM       = SCREEN_XSIZE,
                        .ysize_ARM       = SCREEN_YSIZE,
                        .charwidth_ARM   = 0,
                        .charheight_ARM  = 0 }; 

Env_ARM env0    = &envS;
TFT_ARM tft0    = &tftS;


void scanEnvRoots (void) {
    int i;
	for(i=0;i<32;i++) {
        PROTECT(1);
		handler_table[i] = (CLOS)copy((ADDR)handler_table[i]);
        PROTECT(0);
	}
}

struct Scanner envRootScanner = {scanEnvRoots, NULL};



Env_ARM arm_ARM(World w, Int dummy) {
    return env0;
}

TFT_ARM tft_ARM(World w, Int dummy) {
    return tft0;
}

void _init_external_ARM(void) {
//  PINSEL0 = 0x00000050;   // USART0
//  PINMODE0 = 0;           // Enable pullups on all pins
//  PINSEL3 = 0x05555505;   // Ethernet and LCD
//  PINSEL4 = 0x000fffff;   // Ethernet, JTAG, LCD
//  PINSEL9 = 0x5a555555;   // SDRAM_sdwen, LCD , BSL0, BSL1
//  PINSEL9 = 0x0a040000;   // SDRAM_sdwen, LCD
//  PINSEL5 = 0x05010115;   // SDRAM CAS,RAS, DYCS0, DQM0 ... uboot:05050555 

    //initialize LCD  
    PCONP |= 0x00100000;    /* Power Control for CLCDC */ 

    lcdInit(&ea_QVGA_v2); 
    lcdTurnOn();

    initfb();
    tft0->charwidth_ARM = CHAR_WIDTH;
    tft0->charheight_ARM = CHAR_HEIGHT;
    
    VICVectAddr0  = (unsigned long)handler0;
    VICVectAddr1  = (unsigned long)handler1;
    VICVectAddr2  = (unsigned long)handler2;
    VICVectAddr3  = (unsigned long)handler3;
    // Note: slot 4 reserved by the kernel, used by timer0
    VICVectAddr5  = (unsigned long)handler5;
    VICVectAddr6  = (unsigned long)handler6;
    VICVectAddr7  = (unsigned long)handler7;
    VICVectAddr8  = (unsigned long)handler8;
    VICVectAddr9  = (unsigned long)handler9;
    VICVectAddr10 = (unsigned long)handler10;
    VICVectAddr11 = (unsigned long)handler11;
    VICVectAddr12 = (unsigned long)handler12;
    VICVectAddr13 = (unsigned long)handler13;
    VICVectAddr14 = (unsigned long)handler14;
    VICVectAddr15 = (unsigned long)handler15;
    VICVectAddr16 = (unsigned long)handler16;
    VICVectAddr17 = (unsigned long)handler17;
    VICVectAddr18 = (unsigned long)handler18;
    VICVectAddr19 = (unsigned long)handler19;
    VICVectAddr20 = (unsigned long)handler20;
    VICVectAddr21 = (unsigned long)handler21;
    VICVectAddr22 = (unsigned long)handler22;
    VICVectAddr23 = (unsigned long)handler23;
    VICVectAddr24 = (unsigned long)handler24;
    VICVectAddr25 = (unsigned long)handler25;
    VICVectAddr26 = (unsigned long)handler26;
    VICVectAddr27 = (unsigned long)handler27;
    VICVectAddr28 = (unsigned long)handler28;
    VICVectAddr29 = (unsigned long)handler29;
    VICVectAddr30 = (unsigned long)handler30;
    VICVectAddr31 = (unsigned long)handler31;

    addRootScanner(&envRootScanner);    
}

#else


Env_ARM arm_ARM(World w, Int dummy) {
    RAISE(3);
    return NULL;
}

TFT_ARM tft_ARM(World w, Int dummy) {
    RAISE(3);
    return NULL;
}

void _init_external_ARM(void) {
    // Nothing
}

#endif

volatile int xxxx = 0;

void busy_ARM(Int n) {
    int i;
    for (i = 0; i < n; i++)
        xxxx = xxxx + 1;
}

