// The Timber compiler <timber-lang.org>
// 
// Copyright 2008 Johan Nordlander <nordland@csee.ltu.se>
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


#define GC_STD                  0
#define GC_ARRAY                1
#define GC_TUPLE                2
#define GC_BIG                  3
#define GC_MUT                  4

// Required by cyclic.c
#define GC_TYPE(info)           (info[1])
#define STATIC_SIZE(info)       (info[0])
#define POLYTAGS(width)         (((width)+31) % 32)
#define IND0(obj)               (ADDR)((ADDR)obj)[0]

#define ISSTATIC(a)				INSIDE(base0,a,lim0)
#define ISWHITE(a)              INSIDE(base2,a,hp2)
#define ISBLACK(a)              (INSIDE(base,a,scanp) || ISSTATICANDBLACK(a))
#define ISSTATICANDBLACK(a)		(ISSTATIC(a) && (((ADDR)(a))[-1] == scanflag))
#define ISFORWARD(a)            INSIDE(base,a,hp)

#define INSIDE(base,a,lim)      ((base) <= (a) && (a) < (lim))

ADDR base0, lim0;                       // start and end of static heap (finalized by pruneStaticHeap)
ADDR base, lim, hp;                     // start, end, and current pos of "tospace" (normal space)
ADDR base2, lim2, hp2;                  // start, end and current pos of "fromspace" (only used during gc)
ADDR scanp;                   			// current pos of currently scanned segment (only used during gc)

static WORD scanflag;

void scanEnvRoots(int);
void scanTimerQ(int);
extern int envRootsDirty;
extern int timerQdirty;

ADDR gc_new(int words)
{
	ADDR addr;
	int status = ISPROTECTED();
	PROTECT(1);
	
	if (!base0) {						// add space for scanned field before static obj
		hp[0] = !scanflag;
		hp++;
	}
	
	addr = hp;
	hp = hp+(words);
	if (hp>=lim) {
		panic("gc_new_overflow");
	}

	PROTECT(status);
	return addr;
}

void gc_prologue(Ref *obj)
{
	if (ISFORWARD(IND0(*obj))) {
		*obj = (Ref)IND0(*obj);
	}
}

void gc_epilogue(Ref *obj)
{
	if (ISBLACK((ADDR)*obj)) {
		ADDR a = gc_new(1);
		a[0] = (WORD)(*obj); 				/* insert a rescan request by inserting a pointer at the gc_info field */
	}

	/* ISBLACK() and scanning flag is not mutually exclusive (yet). */
	if (IND0((ADDR)*obj) == 0) {
		ADDR info = (ADDR)*obj;
		*info = 1;
	}
	
	/*
	if (ISSTATIC((ADDR)*obj)) {
		((ADDR)*obj)[-1] = !scanflag;		// Revert color back to gray if object is static 
	}
	
	if (((ADDR)*obj) == cleanscan) {
		cleanscan = (ADDR) 0;
	} else if (((ADDR)*obj) == cleancopy) {
		cleancopy = (ADDR) 0;
	}
	*/
}

/* Note!! these sizes are in WORDS, not BYTES */
#define MB(x) x*0x100000
#define KB(x) x*0x400

char staticheap[KB(500)] __attribute__ ((aligned (4)));

char dynheap1[MB(1)] __attribute__ ((aligned (4)));
char dynheap2[MB(1)] __attribute__ ((aligned (4)));

void pruneStaticHeap(void)
{
	base0 	= base;										//Finalize start and end of static heap.
	lim0 	= hp;

	base 	= (ADDR)&dynheap1[0];								//Start using dynheap1 as tospace
	base2 	= (ADDR)&dynheap2[0];								//and leave dynheap2 as spare

	if (!base || !base2)
		panic("Cannot allocate heap spaces");

	lim		= base + sizeof(dynheap1)/sizeof(dynheap1[0]) - 1;	//Set appropriate limits of the spaces
	lim2	= base2 + sizeof(dynheap2)/sizeof(dynheap2[0]) - 1;

	hp		= base;
	hp2		= (ADDR) 0;									// spare heap is inactive (gc is not running)
}

static ADDR copystateful(ADDR obj, ADDR info)
{
	int status = ISPROTECTED();
	WORD j, i = STATIC_SIZE(info);
	ADDR dest, datainfo = IND0(obj+i);              // actual mutable struct follows right after the Ref struct
	WORD size = i + STATIC_SIZE(datainfo);          // dataobj must be a GC_STD or a GC_BIG
	dest = gc_new(size);

	/*
	PROTECT(1);

	cleancopy = obj;
	*/

restart:

	PROTECT(status);

	obj[0] = 0;

	for (j = i; j < size; j++) {
		/*PROTECT(status);*/
		dest[j] = obj[j];
		/*PROTECT(1);
		if (!cleancopy) {
			j = i;
			cleancopy = obj;
		}
		*/
	}

	PROTECT(1);

	if (IND0(obj) != 0) {
		goto restart;
	}

	INITREF((Ref)dest);

	obj[0] = (WORD)dest;
	obj[i] = (WORD)(&dest[i]);						//Forward the mutable state as well (just in case?!)

	if (info[2]) {
		// object has mutable arrays...  !!!!!!!!
	}

	PROTECT(status);
	return dest;
}

ADDR scan(ADDR obj);

ADDR copy(ADDR obj)
{
	if (ISSTATIC(obj)) {							// don't copy if obj is in static heap;
		if (obj[-1] != scanflag) {
			obj[-1] = scanflag;
			scan(obj);								// just scan it.
		}
		return obj;
	}

	if (!ISWHITE(obj))								// don't copy if obj is a tospace address or a low range constant
		return obj;

	ADDR dest, info = IND0(obj);

	if (!info)
		panic("copy: gcinfo is not legal");

	if (ISFORWARD(info))							// gcinfo should point to static data;
		return info;								// if not, we have a forward ptr

	WORD i, size = STATIC_SIZE(info);

	switch (GC_TYPE(info)) {
	case GC_ARRAY:
		size += obj[1];
		break;
	case GC_TUPLE:
		size += obj[1] + POLYTAGS(obj[1]);
		break;
	case GC_MUT:
		return copystateful(obj,info);
	default:
		break;
	}

	dest = gc_new(size);								// allocate in tospace
	for (i=0; i<size; i++)
		dest[i] = obj[i];
	obj[0] = (WORD)dest;							// mark fromspace object as forwarded

	return dest;
}

static ADDR scanstateful(ADDR state, ADDR objinfo)
{
	ADDR obj = state - STATIC_SIZE(objinfo);
	ADDR stateinfo = IND0(state);

	switch (GC_TYPE(stateinfo)) {

		case GC_STD: {
			int status = ISPROTECTED();
			WORD size = STATIC_SIZE(stateinfo), i = 2, offset = stateinfo[i];
			WORD orig = offset;

			/* Mark scan in progress. */
			obj[0] = 0;

			while (offset) {
				ADDR new;
				PROTECT(status);
				new = copy((ADDR)state[offset]);
				PROTECT(1);

				if (IND0(obj) == 0) {
					state[offset] = (WORD)new;
					offset = stateinfo[++i];
				} else {
					offset = orig;
				}
			}

			/* Mark scan as done. */
			obj[0] = (WORD)objinfo;

			/* Mark object as black. */
			if (!ISSTATIC(state)) {
				//move scanp beyond the object to make the object black
				scanp = state + size;
			}

			PROTECT(status);

			return scanp;
		}

		case GC_ARRAY:	{
			panic("scanstateful: state is an array!?!\r\n");
		}

		case GC_TUPLE:	{
			panic("scanstateful: state is a tuple!?!\r\n");
		}

		case GC_BIG:	{
			panic("scanstateful: state is BIG!?!\r\n");
		}

		case GC_MUT:	{
			panic("scanstateful: state is an object!?!\r\n");
		}
	}

	panic("scanstateful: GC_TYPE(info) is not legal\r\n");
	return (ADDR)0;
}

ADDR scan(ADDR obj)
{
	ADDR info = IND0(obj);

	if (!info) {
		panic("scan: gcinfo is not legal");
	}

	if (ISBLACK(info) || ISSTATIC(info)) {						// info field is black pointer means write barrier (rescan request)
		scan(info);
		return obj + 1;
	}
	
	/*
	else if (ISSTATIC(info)) {				// info field is static black pointer means write barrier (rescan request)
		scan(info);
		return obj + 1;
	}
	*/

	switch (GC_TYPE(info)) {
		case GC_STD:	{
			WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
			while (offset) {
				obj[offset] = (WORD)copy((ADDR)obj[offset]);
				offset = info[++i];
			}
			return obj + size;
		}

		case GC_ARRAY:	{
			WORD size = STATIC_SIZE(info) + obj[1], offset = 2;     // find size of dynamic part in second slot of obj, add static size

			if (info[2])
				return obj + size;                              // return immediately if array contains only scalars

			while (offset<size) {
				obj[offset] = (WORD)copy((ADDR)obj[offset]);
				offset++;
			}

			return obj + size;
		}

		case GC_TUPLE:	{
			WORD width = obj[1], offset = 1 + POLYTAGS(width), i = 1, j, tags;
			while (width > 32) {
				for (j = 0, tags = obj[i++]; j < 32; j++, offset++, tags = tags >> 1)
					if (!(tags & 1))
						obj[offset] = (WORD)copy((ADDR)obj[offset]);
				width -= 32;
			}
			for (tags = obj[i]; width > 0; width--, offset++, tags = tags >> 1)
				if (!(tags & 1))
					obj[offset] = (WORD)copy((ADDR)obj[offset]);
			return obj + STATIC_SIZE(info) + width + POLYTAGS(width);
		}

		case GC_BIG:	{
			WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
			while (offset) {
				obj[offset] = (WORD)copy((ADDR)obj[offset]);
				offset = info[++i];
			}
			offset = info[++i];
			while (offset) {                                       // scan dynamically identified pointers
				WORD tagword = info[++i];
				WORD bitno = info[++i];
				if ((tagword & (1 << bitno)) == 0)
					obj[offset] = (WORD)copy((ADDR)obj[offset]);
				offset = info[++i];
			}
			return obj + size;
		}

		case GC_MUT:	{
			return scanstateful(obj + STATIC_SIZE(info), info);

		}
	}
	panic("scan: GC_TYPE(info) is not legal");
	return (ADDR)0;
}

void gc(int force)
{
	WORD tmp;
	int status = ISPROTECTED();
	PROTECT(1);
	if ((!force) && (hp < base + ((lim-base)>>1))) {
		PROTECT(status);
		return;
	}
	ADDR baset,limt;
	baset	= base2;
	limt	= lim2;
	base2	= base;
	lim2	= lim;
	hp2		= hp;
	base	= baset;
	lim		= limt;
	hp		= base;

	scanp	= hp;

	tmp = hp2-base2;

	PROTECT(status);

	/*event_log(EVENT_GC_START, EVENT_DATA(tmp));*/
	scanTimerQ(1); // force a scan
	scanEnvRoots(1); // force a scan

	while (1) {
		WORD *tmp;

		if (scanp == hp) {
			break;                  // break loop when we seem to be done
		}

		tmp = scan(scanp);

		if (tmp < scanp) {
			debug("tmp: 0x");
			debug_hex((unsigned int)tmp);
			debug("\r\nscanp: 0x");
			debug_hex((unsigned int)scanp);
			panic("\r\nThis cannot happen(0)!");
		}

		scanp = tmp;

		if (scanp > hp) {
			panic("This cannot happen(1)!\r\n");
		}

		if (ISSTATIC(scanp)) {
			panic("This cannot happen(2)!\r\n");
		}
	}

	PROTECT(1);

	tmp = hp-base;

	hp2 	= (ADDR) 0;					// Inactivate the spare heap
	scanp	= (ADDR) 0;					// and the scan pointer

	scanflag = !scanflag;

	PROTECT(status);

	/*event_log(EVENT_GC_DONE, EVENT_DATA(tmp));*/
}

void gcinit()
{
	base = (ADDR)&staticheap[0];
	if (!base)
		panic("Cannot allocate initial heap");
	hp = base;
	lim = base + sizeof(staticheap)/sizeof(staticheap[0]) - 1;
	
	base0		= (ADDR) 0;
	lim0		= (ADDR) 0;
	base2		= (ADDR) 0;
	lim2 		= (ADDR) 0;
	hp2			= (ADDR) 0;
	scanp		= (ADDR) 0;
	scanflag 	= 0;
}
