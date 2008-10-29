
#define BIT0(val)               (((WORD)(val)) & 0x01)
#define CLR0(val)               ((WORD)(val) & (~0x01))
#define TOGGLE0(val)            ((WORD)(val) ^ 0x01)

#define VISITED(obj)            (BIT0(GCINFO(obj)) == current->visit_flag)
#define MARK_VISITED(info)      ((WORD)(info) | current->visit_flag)

#define ISPLACEHOLDER(obj)      (BIT0(obj) && ((int)(obj) < 0))
#define INDEXOF(obj)            (((-(int)(obj)) >> 1) - current->placeholders)
#define PLACEHOLDER(index)      (-((((index) + current->placeholders) << 1) | 0x01))

#define CURRENT_CYCLE(obj,roots)    !INSIDE(heapchain,obj,(ADDR)roots)

#define SUBST(obj,off,roots,lim)      { ADDR obj1 = (ADDR)obj[off]; \
                                        if (ISPLACEHOLDER(obj1)) { \
                                                int index = INDEXOF(obj1); \
                                                if (index >= 0 && index < (lim)) \
                                                        obj[off] = (WORD)roots->elems[index]; \
                                        } \
                                      }

ADDR substObj(ADDR obj, Array roots, int limit, Thread current) {
        ADDR info = IND0(obj);
        if (!info)                                      // if gcinfo is null we have reached the end of a heap segment
                return (ADDR)obj[1];                    // pointer to first object of next segment is found in subsequent slot
        switch (GC_TYPE(info)) {
                case GC_STD: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {
                                SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_ARRAY: {
                        WORD size = STATIC_SIZE(info) + obj[1], offset = 2;     // find size of dynamic part in second slot of obj, add static size
                        if (info[2])
                                return obj + size;                              // return immediately if array contains only scalars
                        while (offset<size) {
                                SUBST(obj,offset,roots,limit);
                                offset++;
                        }
                        return obj + size;
                }
                case GC_TUPLE: {
                        WORD width = obj[1], offset = 1 + POLYTAGS(width), i = 1, j, tags;
                        while (width > 32) {
                                for (j = 0, tags = obj[i++]; j < 32; j++, offset++, tags = tags >> 1)
                                        if (!(tags & 1))
                                                SUBST(obj,offset,roots,limit);
                                width -= 32;
                        }
                        for (tags = obj[i]; width > 0; width--, offset++, tags = tags >> 1) 
                                if (!(tags & 1))
                                        SUBST(obj,offset,roots,limit);
                        return obj + STATIC_SIZE(info) + width + POLYTAGS(width);
                }
                case GC_BIG: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {                                        // scan all statically known pointer fields
                                SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        offset = info[++i];
                        while (offset) {                                        // scan dynamically identified pointers
                                WORD tagword = info[++i];
                                WORD bitno = info[++i];
                                if ((tagword & (1 << bitno)) == 0)
                                        SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_MUT: {
                        return scan(obj + STATIC_SIZE(info));
                }
        }
        return (ADDR)0;                 // Not reached
}

void subst(Array roots, int limit, ADDR stop, Thread current) {
        ADDR p = (ADDR)roots + STATIC_SIZE(roots->GCINFO) + roots->size;
        int i;
        for (i = 0; i < roots->size; i++)
                if (ISPLACEHOLDER(roots->elems[i])) 
                        RAISE(2);
        while (p != stop)
                p = substObj(p, roots, limit, current);
}

Array CYCLIC_BEGIN(Int n, Int updates) {
        Thread current = CURRENT();
        Array roots = EmptyArray(0,n);
        int i;
        for (i = 0; i < n; i++)
                roots->elems[i] = (POLY)PLACEHOLDER(i);
        if (updates % 2 == 0)
                current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders += n;
        return roots;
}

void CYCLIC_UPDATE(Array roots, Int limit, ADDR stop) {
        Thread current = CURRENT();
        current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders -= roots->size;
        subst(roots, limit, stop, current);
        current->placeholders += roots->size;
}

void CYCLIC_END(Array roots, ADDR stop) {
        Thread current = CURRENT();
        current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders -= roots->size;
        subst(roots, roots->size, stop, current);
}

