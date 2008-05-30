
#define BIT0(val)               (((WORD)(val)) & 0x01)
#define CLR0(val)               ((WORD)(val) & (~0x01))
#define TOGGLE0(val)            ((WORD)(val) ^ 0x01)

#define VISITED(obj)            (BIT0(GCINFO(obj)) == current->visit_flag)
#define MARK_VISITED(info)      ((WORD)(info) | current->visit_flag)

#define ISPLACEHOLDER(obj)      (BIT0(obj) && ((int)(obj) < 0))
#define INDEXOF(obj)            (((-(int)(obj)) >> 1) - current->placeholders)
#define PLACEHOLDER(index)      (-((((index) + current->placeholders) << 1) | 0x01))

#define CURRENT_CYCLE(obj,roots)    ((ADDR)(roots) <= (obj))


void visit(ADDR obj, Array roots, Int limit) {
        ADDR info = (ADDR)CLR0(GCINFO(obj));
        GCINFO(obj) = MARK_VISITED(info);
        WORD size = info[0];
        if (size) {
                WORD i = 1, offset = info[i];
                while (offset) {
                        ADDR obj1 = (ADDR)obj[offset];
                        if (ISPLACEHOLDER(obj1)) {
                                int index = INDEXOF(obj1);
                                if (index >= 0 && index < limit)
                                        obj[offset] = (WORD)roots->elems[index];
                        }
                        else if (CURRENT_CYCLE(obj1,roots) && !VISITED(obj1))
                                visit(obj1, roots, limit);
                        offset = info[++i];
                }
        } else if (info[1]) {
                size = obj[1] + 2;
                WORD i = 2;
                while (i < size) {
                        ADDR obj1 = (ADDR)obj[i];
                        if (ISPLACEHOLDER(obj1)) {
                                int index = INDEXOF(obj1);
                                if (index >= 0 && index < limit)
                                        obj[i] = (WORD)roots->elems[index];
                        }
                        else if (CURRENT_CYCLE(obj1,roots) && !VISITED(obj1))
                                visit(obj1, roots, limit);
                        i++;
                }
        }
}

void visitRoots(Array roots, int limit) {
        int i;
        for (i = 0; i < limit; i++) {
                ADDR obj = (ADDR)roots->elems[i];
                if (ISPLACEHOLDER(obj)) {
                        int index = INDEXOF(obj);
                        if (index >= 0 && index < i)
                                roots->elems[i] = roots->elems[index];
                }
                visit(obj, roots, limit);
        }        
}

Array CYCLIC_BEGIN(Int n, Int updates) {
        Array roots = EmptyArray(n);
        int i;
        for (i = 0; i < n; i++)
                roots->elems[i] = (POLY)PLACEHOLDER(i);
        if (updates % 2 == 0)
                current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders += n;
        return roots;
}

void CYCLIC_UPDATE(Array roots, Int limit) {
        current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders -= roots->size;
        visitRoots(roots, limit);
        current->placeholders += roots->size;
}

void CYCLIC_END(Array roots) {
        current->visit_flag = TOGGLE0(current->visit_flag);
        current->placeholders -= roots->size;
        visitRoots(roots, roots->size);
}
