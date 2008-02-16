
#define BIT0(val)               (((WORD)(val)) & 0x01)
#define CLR0(val)               ((WORD)(val) & (~0x01))
#define TOGGLE0(val)            ((WORD)(val) ^ 0x01)

#define VISITED(obj)            (BIT0(GCINFO(obj)) == current->visit_flag)
#define MARK_VISITED(info)      ((WORD)(info) | current->visit_flag)

#define ISPLACEHOLDER(obj)      (BIT0(obj) && ((int)(obj) < 0))
#define INDEXOF(obj)            (((-(int)(obj)) >> 1) - current->placeholders)
#define PLACEHOLDER(index)      (-((((index) + current->placeholders) << 1) | 0x01))

#define CURRENT_CYCLE(obj,roots)    ((ADDR)(roots) <= (obj))


void visit(ADDR obj, Array roots) {
        ADDR info = (ADDR)CLR0(GCINFO(obj));
        GCINFO(obj) = MARK_VISITED(info);
        WORD size = info[0];
        if (size) {
                WORD i = 1, offset = info[i];
                while (offset) {
                        ADDR obj1 = (ADDR)obj[offset];
                        if (ISPLACEHOLDER(obj1)) {
                                int index = INDEXOF(obj1);
                                if (index >= 0)
                                        obj[offset] = (WORD)roots->elems[index];
                        }
                        else if (CURRENT_CYCLE(obj1,roots) && !VISITED(obj1))
                                visit(obj1, roots);
                        offset = info[++i];
                }
        } else if (info[1]) {
                size = obj[1] + 2;
                WORD i = 2;
                while (i < size) {
                        ADDR obj1 = (ADDR)obj[i];
                        if (ISPLACEHOLDER(obj1)) {
                                int index = INDEXOF(obj1);
                                if (index >= 0)
                                        obj[i] = (WORD)roots->elems[index];
                        }
                        else if (CURRENT_CYCLE(obj1,roots) && !VISITED(obj1))
                                visit(obj1, roots);
                        i++;
                }
        }
}

Array CYCLIC_BEGIN(int n) {
    Array roots = EmptyArray(n);
    int i;
    for (i = 0; i < n; i++)
        roots->elems[i] = (POLY)PLACEHOLDER(i);
    current->visit_flag = TOGGLE0(current->visit_flag);
    current->placeholders += n;
    return roots;
}

void CYCLIC_END(Array roots) {
    int i;
    current->visit_flag = TOGGLE0(current->visit_flag);
    current->placeholders -= roots->size;
    for (i = 0; i < roots->size; i++)
        visit((ADDR)roots->elems[i], roots);
}
