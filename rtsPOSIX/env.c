
struct DescFile {
        struct File_3_POSIX super;
        int desc;
};
typedef struct DescFile *DescFile;

LIST read_fun( File_3_POSIX this, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        LIST xs = (LIST)_NIL;
        while (1) {
                DISABLE(&previous_mask);
                int r = read(((DescFile)this)->desc, buf, 1023);
                ENABLE(&previous_mask);
                if (r <= 0)
                        return xs;
                while (r) {
                        CONS n; NEW(CONS, n, sizeof(struct CONS));
                        n->a = (POLY)(Int)buf[--r];
                        n->b = xs;
                        xs = (LIST)n;
                }
        }
}

LIST write_fun( File_3_POSIX this, LIST xs, POLY self ) {
        sigset_t previous_mask;
        char buf[1024];
        while (xs) {
                LIST xs0 = xs;
                int len = 0;
                while (xs && len < 1024) {
                        buf[len++] = (Char)(Int)((CONS)xs)->a;
                        xs = ((CONS)xs)->b;
                }
                DISABLE(&previous_mask);
                int r = write(((DescFile)this)->desc, buf, len);
                ENABLE(&previous_mask);
                if (r < 0) r = 0;
                if (r < len) {
                        while (r--)
                                xs0 = ((CONS)xs0)->b;
                        return xs0;
                }
        }
        return (LIST)_NIL;
}

UNITTYPE exit_fun( Env_2_POSIX this, Int n, POLY self ) {
        DISABLE(NULL);
        exit(n);
}

struct DescFile stdin_struct  = { { read_fun, write_fun }, 0 };

struct DescFile stdout_struct = { { read_fun, write_fun }, 1 };

struct Env_2_POSIX env_struct = { NULL, (File_3_POSIX)&stdin_struct, (File_3_POSIX)&stdout_struct, exit_fun };
Env_2_POSIX env = &env_struct;

void init_env() {
        fcntl(0, F_SETFL, O_NONBLOCK + O_ASYNC);
        fcntl(1, F_SETFL, O_NONBLOCK + O_ASYNC);
}


