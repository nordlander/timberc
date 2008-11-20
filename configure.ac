#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([timberc], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([Main.hs])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# XXX This is ugly
# For now - hard code the options and it will work.
case $(uname) in
Darwin)
        LDFLAGS="$LDFLAGS -Wl,-stack_size,0x3000000"
        ;;
*)
        ;;
esac

# Make sure TIBROWSER is set
if test "x$TIBROWSER" = "x"; then
	AC_CHECK_PROGS(TIBROWSER,[more])
fi

# Checks for libraries.

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h sys/time.h termios.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday])

CFLAGS="$CFLAGS -c -D_GNU_SOURCE"
LDFLAGS="$LDFLAGS -lpthread"
CC="gcc"

AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(SRC)
AC_SUBST(prefix)
AC_SUBST(HFLAGS)
AC_SUBST(DEST)

AC_CONFIG_FILES([Makefile CompileConfig.hs rtsPOSIX/timberc.cfg])
AC_OUTPUT
