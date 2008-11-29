#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([rtsPOSIX], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([main.c])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Checks for libraries.

# Look for pthreads.
ACX_PTHREAD

if test "$THREAD_SUPPORT" = "no"; then
	AC_MSG_ERROR([Cannot enable threads on this platform.])
fi

CC="$PTHREAD_CC"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
LIBS="$PTHREAD_LIBS $LIBS"

AC_ARG_WITH(timberc,
[AC_HELP_STRING([--with-timberc[=DIR]], [Path to timberc])],
[with_timberc=$withval]
)

if test "x$with_timberc" = "x"; then
	AC_CHECK_PROGS(TIMBERC, [timberc])
else
	TIMBERC="$with_timberc"
fi

if test "x$TIMBERC" = "x"; then
	AC_MSG_ERROR([Timberc not found.])
fi

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h sys/time.h termios.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME

# Look for the timber compiler

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday])

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(SRC)
AC_SUBST(DEST)
AC_SUBST(TIMBERC)
AC_SUBST(prefix)

AC_CONFIG_FILES([Makefile timberc.cfg])
AC_OUTPUT
