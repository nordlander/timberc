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

# Make sure that we have realtime-threads extension so we can build with
# the mutex protocol etc. This requires _GNU_SOURCE=1 on most Linux
# ditrubutions (see features.h for more info).
AC_MSG_CHECKING([pthread realtime-threads extension])
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <pthread.h>]],
		[[pthread_mutexattr_setprotocol(NULL, PTHREAD_PRIO_INHERIT);]]
	)],
	[need_gnu_source=no],
	[need_gnu_source=yes]
	)
if test "${need_gnu_source}"x = "yes"x
then
	CFLAGS="${CFLAGS} -D_GNU_SOURCE=1"
fi

# Just make sure that the program actually compiles now.
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <pthread.h>]],
		[[pthread_mutexattr_setprotocol(NULL, PTHREAD_PRIO_INHERIT);]]
	)],
	[AC_MSG_RESULT([yes])],
	[AC_MSG_FAILURE([no])]
	)

# Moving swiftly on to the S_IWUSR etc. flags that are also required 99%
# of the times. _GNU_SOURCE will enable this but let's not take any chances.
AC_MSG_CHECKING([sys/stat.h header sanity])
AC_COMPILE_IFELSE(
	[AC_LANG_PROGRAM([[#include <sys/stat.h>]],
		[[open("", S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH);]]
	)],
	[AC_MSG_RESULT([yes])],
	[AC_MSG_FAILURE([no])]
	)

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
