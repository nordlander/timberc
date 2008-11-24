#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([timberc], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([Main.hs])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

# Make sure TIBROWSER is set
if test "x$TIBROWSER" = "x"; then
	AC_CHECK_PROGS(TIBROWSER,[more])
fi

# Checks for libraries.

# Look for pthreads.
ACX_PTHREAD

if test "$THREAD_SUPPORT" = "no"; then
	AC_MSG_ERROR([Cannot enable threads on this platform.])
fi

CC="$PTHREAD_CC"
CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
LIBS="$PTHREAD_LIBS $LIBS"



# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h sys/time.h termios.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME

# Look for Happy, otherwise touch Parser.hs to give current time stamp.
AC_CHECK_PROGS(HAPPY, [happy])
if test "x$HAPPY" = "x"; then
	AC_MSG_WARN([Happy not found, you can not change the grammar.])
	touch $srcdir/Parser.hs
fi

# Checks for library functions.
AC_FUNC_MALLOC
AC_CHECK_FUNCS([gettimeofday])

AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(SRC)
AC_SUBST(DEST)
AC_SUBST(HAPPY)
AC_SUBST(prefix)

AC_CONFIG_FILES([Makefile rtsPOSIX/Makefile rtsPOSIX/timberc.cfg])
AC_OUTPUT
