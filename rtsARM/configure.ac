#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([rtsARM], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([rts.c])

# Checks for programs.
AC_PROG_INSTALL

# If no --target option was given then guess one.
if test "x$target_alias" = "x"; then
	GUESS_GCC=`(IFS=:;for d in $PATH; do for f in $d/arm-*-gcc; do test -x $f && echo $f; done; done)|head -n1|xargs basename`
	GUESS_TARGET=`echo $GUESS_GCC|sed -ne 's/.*\(arm-.*\)-gcc.*/\1/p'`
	target_alias=$GUESS_TARGET
fi

# Check to target tools, gcc and ar.
AC_CHECK_TARGET_TOOL([TARGETCC], [gcc])
if test "x$TARGETCC" = "x"; then
	AC_MSG_ERROR([Unable to find target gcc compiler.])
fi

AC_CHECK_TARGET_TOOL([TARGETAR], [ar])
if test "x$TARGETAR" = "x"; then
	AC_MSG_ERROR([Unable to find target ar tool.])
fi

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

# Check the datapath of the compiler.
TIMBERC_DATADIR=`$TIMBERC --print-datadir`

# Checks for header files.
#AC_HEADER_STDC

#AC_CHECK_HEADERS([stdlib.h sys/time.h termios.h])

# Checks for typedefs, structures, and compiler characteristics.
#AC_HEADER_TIME

# Look for the timber compiler

# Checks for library functions.
#AC_FUNC_MALLOC
#AC_CHECK_FUNCS([gettimeofday sysconf])

AC_SUBST(TARGETCC)
AC_SUBST(TARGETAR)
AC_SUBST(SRC)
AC_SUBST(DEST)
AC_SUBST(TIMBERC)
AC_SUBST(prefix)
AC_SUBST(TIMBERC_DATADIR)

AC_CONFIG_FILES([Makefile timberc.cfg])
AC_OUTPUT
