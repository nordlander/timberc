#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([rtsBrowser], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([rts.js])

# Checks for programs.
AC_PROG_INSTALL

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

# Look for the timber compiler

AC_SUBST(SRC)
AC_SUBST(DEST)
AC_SUBST(TIMBERC)
AC_SUBST(prefix)

AC_CONFIG_FILES([Makefile timberc.cfg])
AC_OUTPUT
