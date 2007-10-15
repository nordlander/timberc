#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([timberc], [0.9.9], [nordland@csee.ltu.se])
AC_CONFIG_SRCDIR([Main.hs])
AC_CONFIG_HEADERS([config.h])

# Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL

AC_ARG_ENABLE(hugs,
[AC_HELP_STRING([--enable-hugs],
[Use Hugs as interpreter [default=no]])],
[ if test x"$enableval" = x"yes"; then
        AC_CHECK_PROGS(HCOMPILER, [hugs])
	if test "x$HCOMPILER" = "x"; then
		AC_MSG_ERROR([Hugs not found.])
	fi
  fi
],
[UHCOMPILER=""]
)

AC_ARG_ENABLE(ghc,
[AC_HELP_STRING([--enable-ghc],
[Use GHC as compiler [default=yes]])],
[ if test x"$enableval" = x"yes"; then
        AC_CHECK_PROGS(HCOMPILER, [ghc])
	if test "x$HCOMPILER" = "x"; then
		AC_MSG_ERROR([GHC not found.])
	fi
  fi
],
[UHCOMPILER=""]
)


# No --enable-<compiler>
if test "x$HCOMPILER" = "x"; then
	# Either compile with ghc or hugs.
	AC_CHECK_PROGS(HCOMPILER, [ghc hugs])
fi

if test "x$HCOMPILER" = "x"; then
	AC_MSG_ERROR([No working Haskell compiler found.])
fi

# XXX This is ugly
# For now - hard code the options and it will work.
case $HCOMPILER in
ghc*)
	HFLAGS="-fglasgow-exts -fallow-undecidable-instances --make"
	SRC="Main.hs"
	DEST="-o timberc"
	;;
hugs*)
	HFLAGS="-98 -P{Hugs}/libraries -h2500000"
	SRC=""
	DEST="Main"
	;;
esac

# Look for Happy, otherwise touch Parser.hs to give current time stamp.
AC_CHECK_PROGS(HAPPY, [happy])

if test "x$HAPPY" = "x"; then
	AC_MSG_WARN([Happy not found, you can not change the grammar.])	
	touch $srcdir/Parser.hs
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

CFLAGS="$CFLAGS -c"

AC_SUBST(CFLAGS)
AC_SUBST(SRC)
AC_SUBST(prefix)
AC_SUBST(HFLAGS)
AC_SUBST(DEST)

AC_CONFIG_FILES([Makefile CompileConfig.hs rtsTrivial/timberc.cfg rtsPOSIX/timberc.cfg])
AC_OUTPUT
