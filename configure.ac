AC_INIT([user-accounts], [0.3],
        [https://github.com/matijaskala/user-accounts/issues])

AC_CONFIG_SRCDIR([src])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 no-dist-gzip dist-xz tar-ustar check-news])
AM_MAINTAINER_MODE([enable])
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

# Check for programs
AC_PROG_CC
AM_PROG_CC_C_O
AC_HEADER_STDC

# Initialize libtool
LT_PREREQ([2.2])
LT_INIT

# Internationalization support

IT_PROG_INTLTOOL([0.40.1])

GETTEXT_PACKAGE=user-accounts
AC_SUBST(GETTEXT_PACKAGE)
AC_DEFINE_UNQUOTED(GETTEXT_PACKAGE, "$GETTEXT_PACKAGE", [Gettext package])

AC_PATH_XTRA

AC_PATH_PROG([GLIB_MKENUMS],[glib-mkenums])

AC_CHECK_LIB(m, floor)

dnl ==============================================
dnl Check that we meet the  dependencies
dnl ==============================================

GLIB_REQUIRED_VERSION=2.39.91
GDKPIXBUF_REQUIRED_VERSION=2.23.0
POLKIT_REQUIRED_VERSION=0.103
SCHEMAS_REQUIRED_VERSION=3.15.4
ACCOUNTSSERVICE_REQUIRED_VERSION=0.6.39
PWQUALITY_REQUIRED_VERSION=1.2.2

PKG_CHECK_MODULES(USER_ACCOUNTS_PANEL, gtk+-2.0
                  glib-2.0 >= $GLIB_REQUIRED_VERSION
                  gthread-2.0
                  gio-2.0
                  gio-unix-2.0
                  gsettings-desktop-schemas >= $SCHEMAS_REQUIRED_VERSION
                  polkit-gobject-1 >= $POLKIT_REQUIRED_VERSION
                  gdk-pixbuf-2.0 >= $GDKPIXBUF_REQUIRED_VERSION
                  pwquality >= $PWQUALITY_REQUIRED_VERSION
                  accountsservice >= $ACCOUNTSSERVICE_REQUIRED_VERSION)

GDESKTOP_PREFIX=`$PKG_CONFIG --variable prefix gsettings-desktop-schemas`
AC_SUBST(GDESKTOP_PREFIX)

AC_ARG_WITH([faces],
            AS_HELP_STRING([--without-faces], [do not install user pixmaps]),,
            with_faces=yes)
AM_CONDITIONAL(INSTALL_FACES, test x${with_faces} = xyes)

# Optional dependency for the user accounts panel
AC_ARG_WITH([cheese],
            AS_HELP_STRING([--with-cheese], [enable cheese webcam support]),,
            with_cheese=auto)

if test x"$with_cheese" != x"no" ; then
   # No webcam support at this time
   PKG_CHECK_MODULES(CHEESE, gstreamer-1.0 cheese >= 3.5.91, [have_cheese=no], [have_cheese=no])
   if test x${have_cheese} = xyes; then
     AC_DEFINE(HAVE_CHEESE, 1, [Define to 1 to enable cheese webcam support])
   fi
   if test x${with_cheese} = xyes && test x${have_cheese} = xno; then
     AC_MSG_ERROR([Cheese configured but not found])
   fi
else
   have_cheese=no
fi
AM_CONDITIONAL(BUILD_CHEESE, test x${have_cheese} = xyes)

# Kerberos kerberos support
AC_ARG_ENABLE([kerberos],
              AS_HELP_STRING([--enable-kerberos], [enable kerberos support]),,
              enable_kerberos=no)

if test x"enable_kerberos" != x"no" ; then
    AC_PATH_PROG(KRB5_CONFIG, krb5-config, no)
    if test "$KRB5_CONFIG" = "no"; then
      AC_MSG_ERROR([krb5-config executable not found in your path - should be installed with the kerberos libraries])
    fi

    AC_MSG_CHECKING(for krb5 libraries and flags)
    KRB5_CFLAGS="`$KRB5_CONFIG --cflags`"
    KRB5_LIBS="`$KRB5_CONFIG --libs`"
    AC_MSG_RESULT($KRB5_CFLAGS $KRB5_LIBS)
    AC_DEFINE(HAVE_KERBEROS, 1, [defined if krb5 is available])
fi

AC_SUBST(KRB5_CFLAGS)
AC_SUBST(KRB5_LIBS)

USER_ACCOUNTS_PANEL_CFLAGS="$USER_ACCOUNTS_PANEL_CFLAGS $KRB5_CFLAGS"
USER_ACCOUNTS_PANEL_LIBS="$USER_ACCOUNTS_PANEL_LIBS $KRB5_LIBS"

dnl =======================================
dnl Panels
dnl =======================================

PANEL_CFLAGS="-DG_LOG_DOMAIN=\"\\\"\$(cappletname)-cc-panel\\\"\""
AC_SUBST(PANEL_CFLAGS)

PANEL_LIBS=""
AC_SUBST(PANEL_LIBS)

dnl =======================================
dnl Update Mime Database
dnl =======================================

AC_PATH_PROG(UPDATE_MIME_DATABASE, update-mime-database, no)

AC_ARG_ENABLE(update-mimedb,
   AS_HELP_STRING([--disable-update-mimedb],
                  [do not update mime database after installation]),,
                   enable_update_mimedb=yes)
AM_CONDITIONAL(ENABLE_UPDATE_MIMEDB, test x$enable_update_mimedb = xyes)

dnl =======================================
dnl Finish
dnl =======================================

# Turn on the additional warnings last

AC_ARG_ENABLE(more-warnings,
              AS_HELP_STRING([--enable-more-warnings],
                             [Maximum compiler warnings]),
              set_more_warnings="$enableval",[
	      if test -d $srcdir/.git; then
	        set_more_warnings=yes
	      else
	        set_more_warnings=no
              fi])
AC_MSG_CHECKING(for more warnings)
if test "$GCC" = "yes" -a "$set_more_warnings" != "no"; then
        AC_MSG_RESULT(yes)
        CFLAGS="\
        -Wall -Wclobbered -Wempty-body -Wignored-qualifiers \
        -Wmissing-field-initializers -Wmissing-parameter-type \
        -Wold-style-declaration -Woverride-init -Wtype-limits \
        -Wuninitialized \
        -Wchar-subscripts -Wmissing-declarations -Wmissing-prototypes \
        -Wnested-externs -Wpointer-arith \
        -Wcast-align -Wsign-compare \
        $CFLAGS"

	# Only add this when optimizing is enabled (default)
	AC_MSG_CHECKING([whether optimization is enabled])
	AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[#if __OPTIMIZE__ == 0
	                                     #error No optimization
	                                     #endif
	                                   ]], [[]])],
	                  [has_optimization=yes],
	                  [has_optimization=no])
	if test $has_optimization = yes; then
		CFLAGS="$CFLAGS -Wp,-D_FORTIFY_SOURCE=2"
	fi
	AC_MSG_RESULT($has_optimization)

        for option in -Wno-strict-aliasing -Wno-sign-compare; do
                SAVE_CFLAGS="$CFLAGS"
                CFLAGS="$CFLAGS $option"
                AC_MSG_CHECKING([whether gcc understands $option])
                AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]], [[]])],
                                  [has_option=yes],
                                  [has_option=no])
                if test $has_option = no; then
                        CFLAGS="$SAVE_CFLAGS"
                fi
                AC_MSG_RESULT($has_option)
                unset has_option
                unset SAVE_CFLAGS
        done
        unset option
else
        AC_MSG_RESULT(no)
fi

AC_CONFIG_FILES([
Makefile
src/Makefile
data/Makefile
data/user-accounts.desktop.in
data/faces/Makefile
data/icons/Makefile
po/Makefile.in
])

AC_OUTPUT

if test "x$have_cheese" = "xyes"; then
        AC_MSG_NOTICE([Webcam support enabled])
else
        AC_MSG_NOTICE([Webcam support disabled])
fi
