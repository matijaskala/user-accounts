SUBDIRS = icons
if INSTALL_FACES
SUBDIRS += faces
endif
NULL =

@INTLTOOL_DESKTOP_RULE@

desktopdir = $(datadir)/applications
Desktop_in_files = user-accounts.desktop.in
desktop_DATA = $(Desktop_in_files:.desktop.in=.desktop)

uidir=$(pkgdatadir)
ui_DATA=				\
	account-dialog.ui		\
	account-fingerprint.ui		\
	history-dialog.ui		\
	password-dialog.ui		\
	user-accounts-dialog.ui		\
	$(NULL)

EXTRA_DIST = 				\
	user-accounts.desktop.in.in	\
	$(ui_DATA)			\
	org.freedesktop.realmd.xml

CLEANFILES =					\
	$(desktop_DATA)				\
	$(NULL)

-include $(top_srcdir)/git.mk
