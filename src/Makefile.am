# This is used in PANEL_CFLAGS
cappletname = user-accounts

AM_CPPFLAGS =						\
	-DGNOMELOCALEDIR=\""$(datadir)/locale"\"        \
	-DDATADIR=\""$(pkgdatadir)"\"			\
	-DHAVE_LIBPWQUALITY				\
	-I$(srcdir)/../common/				\
	-I$(srcdir)/../../shell/			\
	-I$(top_srcdir)/libgd/				\
	$(CFLAGS)					\
	$(PANEL_CFLAGS)					\
	$(USER_ACCOUNTS_PANEL_CFLAGS)

if BUILD_CHEESE
AM_CPPFLAGS += $(CHEESE_CFLAGS)
endif

bin_PROGRAMS = user-accounts

BUILT_SOURCES = \
	um-realm-generated.c \
	um-realm-generated.h

user_accounts_SOURCES =			\
	cc-crop-area.h			\
	cc-crop-area.c			\
	um-account-type.h		\
	um-account-type.c 		\
	um-account-dialog.h		\
	um-account-dialog.c		\
	um-history-dialog.h		\
	um-history-dialog.c		\
	um-password-dialog.h		\
	um-password-dialog.c		\
	pw-utils.h			\
	pw-utils.c			\
	um-fingerprint-dialog.h		\
	um-fingerprint-dialog.c		\
	um-utils.h			\
	um-utils.c			\
	fingerprint-strings.h		\
	run-passwd.h			\
	run-passwd.c			\
	um-photo-dialog.h		\
	um-photo-dialog.c		\
	um-realm-manager.c		\
	um-realm-manager.h		\
	um-user-image.h			\
	um-user-image.c			\
	um-cell-renderer-user-image.h	\
	um-cell-renderer-user-image.c	\
	um-user-panel.h 		\
	um-user-panel.c			\
	$(BUILT_SOURCES)

user_accounts_LDADD = 			\
	$(PANEL_LIBS)			\
	$(USER_ACCOUNTS_PANEL_LIBS)	\
	-lpwquality			\
	-lcrypt				\
	-lm				

if BUILD_CHEESE
#user_accounts_SOURCES +=		\
#	um-editable-button.h		\
#	um-editable-button.c		\
#	um-editable-combo.h		\
#	um-editable-combo.c		
#
user_accounts_LDADD += $(CHEESE_LIBS)
endif

um-realm-generated.c: $(top_srcdir)/data/org.freedesktop.realmd.xml
	$(AM_V_GEN) gdbus-codegen --interface-prefix org.freedesktop.realmd. \
		--generate-c-code um-realm-generated --c-namespace UmRealm \
		--annotate "org.freedesktop.realmd.Realm" org.gtk.GDBus.C.Name Common \
		--c-generate-object-manager $<
um-realm-generated.h: um-realm-generated.c

noinst_PROGRAMS = frob-account-dialog

frob_account_dialog_SOURCES = \
	frob-account-dialog.c \
	um-account-dialog.h \
	um-account-dialog.c \
	um-realm-manager.c \
	um-realm-manager.h \
	um-utils.h \
	um-utils.c \
	pw-utils.h \
	pw-utils.c \
	$(BUILT_SOURCES)

frob_account_dialog_LDADD = \
	$(user_accounts_LDADD)

frob_account_dialog_CFLAGS = \
	$(AM_CFLAGS)

CLEANFILES = \
	$(BUILT_SOURCES)

-include $(top_srcdir)/git.mk
