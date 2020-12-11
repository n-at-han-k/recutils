# guile/ Makefile.am
# GNU recutils

# Copyright (C) 2012 Jose E. Marchesi

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This is where libguile-recutils.{so, dylib} will go.
guileextensiondir = $(GUILE_EXTENSION)

guileextension_LTLIBRARIES = libguile-recutils.la
libguile_recutils_la_SOURCES = libguile-recutils.c
libguile_recutils_la_CPPFLAGS = $(GUILE_CFLAGS)
libguile_recutils_la_LDFLAGS = $(GUILE_LDFLAGS) -L$(top_builddir)/src -lrec -module

SNARFCPPOPTS = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(CPPFLAGS) \
               $(CFLAGS) $(AM_CFLAGS) $(GUILE_CFLAGS) -I$(top_builddir)/src

.c.x:
	$(GUILE_SNARF) -o $@ $< $(SNARFCPPOPTS)

# This is where the Scheme modules will go (recutils.scm).
moddir=$(GUILE_SITE)

# This is where the compiled Scheme modules are installed so that
# when someone installs libguile-recutils, the files are already
# compiled.
godir=$(GUILE_SITE_CCACHE)

# The Scheme modules to distribute.
SOURCES = recutils.scm

# Tell make to compile all .scm files.
GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	./pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

# Tests

TEST_EXTENSIONS = .scm

SCM_TESTS = \
  tests/records.scm \
  tests/fields.scm

TESTS = $(SCM_TESTS)

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" GUILE_AUTO_COMPILE=0

SCM_LOG_DRIVER =                            \
  ./pre-inst-env                            \
  $(GUILE) --no-auto-compile -e main        \
  test-driver.scm

AM_SCM_LOG_DRIVER_FLAGS = --brief=no

# Flymake support

check-syntax:
	$(COMPILE) -o /dev/null ${CFLAGS} ${libguile_recutils_la_CPPFLAGS} -S ${CHK_SOURCES} || true

.PHONY: check-syntax

CLEANFILES = $(GOBJECTS) libguile-recutils.x
# End of Makefile.am
