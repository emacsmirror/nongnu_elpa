# configure.ac --- configuration setup for VM

# Author:        Robert Widhopf-Fenk <hack@robf.de>

# Copyright (C) 2006-2007  Robert Widhopf-Fenk <hack@robf.de>
# Copyright (C) 2010       Uday S Reddy <reddyuday@launchpad.net>
# Copyright (C) 2024-2025  The VM Developers

# VM is free software; you can redistribute it and/or modify it under the
# terms of the GNU Library General Public License as published by the Free
# Software Foundation; either version 2 of the License, or (at your option)
# any later version.

# VM is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
# more details.

# You should have received a copy of the GNU Library General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

# Process this file with autoconf to produce a new configure script

AC_PREREQ([2.69])
AC_INIT([VM],[9.0.0snapshot],[viewmail-info@nongnu.org])
AC_COPYRIGHT([Copyright (C) 2024-2025 The VM Developers])
AC_CONFIG_SRCDIR([configure.ac])

# Find a (g)tar program and make sure it is GNU one. A failure is not fatal
# since tar is needed for non critical targets only.
AC_DEFUN([VM_PROG_GNU_TAR],
  [ AC_CHECK_PROGS([TAR], [gtar tar])
    if test "x${TAR}" = "xtar" ; then
      AC_MSG_CHECKING([that tar is GNU tar])
      ${TAR} --version > /dev/null 2>&1 || TAR=
      if test "x${TAR}" = "x" ; then
        AC_MSG_RESULT(no)
      else
        AC_MSG_RESULT(yes)
      fi
    fi
    if test "x${TAR}" = "x" ; then
      AC_MSG_WARN([*** No GNU tar program found.])
      AC_MSG_WARN([*** Some targets will be unavailable.])
    fi ])

# AC_PROG_INSTALL answers the absolute path of the install it chose, and
# config.status rewrites a relative one for each subdirectory, so every
# Makefile carries the path of the machine that ran configure.  VM_INSTALL is
# the same program named so that make looks it up in PATH, where PATH resolves
# `install' to the very file configure chose.
#
# The bare name has to serve GNU, macOS and BSD install alike; all three take
# the -c this passes and the -m 644 the Makefiles add for data.  Where the two
# disagree, configure having passed over the first install in PATH or fallen
# back to install-sh, the answer stays a full path, made absolute here because
# four directories use it and nothing rewrites it for them.
AC_DEFUN([VM_INSTALL_PROG],
  [ AC_MSG_CHECKING([whether install can be named without a path])
    vm_install_bare=no
    case $INSTALL in
      /*" -c")
        vm_install_chosen=`expr "X$INSTALL" : 'X\(.*\) -c$'`
        if test "x`command -v install 2>/dev/null`" = "x$vm_install_chosen" ; then
          vm_install_bare=yes
        fi ;;
    esac
    AC_MSG_RESULT([$vm_install_bare])
    if test $vm_install_bare = yes ; then
      VM_INSTALL="install -c"
    else
      case $INSTALL in
        [[\\/]]* | ?:[[\\/]]* ) VM_INSTALL=$INSTALL ;;
        * ) VM_INSTALL=`pwd`/$INSTALL ;;
      esac
    fi
    AC_SUBST([VM_INSTALL]) ])

# Look for an optional program: warn and carry on when it is missing, saying
# what will not work without it.
AC_DEFUN([VM_PROG_OPTIONAL],
  [ AC_CHECK_PROG([$1], [$2], [$2])
    AS_IF([test "x${$1}" = x],
      [ AC_MSG_WARN([*** No $2 program found.])
        AC_MSG_WARN([*** $3]) ]) ])

# Choose an Emacs according to the --with-emacs user option, or find one.
# We use EMACS_PROG instead of EMACS to avoid colliding with Emacs' own
# internal environment.
AC_DEFUN([VM_PROG_EMACS],
  [ AC_ARG_WITH([emacs],
     AS_HELP_STRING([--with-emacs=PROG],[choose which flavor of Emacs to use]),
      [ EMACS_PROG="${withval}" ],
      [ AC_CHECK_PROGS([EMACS_PROG], [emacs]) ])
    if test "x${EMACS_PROG}" = "x" ; then
      dnl This is critical enough to generate an error and not a warning...
      AC_MSG_ERROR([*** No Emacs program found.])
    fi
    # EMACS TYPE ##################################################
    # It was probed, XEmacs being the other answer.  VM no longer supports
    # XEmacs, and this names the directory that lisp and info install under.
    EMACS_FLAVOR=emacs
    # EMACS VERSION ###############################################
    # Keep VM_MIN_EMACS_VERSION in sync with vm-min-emacs-version in lisp/vm.el,
    # which is the check that matters: VM signals an error as it loads in
    # anything older, so accepting it here only moves the failure to run time.
    # The comparison is left to Emacs rather than the shell, there being no
    # portable way to compare 28.10 with 28.9 in test(1).
    VM_MIN_EMACS_VERSION=28.1
    AC_MSG_CHECKING([checking emacs-version of ${EMACS_PROG}])
    cat > conftest.el <<TEST
(princ (format "%s" emacs-version))
TEST
    EMACS_VERSION=`"${EMACS_PROG}" --no-site-file --batch -l conftest.el | ${GREP} .`
    cat > conftest.el <<TEST
(princ (if (and (fboundp 'version<)
                (not (version< emacs-version "${VM_MIN_EMACS_VERSION}")))
           "ok" "old"))
TEST
    EMACS_VERSION_OK=`"${EMACS_PROG}" --no-site-file --batch -l conftest.el | ${GREP} .`
    if test "x${EMACS_VERSION_OK}" != xok; then
      AC_MSG_ERROR([Emacs version ${EMACS_VERSION} is too old, ${VM_MIN_EMACS_VERSION} is minimum!])
    fi
    # EMACS DEPENDENT SETTINGS ####################################
    AC_MSG_RESULT([${EMACS_VERSION}])

    # Copied from gnus aclocal.m4
    AC_ARG_WITH([lispdir],
      AS_HELP_STRING([--with-lispdir=DIR], [where to install lisp files]),
      [lispdir=${withval}])
    AC_MSG_CHECKING([where .elc files should go])
    if test -z "$lispdir"; then
      dnl Set default value
      theprefix=$prefix
      if test "x$theprefix" = "xNONE"; then
  	theprefix=$ac_default_prefix
      fi
      datarootdir="\$(prefix)/share"
      datadir="${datarootdir}"
      lispdir="${datarootdir}/${EMACS_FLAVOR}/site-lisp/vm"
      for thedir in share lib; do
  	potential=
  	if test -d "${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp"; then
  	   lispdir="${datarootdir}/${EMACS_FLAVOR}/site-lisp/vm"
  	   break
  	fi
      done
    fi
    AC_MSG_RESULT([$lispdir])
    AC_SUBST([lispdir])

    AC_ARG_WITH([etcdir],
      AS_HELP_STRING([--with-etcdir=DIR], [where to install data files]),
      [etcdir=${withval}])
    AC_MSG_CHECKING([where data files should go])
    if test -z "$etcdir"; then
      dnl Set default value
      etcdir="${datadir}/vm"
    fi
    pixmapdir="${etcdir}/pixmaps"
    AC_MSG_RESULT([$etcdir])
    AC_SUBST([etcdir])
    AC_SUBST([pixmapdir])

    AC_ARG_WITH([docdir],
      AS_HELP_STRING([--with-docdir=DIR], [where to install doc files]),
      [docdir=${withval}],
      dnl Set default value. Install ancillary documentation in etcdir,
      dnl following Emacs conventions.
      [docdir="${etcdir}"])
    AC_MSG_CHECKING([where doc files should go])
    AC_MSG_RESULT([$docdir])
    AC_SUBST([docdir])

    AC_SUBST([EMACS_PROG])
    AC_SUBST([EMACS_FLAVOR])
    AC_SUBST([EMACS_VERSION])
])

# copied from gnus aclocal.m4
AC_DEFUN([VM_BUILD_FLAGS], [
  AC_MSG_CHECKING([which options to pass on to Emacs])
  if test "x$FLAGS" = "x"; then
    FLAGS="-batch -q -no-site-file -no-init-file -l \$(srcdir)/vm-build.el"
  fi
  AC_MSG_RESULT([$FLAGS])
  AC_SUBST([FLAGS])
])


# Copied from gnus aclocal.m4 (AC_PATH_INFO_DIR)
AC_DEFUN([VM_PATH_INFO_DIR], [
  AC_MSG_CHECKING([where the TeXinfo docs should go])
  dnl Set default value.  This must be an absolute path.
  AC_MSG_RESULT([$infodir])
  AC_SUBST([infodir])
])

# --with-other-dir option
AC_DEFUN([VM_OTHERDIRS],
  [ AC_ARG_WITH([other-dirs],
      AS_HELP_STRING([--with-other-dirs=DIRS],
        [set other needed lisp directories (a list of semicolon separated paths)]),
      [OTHERDIRS="${withval}"])
  AC_MSG_CHECKING([otherdirs])
  cat > conftest.el <<TEST
    (let ((otherdirs (delete "" (split-string "${OTHERDIRS}" ";")))
    	  absolutedirs
          dir)
      (while otherdirs
        (setq dir (expand-file-name (vm-fix-cygwin-path (car otherdirs)))
	      otherdirs (cdr otherdirs))
        (if (not (file-exists-p dir))
            (error "Directory %S does not exist!" dir)
          (add-to-list 'absolutedirs dir)))
      (princ (format "%S" absolutedirs)))
TEST
  OTHERDIRS=`"${EMACS_PROG}" --no-site-file --batch -l ${srcdir}/lisp/vm-build.el -l conftest.el | tr -d "\r\n"`
  AC_MSG_RESULT([$OTHERDIRS])
  AC_SUBST([OTHERDIRS])
])

# Common system utilities checking:
AC_PROG_MAKE_SET
AC_PROG_INSTALL
VM_INSTALL_PROG
AC_PROG_LN_S
# Substituted into the Makefiles, so these must be bare names: an absolute
# path found here is the build machine's, and a package built in one prefix
# and run in another looks for tools that are not there.
AC_CHECK_PROG([RM], [rm], [rm], [/bin/rm])
AC_CHECK_PROG([MKDIR], [mkdir], [mkdir], [/bin/mkdir])
AC_CHECK_PROG([RMDIR], [rmdir], [rmdir], [/bin/rmdir])
# GREP is used by configure itself and reaches no Makefile.
AC_PATH_PROG([GREP], [grep], [/bin/grep])

# External programs checking:
VM_PROG_GNU_TAR
VM_PROG_OPTIONAL([MAKEINFO], [makeinfo], [Info files will not be built.])
VM_PROG_OPTIONAL([INSTALL_INFO], [install-info],
                 [Info dir file will not be updated on install.])

# Which subdirectories `make' is to enter.  Without makeinfo the info
# directory is left out rather than entered and failed in: an empty MAKEINFO
# made the recipe read `-o vm.info vm.texinfo', whose leading dash told make to
# ignore the error, so the build printed "make: o: No such file or directory"
# and succeeded with no manual (emacs-vm/vm#718).
AS_IF([test "x${MAKEINFO}" = x],
      [ INFO_SUBDIR=
        AC_MSG_NOTICE([*** Not building the info directory.]) ],
      [ INFO_SUBDIR=info ])
AC_SUBST([INFO_SUBDIR])

VM_PROG_EMACS
VM_BUILD_FLAGS
VM_PATH_INFO_DIR
VM_OTHERDIRS

# is there a sane way to set this to a useful default?
AC_ARG_WITH([package-dir],
  AS_HELP_STRING([--with-package-dir=DIR],
    [set the Emacs package directory to DIR]),
  [PACKAGEDIR="${withval}"])
AC_SUBST([PACKAGEDIR])

AC_CONFIG_FILES([Makefile lisp/Makefile info/Makefile pixmaps/Makefile
                 test/Makefile vm-load.el])
AC_OUTPUT

# configure.ac ends here
