;;; debian-changelog-mode.el --- change log maintenance for Debian changelogs

;; Keywords: maint

;; Copyright (C) 1996 Ian Jackson
;; Copyright (C) 1997 Klee Dienes
;; Copyright (C) 1999 Chris Waters
;; Copyright (C) 2000, 2001, 2002 Peter S Galbraith
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; debian-changelog-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your Debian installation, in /usr/share/common-licenses/GPL
;; If not, write to the Free Software Foundation, 675 Mass Ave,
;; Cambridge, MA 02139, USA.
;;
;; New versions of the file may be found at 
;;   http://people.debian.org/~psg/debian-changelog-mode.el
;; ----------------------------------------------------------------------------
;;; Change log:
;;
;; V1.00 30aug00  Peter S Galbraith <psg@debian.org>
;;  -  Prior version had no changelogs; starting one now.
;;     This is the potato version plus extensions by Chris Waters (easymenu; 
;;     better menus, font-lock support).
;; V1.01 30aug00  Peter S Galbraith <psg@debian.org>
;;  - debian-changelog-finalise-last-version: Use XEmacs' (user-mail-address)
;;    function if variable user-mail-address is undefined. 
;;    Thanks to Robert Bihlmeyer <robbe@orcus.priv.at>, closes Bug#61524
;;  - debian-changelog-finalise-last-version: Takes account of some env vars
;;    Thanks to Rafael Laboissiere <rafael@icp.inpg.fr>, closes Bug#61226
;;  - debian-changelog-close-bug:  new command.
;; V1.02 23Feb01  Peter S Galbraith <psg@debian.org>
;;  - Added `debian-changelog-suggest-version', a mechanisn for guessing 
;;    what the new version number should be.
;;    Closes half of Bug#85412
;; V1.03 23Feb01  Peter S Galbraith <psg@debian.org>
;;  - Fixed `fill-paragraph' by tweaks to paragraph-start and 
;;    paragraph-separate variables.
;;    Closes second half of Bug#85412
;; V1.04 23Feb01  Peter S Galbraith <psg@debian.org>
;;  - Added `debian-changelog-web-bugs' `debian-changelog-web-packages'
;;    `debian-changelog-web-package'
;; V1.05 23Feb01  Peter S Galbraith <psg@debian.org>
;;  - made `debian-changelog-suggest-package-name' more picky about finding 
;;    an acceptable name.
;; V1.06 28Feb01  Peter S Galbraith <psg@debian.org>
;;  - Create customizable variables debian-changelog-full-name and 
;;    debian-changelog-mailing-address.  
;;  - Make debian-changelog-finalise-last-version use them.
;; V1.07 28Feb01  Peter S Galbraith <psg@debian.org>
;;  - debian-changelog-suggest-version: Handle epochs!
;;    closes: Bug#87964: dpkg-dev-el: does wrong things with epochs
;; V1.08 07Mar01  Peter S Galbraith <psg@debian.org>
;;    debian-changelog-suggest-version: Handle package names with hyphens!
;;    closes: #88589 and #88245
;; V1.09 09Mar01  Peter S Galbraith <psg@debian.org>
;;    debian-changelog-suggest-version: better regexps for version numbers
;;    Created debian-changelog-increment-version
;; V1.10 10Mar01  Peter S Galbraith <psg@debian.org>
;;    tweaks docs for debian-changelog-mode function concerning 
;;    add-log-mailing-address (now obsolete).
;; V1.11 24Apr01  Peter S Galbraith <psg@debian.org>
;;    Add stuff to try to trim out obsolete "Local Variables:" block from 
;;    changelog files.
;; V1.12 24Apr01  Peter S Galbraith <psg@debian.org>
;;    Modify font-lock code. closes: #93243
;; V1.13 27Apr01  Peter S Galbraith <psg@debian.org>
;;    Move code concerning local variables near beginning of file such that
;;    `hack-local-variables' doesn't complain. 
;; V1.14 30Apr01  Peter S Galbraith <psg@debian.org>
;;    Add `critical' bug severity (see http://bugs.debian.org/94475)
;; V1.15 30Apr01  Peter S Galbraith <psg@debian.org>
;;    Tweak font-locking bug number regexp to match dpkg-parsechangelog 1.9.1
;; V1.16 30Apr01  Peter S Galbraith <psg@debian.org>
;;    Added debian-changelog-web-bug (will bound to a mouse button later)
;; V1.17 30Apr01  Peter S Galbraith <psg@debian.org>
;;    debian-changelog-increment-version: Handle 3.5.4.0 case (single digits)
;;    closes: #95831
;; V1.18 30Apr01  Peter S Galbraith <psg@debian.org>
;;    Add mouse interface to web-bug (with green highlight).
;; V1.19 01May01  Peter S Galbraith <psg@debian.org>
;;    Add imenu support as `History'.  Bug: The history menu is empty when
;;    point is on the (mouse-highlighted) bug number (using emacs-20.7).
;; V1.20 02May01  Peter S Galbraith <psg@debian.org>
;;    Leave `mode: debian-changelog-mode' alone for native packages.
;; V1.21 02May01  Peter S Galbraith <psg@debian.org>
;;    Fix empty History menu when on bug numbers.
;; V1.22 02May01  Peter S Galbraith <psg@debian.org>
;;    Fontify version number (e.g. NMU in warning-face)
;; V1.23 02May01  Peter S Galbraith <psg@debian.org>
;;    Bypass imenu-progress-message because it breaks byte-compilation (?)
;; V1.24 03May01  Peter S Galbraith <psg@debian.org>
;;    Correct fix for imenu-progress-message macro (can't rely on variable
;;    defined here for loading of imenu during byte-compilation).
;; V1.25 04May01  Peter S Galbraith <psg@debian.org>
;;    Add `experimental' distribution.
;; V1.26 04May01  Peter S Galbraith <psg@debian.org>
;;    Web site changed the URL for package searches:
;;    http://cgi.debian.org/cgi-bin -> http://packages.debian.org/cgi-bin
;; V1.27 04May01  Peter S Galbraith <psg@debian.org>
;;    Set new version to `experimental' when last one was set to that.
;;    closes: #96260: Default to the same distribution as the previous release 
;; V1.28 04May01  Peter S Galbraith <psg@debian.org>
;;    Make `set-distribution' and `set-urgency' unavailable when changelog 
;;    is finalised (error at command line and menu grayed-out). 
;; V1.29 04May01  Peter S Galbraith <psg@debian.org>
;;    Add-to auto-mode-alist in case not using dpkg-dev-el package.
;; V1.30 09May01  Peter S Galbraith <psg@debian.org>
;;    Fixed brain-damaged auto-mode-alist added in V1.29 (*blush*).
;; V1.31 28May01  Peter S Galbraith <psg@debian.org>
;;    Fix typo (closes: #98577).
;;    Add a message display after each call to browse-url.
;; V1.32 28May01  Peter S Galbraith <psg@debian.org>
;;  - XEmacs21's easy-menu-define doesn't like :active.
;;  - XEmacs21 need easy-menu-add call in mode setup.  
;;  - debian-changelog-setheadervalue: check at this lower level if finalised.
;; V1.33 29May01  Peter S Galbraith <psg@debian.org>
;;    Fix History IMenu for XEmacs21 (it doesn't autoload 
;;    match-string-no-properties).
;; V1.34 29May01  Peter S Galbraith <psg@debian.org>
;;  - debian-changelog-fontify-version: allow version numbers with many hyphens
;;  - debian-changelog-suggest-version: heavy changes to deal with many hyphens
;; V1.35 06Jun01 Peter S Galbraith <psg@debian.org>
;;  - patch from Brian Warner <warner@lothar.com> to make 
;;    debian-changelog-local-variables-maybe-remove-done really buffer-local. 
;;  - Change another occurrence of make-local-variable.
;; V1.36 11Jun01 Peter S Galbraith <psg@debian.org>
;;    changed urgency "critical" to "emergency".
;;    See http://lists.debian.org/debian-policy-0106/msg00095.html
;; V1.37 11Jun01 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-suggest-version: another tweak when upstream version
;;    number contains hyphens (closes: #100162).
;; V1.38 13Jun01 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-suggest-version: peppered regexp-quote at various places
;;    to match package names and version that contain regexp characters.
;; V1.39 13Jun01 Peter S Galbraith <psg@debian.org>
;;    change (provide 'debian-changelog) to (provide 'debian-changelog-mode)
;;    (closes: #100639)  Thanks *again* Yann Dirson!
;; V1.40 22Jun01 Peter S Galbraith <psg@debian.org>
;;    Changed urgency "emergency" back to "critical" (!)
;;    See http://lists.debian.org/debian-policy-0106/msg00240.html
;; V1.41 04Jul01 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-finalised-p updated by Tommi Virtanen <tv@debian.org>
;;    (closes: #102088)
;; V1.42 10Jul01 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-finalised-p: tweak regexp (really closes: #102088)
;; V1.43 25Jul01 Peter S Galbraith <psg@debian.org>
;;    font-lock enforces 2 space exactly between email and date.
;; V1.44 26Jul01 Peter S Galbraith <psg@debian.org>
;;     No conditions left to keep variable block (See bug #105889)
;;     - Removed debian-changelog-package-native-p function.
;;     - Removed debian-changelog-local-variables-email-p function.
;;     - Removed debian-changelog-local-variables-remove-address function.
;;     - Removed debian-changelog-local-variables-remove-mode function.
;;     - Created debian-changelog-local-variables-remove function.
;; V1.45 15Aug01 Peter S Galbraith <psg@debian.org>
;;    Bug list menu added (via wget).
;; V1.46 15Aug01 Roland Mas <lolando@debian.org>
;;    One-character tweak to package name font-lock regexp.
;; V1.47 15Aug01 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-web-bug: bug fix when called from menu
;; V1.48 19Sep01 Brian Warner <warner@lothar.com>
;;  - move to end of file before prompting for removal of local variables.
;;  - remove global def of debian-changelog-local-variables-maybe-remove-done.
;; V1.49 22Nov01 Roland Mas <lolando@debian.org>
;;    debian-changelog-suggest-version: tweak regexp for case of upstream
;;    version number with a single character.
;; V1.50 30Nov01 Roland Mas <lolando@debian.org>
;;    replaced debian-changelog.el by debian-changelog-mode.el
;; V1.51 24Jan02 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-web-bugs: return all bugs for the source package.
;; V1.52 07Feb02 Peter S Galbraith <psg@debian.org>
;;    debian-changelog-build-bug-menu: return all bugs for the source package.
;; ----------------------------------------------------------------------------
;; TO DO List:
;;  - Menu to close bugs with each bug having a menu entry.
;;  - Insert bug description when inserting closes: statement.
;; ----------------------------------------------------------------------------
;;; Acknowledgements:  (These people have contributed)
;;   Roland Rosenfeld <roland@debian.org>
;;   James LewisMoss <dres@ioa.com>
;;   Rafael Laboissiere <rafael@icp.inpg.fr>
;;   Brian Warner <warner@lothar.com>
;;   Yann Dirson <dirson@debian.org>

(cond
 ((not (fboundp 'defcustom))
  (defvar debian-changelog-full-name (or (getenv "DEBFULLNAME")
                                         (user-full-name))
    "*Full name of user, for inclusion in Debian changelog headers.
This defaults to the contents of environment variable DEBFULLNAME
or else to the value returned by the `user-full-name' function.")

  (defvar debian-changelog-mailing-address 
    (or (getenv "DEBEMAIL")
	(getenv "EMAIL")
	(and (boundp 'user-mail-address) user-mail-address)
	(and (fboundp 'user-mail-address) (user-mail-address)))
    "*Electronic mail address of user, for inclusion in Debian changelog headers.
This defaults to the value of (in order of precedence):
 Contents of environment variable DEBEMAIL,
 Contents of environment variable EMAIL,
 Value of `user-mail-address' variable,
 Value returned by the `user-mail-address' function.")

  (defvar debian-changelog-local-variables-maybe-remove t 
    "*Ask to remove obsolete \"Local Variables:\" block from changelog.")

  (defvar debian-changelog-highlight-mouse-t t
    "*Use special overlay for bug numbers, defining mouse-3 to web interface.")

  (defvar debian-changelog-use-imenu (fboundp 'imenu-add-to-menubar)
    "*Use imenu package for debian-changelog-mode?
If you do not wish this behaviour, reset it in your .emacs file like so:

  (setq debian-changelog-use-imenu nil)")
  ) 
 (t
  (defgroup debian-changelog nil "Debian changelog maintenance"
    :group 'tools
    :prefix "debian-changelog-")

  (defgroup debian-changelog-faces nil
    "Faces for fontifying text in debian-changelog."
    :prefix "debian-changelog-"
    :group 'debian-changelog)

  (defcustom debian-changelog-full-name (or (getenv "DEBFULLNAME")
                                            (user-full-name))
    "*Full name of user, for inclusion in Debian changelog headers.
This defaults to the contents of environment variable DEBFULLNAME
or else to the value returned by the `user-full-name' function."
    :group 'debian-changelog
    :type 'string)

  (defcustom debian-changelog-mailing-address 
    (or (getenv "DEBEMAIL")
	(getenv "EMAIL")
	(and (boundp 'user-mail-address) user-mail-address)
	(and (fboundp 'user-mail-address) (user-mail-address)))
    "*Electronic mail address of user, for inclusion in Debian changelog headers.
This defaults to the value of (in order of precedence):
 Contents of environment variable DEBEMAIL,
 Contents of environment variable EMAIL,
 Value of `user-mail-address' variable,
 Value returned by the `user-mail-address' function."
    :group 'debian-changelog
    :type 'string)

  (defcustom debian-changelog-local-variables-maybe-remove t
    "*Ask to remove obsolete \"Local Variables:\" block from changelog
under certain conditions."
    :group 'debian-changelog
    :type 'boolean)

  (defcustom debian-changelog-highlight-mouse-t t
    "*Use special overlay for bug numbers, defining mouse-3 to web interface."
    :group 'debian-changelog
    :type 'boolean)

  (defcustom debian-changelog-use-imenu (fboundp 'imenu-add-to-menubar) 
    "*Use imenu package for debian-changelog-mode?
If you do not wish this behaviour, reset it in your .emacs file like so:

  (setq debian-changelog-use-imenu nil)"
    :group 'debian-changelog
    :type 'boolean)
  ))

(require 'add-log)
(require 'easymenu)
(eval-when-compile
  (require 'cl))
(if (not (fboundp 'match-string-no-properties))
    (load "poe" t t))                   ;XEmacs21.1 doesn't autoload this

;;
;; Clean up old "Local Variables:" entries
;; Peter Galbraith

;; **Important note**
;;
;;  If we get the following warning:
;;
;;   File local-variables error: (error "Local variables entry is missing the prefix")
;;
;;  when installing the dpkg-dev-el package, it's because the command
;;  (hack-local-variables) from files.el is bailing on all the "Local
;;  Variables:" strings in this file.  The simplest solution is to keep all
;;  occurrences of this string before the last 3000 characters of the file,
;;  where `hack-local-variables' starts looking:

;; First, I made the add-log-mailing-address variable obsolete but still 
;; left the "mode:" line in the variable block for Debian native packages
;; because it was impossible to tell what they were from the installed
;; changelog.gz name.  In bug #105889, I came up with code to stick in
;; /etc/emacs/site-start.d/50dpkg-dev-el.el to figure that out in a
;; find-file-hooks hook.  So now the variable block is completely obsolete.
(defun debian-changelog-local-variables-maybe-remove ()
  "Ask to remove local variables block if buffer not read-only."
  (interactive)
  (if (or debian-changelog-local-variables-maybe-remove-done
          buffer-read-only)
      nil
    (setq debian-changelog-local-variables-maybe-remove-done t)
    (if (debian-changelog-local-variables-exists-p)
	(save-excursion
	  (goto-char (point-max)) ; local vars are always at end
	  (if (yes-or-no-p 
	       "Remove obsolete \"local variables:\" from changelog? ")
	      (debian-changelog-local-variables-remove))))))

(defun debian-changelog-local-variables-exists-p ()
  "Return t if package has a \"Local Variables:\" block."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-max))
      (and (re-search-backward "^local variables:" nil t)
           (or (re-search-forward "add-log-mailing-address:" nil t)
               (re-search-forward "mode: debian-changelog" nil t))))))

(defun debian-changelog-local-variables-remove ()
  "Remove add-log-mailing-address entry from local variables block."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-max))
      ;; Remove add-log-mailing-address: line if it exists
      (if (and (re-search-backward "^local variables:" nil t)
	       (re-search-forward "add-log-mailing-address: .+\n" nil t))
          (delete-region (match-beginning 0)(match-end 0)))
      (goto-char (point-max))
      ;; Remove "mode: debian-changelog" line if it exists
      (if (and (re-search-backward "^local variables:" nil t)
	       (re-search-forward "mode: debian-changelog.*\n" nil t))
          (delete-region (match-beginning 0)(match-end 0)))
      (goto-char (point-max))
      ;; Remove empty variable block if it exists
      (if (re-search-backward "^local variables: *\nend:" nil t)
          (delete-region (match-beginning 0)(match-end 0))))))

;;
;; internal functions: getheadervalue and setheadervalue both use a
;; regexp to probe the changelog entry for specific fields.

;; warning: if used with a "re" that doesn't have at least one group,
;; the results will be unpredictable (to say the least).

(defun debian-changelog-setheadervalue (re str)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let ((lineend (save-excursion (end-of-line)(point))))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward re lineend t)
	  (let ((a (match-beginning 1))
		(b (match-end 1)))
	    (goto-char a)
	    (delete-region a b)
	    (insert str))))))

(defun debian-changelog-getheadervalue (re)
  (let ((lineend (save-excursion (end-of-line) (point))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward re lineend)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))

;;
;; some get/set functions for specific fields
;;  (Chris Waters)

(defun debian-changelog-seturgency (val)
  (debian-changelog-setheadervalue "\\;[^\n]* urgency=\\(\\sw+\\)" val))
(defun debian-changelog-geturgency ()
  (debian-changelog-getheadervalue "\\;[^\n]* urgency=\\(\\sw+\\)"))
(defun debian-changelog-setdistribution (val)
  (debian-changelog-setheadervalue ") \\(.*\\)\\;" val))
(defun debian-changelog-getdistribution ()
  (debian-changelog-getheadervalue ") \\(.*\\)\\;"))

;;
;; keymap table definition
;;

(defvar debian-changelog-mode-map nil
  "Keymap for Debian changelog major mode.")
(if debian-changelog-mode-map
    nil
  (setq debian-changelog-mode-map (make-sparse-keymap))
  (define-key debian-changelog-mode-map "\C-c\C-a" 
    'debian-changelog-add-entry)
  (define-key debian-changelog-mode-map "\C-c\C-b"
    'debian-changelog-close-bug)
  (define-key debian-changelog-mode-map "\C-c\C-f"
    'debian-changelog-finalise-last-version)
  (define-key debian-changelog-mode-map "\C-c\C-c" 
    'debian-changelog-finalise-and-save)
  (define-key debian-changelog-mode-map "\C-c\C-v" 
    'debian-changelog-add-version)
  (define-key debian-changelog-mode-map "\C-c\C-d" 
    'debian-changelog-distribution)
  (define-key debian-changelog-mode-map "\C-c\C-u" 
    'debian-changelog-urgency)
  (define-key debian-changelog-mode-map "\C-c\C-e"
    'debian-changelog-unfinalise-last-version))

;;
;; menu definition (Chris Waters)
;;

(defvar debian-changelog-is-XEmacs
  (and 
   (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version))))
   (= 21 emacs-major-version)))

(cond
 (debian-changelog-is-XEmacs
(easy-menu-define
 debian-changelog-menu debian-changelog-mode-map "Debian Changelog Mode Menu"
 '("Changelog"
   ["New Version" debian-changelog-add-version (debian-changelog-finalised-p)]
   ["Add Entry" debian-changelog-add-entry
    (not (debian-changelog-finalised-p))]
   ["Close bug" debian-changelog-close-bug
    (not (debian-changelog-finalised-p))]
   "--"
   ("Set Distribution"
    ["unstable" (debian-changelog-setdistribution "unstable") t]
    ["frozen" (debian-changelog-setdistribution "frozen") t]
    ["stable" (debian-changelog-setdistribution "stable") t]
    ("--")
    ["frozen unstable" (debian-changelog-setdistribution "frozen unstable") t]
    ["stable unstable" (debian-changelog-setdistribution "stable unstable") t]
    ["stable frozen" (debian-changelog-setdistribution "stable frozen") t]
    ["stable frozen unstable" (debian-changelog-setdistribution
			       "stable frozen unstable") t]
    ("--")
    ["experimental" (debian-changelog-setdistribution "experimental") t])
   ("Set Urgency"
    ["low" (debian-changelog-seturgency "low") t]
    ["medium" (debian-changelog-seturgency "medium") t]
    ["high" (debian-changelog-seturgency "high") t]
    ["critical" (debian-changelog-seturgency "critical") t])
   "--"
   ["Unfinalise" debian-changelog-unfinalise-last-version 
    (debian-changelog-finalised-p)]
   ["Finalise" debian-changelog-finalise-last-version
    (not (debian-changelog-finalised-p))]
   ["Finalise+Save" debian-changelog-finalise-and-save
    (not (debian-changelog-finalised-p))]
   "--"
   "Access www.debian.org"
   ["Bugs for this package" (debian-changelog-web-bugs) t]
   ["Specific bug number" (debian-changelog-web-bug) t]
   ["Package list (all archives)" (debian-changelog-web-packages) t]
   ("Package web pages..." 
    ["stable" (debian-changelog-web-package "stable") t]
    ["testing" (debian-changelog-web-package "testing") t]
    ["unstable" (debian-changelog-web-package "unstable") t])
   "--"
   ["Customize" (customize-group "debian-changelog") (fboundp 'customize-group)])))
 (t
(easy-menu-define 
 debian-changelog-menu debian-changelog-mode-map "Debian Changelog Mode Menu"
 '("Changelog"
   ["New Version" debian-changelog-add-version (debian-changelog-finalised-p)]
   ["Add Entry" debian-changelog-add-entry
    (not (debian-changelog-finalised-p))]
   ["Close bug" debian-changelog-close-bug
    (not (debian-changelog-finalised-p))]
   "--"
   ("Set Distribution"     :active (not (debian-changelog-finalised-p))
    ["unstable" (debian-changelog-setdistribution "unstable") t]
    ["frozen" (debian-changelog-setdistribution "frozen") t]
    ["stable" (debian-changelog-setdistribution "stable") t]
    ("--")
    ["frozen unstable" (debian-changelog-setdistribution "frozen unstable") t]
    ["stable unstable" (debian-changelog-setdistribution "stable unstable") t]
    ["stable frozen" (debian-changelog-setdistribution "stable frozen") t]
    ["stable frozen unstable" (debian-changelog-setdistribution
			       "stable frozen unstable") t]
    ("--")
    ["experimental" (debian-changelog-setdistribution "experimental") t])
   ("Set Urgency"     :active (not (debian-changelog-finalised-p))
    ["low" (debian-changelog-seturgency "low") t]
    ["medium" (debian-changelog-seturgency "medium") t]
    ["high" (debian-changelog-seturgency "high") t]
    ["critical" (debian-changelog-seturgency "critical") t])
   "--"
   ["Unfinalise" debian-changelog-unfinalise-last-version 
    (debian-changelog-finalised-p)]
   ["Finalise" debian-changelog-finalise-last-version
    (not (debian-changelog-finalised-p))]
   ["Finalise+Save" debian-changelog-finalise-and-save
    (not (debian-changelog-finalised-p))]
   "--"
   "Access www.debian.org"
   ["Bugs for this package" (debian-changelog-web-bugs) t]
   ["Specific bug number" (debian-changelog-web-bug) t]
   ["Package list (all archives)" (debian-changelog-web-packages) t]
   ("Package web pages..." 
    ["stable" (debian-changelog-web-package "stable") t]
    ["testing" (debian-changelog-web-package "testing") t]
    ["unstable" (debian-changelog-web-package "unstable") t])
   "--"
   ["Customize" (customize-group "debian-changelog") (fboundp 'customize-group)]))))

;;
;; interactive function to add a new line to the changelog
;;

(defun debian-changelog-add-entry ()
  "Add a new change entry to a debian-style changelog."
  (interactive)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (goto-char (point-min))
  (re-search-forward "\n --")
  (backward-char 5)
  (if (prog1 (looking-at "\n") (forward-char 1))
      nil
    (insert "\n"))
  (insert "  * ")
  (save-excursion (insert "\n")))

;;
;; interactive function to close bugs by number. (Peter Galbraith)
;;

(defun debian-changelog-close-bug ()
  "Add a new change entry to close a bug number."
  (interactive)
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let ((bug-number (completing-read 
                     "Bug number to close: " 
                     debian-changelog-bug-alist nil nil)))
    (if (not (string-match "^[0-9]+$" bug-number))
        (error "The bug number should consists of only digits."))
    (debian-changelog-add-entry)
    (save-excursion (insert " (closes: #" bug-number ")"))
    (message "Enter a brief description of what was done here.")))

;;
;; interactive functions to set urgency and distribution
;;

(defun debian-changelog-distribution (arg)
  "Delete the current distribution and prompt for a new one."
  (interactive "P")
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let* ((curstr (debian-changelog-getdistribution))
	 (str (completing-read 
	       "Select distribution: "
	       '(("stable" 1)
		 ("frozen" 2)
		 ("unstable" 3) 
		 ("stable frozen unstable" 4)
		 ("stable unstable frozen" 4)
		 ("unstable stable frozen" 4)
		 ("unstable frozen stable" 4)
		 ("frozen unstable stable" 4)
		 ("frozen stable unstable" 4)
		 ("frozen unstable" 5)
		 ("unstable frozen" 5)
		 ("stable frozen" 6)
		 ("frozen stable" 6)
		 ("stable unstable" 7)
		 ("unstable stable" 7)
		 ("experimental" 8))
	       nil t nil)))
    (if (not (equal str ""))
	(debian-changelog-setdistribution str))))

(defun debian-changelog-urgency (arg)
  "Delete the current urgency and prompt for a new one."
  (interactive "P")
  (if (eq (debian-changelog-finalised-p) t)
      (error (substitute-command-keys "most recent version has been finalised - use \\[debian-changelog-unfinalise-last-version] or \\[debian-changelog-add-version]")))
  (let* ((curstr (debian-changelog-geturgency))
	 (str (completing-read 
	       "Select urgency: "
	       '(("low" 1) ("medium" 2) ("high" 3) ("critical" 4))
	       nil t nil)))
    (if (not (equal str ""))
	(debian-changelog-seturgency str))))

;;
;; internal function: test if changelog has been finalized or not
;; New version by Tommi Virtanen <tv@debian.org>
;; Sun, 24 Jun 2001 16:03:01 UTC;  Debian bug #102088
;; -
;; regexp tweaked by psg, Tue Jul 10 15:29:54 EDT 2001

(defun debian-changelog-finalised-p ()
  "Check whether the most recent debian-style changelog entry is
finalised yet (ie, has a maintainer name and email address and a
release date."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "\n\\S-" (point-max) t)
	(goto-char (point-max)))
    (if (re-search-backward "\n --" (point-min) t)
	(forward-char 4)
      (beginning-of-line)
      (insert " --\n\n")
      (backward-char 2))
    (cond
     ((looking-at 
       "[ \n]+\\S-[^\n\t]+\\S- <[^ \t\n<>]+> +\\S-[^\t\n]+\\S-[ \t]*\n")
      t)
     ((looking-at "[ \t]*\n")
      nil)
     (t
      "finalisation line has bad format (not ` -- maintainer <email> date')"))))
;;
;;  interactive functions to add new versions (whole new sections)
;;  to changelog.
;;

(defun debian-changelog-add-version ()
  "Add a new version section to a debian-style changelog file."
  (interactive)
  (let ((f (debian-changelog-finalised-p)))
    (and (stringp f) (error f))
    (or f (< (point-max) 7) (error "previous version not yet finalised")))
  (goto-char (point-min))
  (let ((pkg-name (or (debian-changelog-suggest-package-name)
                      (read-string "Package name: ")))
        (version (or (debian-changelog-suggest-version)
                     (read-string "New version (including any revision): "))))
    (if (debian-changelog-experimental-p)
        (insert pkg-name " (" version ") experimental; urgency=low\n\n  * ")
      (insert pkg-name " (" version ") unstable; urgency=low\n\n  * "))
    (save-excursion (insert "\n\n --\n\n"))))

(defun debian-changelog-experimental-p ()
;; Peter S Galbraith, 04 May 2001
  "Return t if last upload is to experimental."
  (save-excursion
    (goto-char (point-min))
    (looking-at "\\sw.* (.+).* \\(experimental\\)")))

(defun debian-changelog-suggest-package-name ()
;; Peter S Galbraith, 23 Feb 2001
  "Return a suggested new version number to use for this changelog, or nil"
  (save-excursion
    (goto-char (point-min))
    (if (looking-at 
         "\\(\\S-+\\) +(\\([^()\n\t-]+\\)\\(-\\([^()]+\\)\\)?\\() +[^\n]*\\)")
        (match-string-no-properties 1))))

(defun debian-changelog-suggest-version ()
;; Peter S Galbraith, 23 Feb 2001
  "Return a suggested new version number to use for this changelog, or nil"
  (save-excursion
    (goto-char (point-min))
    (cond
     ((looking-at 
;;; The following is not strictly correct.  The upstream version may actually
;;; contain a hyphen if a debian version number also exists, making two hyphens
;;; I'm also assuming it begins with a digit, which is not enforced
      "\\(\\S-+\\) +(\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:]*\\)\\(-\\([0-9a-zA-Z.+]+\\)\\)?\\() +[^\n]*\\)"))

     ;; No match...
     ;; Check again for multiple hyphens, and adjust match-data if found
     ;; to leave only the bit past the last hyphen as the debian version
     ;; number.
     ((looking-at 
       "\\(\\S-+\\) +(\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:]*\\)\\(-\\([0-9a-zA-Z.+]+\\)\\)*\\() +[^\n]*\\)")
      ;; We have a hit.  Adjust match-data...
      (goto-char (match-end 5))
      (skip-chars-backward "0-9a-zA-Z.+")
      (let ((deb-vsn-beg (point))
            (ups-vsn-end (1- (point))))
        (store-match-data
         (list
          (match-beginning 0)(match-end 0)
          (match-beginning 1)(match-end 1)
          (match-beginning 2)(match-end 2)
          (match-beginning 3) ups-vsn-end
          (match-beginning 4)(match-end 4)
          deb-vsn-beg        (match-end 5)
          (match-beginning 6)(match-end 6)))))

     ;;No match... return nil and end here
     (t
      nil))
            
;;; match 1: package name
;;; match 2: epoch, if it exists
;;; match 3: upstream version number
;;; match 4: debian version number exists if matched
;;; match 5: debian version number
;;; match 6: rest of string
      (let ((pkg-name (match-string-no-properties 1))
            (epoch (or (match-string-no-properties 2) ""))
            (upstream-vsn (match-string-no-properties 3))
            (debian-vsn (match-string-no-properties 5)))
;;debug (message "name: %s  epoch: %s  version: %s  debian: %s" pkg-name epoch upstream-vsn debian-vsn))))
         
        (cond
	 ;; Debian vsn exists + Old upstream version matches current one.  
	 ;; -> Increment Debian version...
         ((and debian-vsn
               (string-match
                (regexp-quote (concat "/" pkg-name "-" upstream-vsn "/debian/changelog"))
                buffer-file-name))
          (concat epoch upstream-vsn "-" 
		  (debian-changelog-increment-version debian-vsn)))

	 ;; Same as above, but more general in case directory name doesn't 
	 ;; match package name.  -> Increment Debian version...
         ((and debian-vsn
               (string-match 
                (concat "-" (regexp-quote upstream-vsn) "/debian/changelog") 
		buffer-file-name))
          (concat epoch upstream-vsn "-"
		  (debian-changelog-increment-version debian-vsn)))

	 ;; Debian vsn exists but old upstream version doesn't match new one.
	 ;; -> Use new upstream version with "-1" debian version.
         ((and debian-vsn
               (string-match
                (concat "/" 
                        (regexp-quote pkg-name)
                        "-\\([0-9a-zA-Z.+-]+\\)/debian/changelog")
                buffer-file-name))
          (concat epoch (match-string 1 buffer-file-name) "-1"))

	 ;; Same as above, but more general in case directory name doesn't 
	 ;; match package name.
	 ;; -> Use new upstream version with "-1" debian version.
         ((and debian-vsn
               (string-match 
                (concat "-\\([0-9][0-9a-zA-Z.+-]+\\)/debian/changelog") 
		buffer-file-name))
          (concat epoch (match-string 1 buffer-file-name) "-1"))

	 ;; Debian vsn exists, but directory name has no version
	 ;; -> increment Debian vsn (no better guess)
         (debian-vsn
          (concat epoch upstream-vsn "-"
		  (debian-changelog-increment-version debian-vsn)))
	  
         ;;; No Debian version number...

	 ;; No debian version number and old upstream version doesn't 
	 ;; match new one.
         ((and (not debian-vsn) upstream-vsn pkg-name
	       (not (string-match 
                     (concat "/" (regexp-quote pkg-name) "-" 
                             (regexp-quote upstream-vsn) "/debian/changelog")
                     buffer-file-name))
	       (string-match (concat "/" (regexp-quote pkg-name) 
				     "-\\([0-9a-zA-Z.+]+\\)/debian/changelog")
			     buffer-file-name))
          (concat epoch (match-string 1 buffer-file-name)))

         ((and
	   pkg-name
	   (string-match
	    (concat "/" (regexp-quote pkg-name) 
		    "-\\([0-9a-zA-Z.+]+\\)/debian/changelog")
	    buffer-file-name))
          ;;Hmmm.. return version number from directory if we get this far
          (concat epoch (match-string 1 buffer-file-name)))
         ((string-match
           (concat "-\\([0-9][0-9a-zA-Z.+]+\\)/debian/changelog")
	   buffer-file-name)
          ;;Hmmm.. return version number from directory if we get this far
          (concat epoch (match-string 1 buffer-file-name)))
	 
	 ;; Directory name has no version -> increment what we have.
         (t
          (concat epoch 
		  (debian-changelog-increment-version upstream-vsn)))))))

(defun debian-changelog-increment-version (version)
;; Peter S Galbraith, 09 Mar 2001
  "Increment the last numeric portion of a version number
1        -> 2
0potato1 -> 0potato2
1.01     -> 1.02"
  (cond
   ((string-match "[1-9][0-9]*$" version)
    (let ((first-part (substring version 0 (match-beginning 0)))
	  (snd-part (match-string 0 version)))
      (concat 
       first-part (number-to-string (+ 1 (string-to-number snd-part))))))
   ((string-match "[0-9]*$" version)
    ;; 3.5.4.0 -> 3.5.4.1
    (let ((first-part (substring version 0 (match-beginning 0)))
	  (snd-part (match-string 0 version)))
      (concat 
       first-part (number-to-string (+ 1 (string-to-number snd-part))))))
   (t
    ;; Safety net only - first condition should catch all
    (number-to-string (+ 1 (string-to-number version))))))

(defun debian-changelog-finalise-and-save ()
  "Finalise, if necessary, and then save a debian-style changelog file."
  (interactive)
  (let ((f (debian-changelog-finalised-p)))
    (and (stringp f) (error f))
    (or f (debian-changelog-finalise-last-version)))
  (save-buffer))

;;
;; internal function to get date as string (used by finalising routines)
;;

(defun debian-changelog-date-string ()
  "Return RFC-822 format date string"
  (let* ((dp "822-date")
	 (cp (point))
	 (ret (call-process "822-date" nil t))
	 (np (point))
	 (out nil))
    (cond ((not (or (eq ret nil) (eq ret 0)))
	   (setq out (buffer-substring-no-properties cp np))
	   (delete-region cp np)
	   (error (concat "error from " dp ": " out)))
	  (t
	   (backward-char)
	   (or (looking-at "\n")
	       (error (concat "error from " dp ": expected newline after date string")))
	   (setq out (buffer-substring-no-properties cp (- np 1)))
	   (delete-region cp np)
	   out))))

;;
;; interactive functions to finalize entry
;;

;;; Use debian-changelog-full-name and debian-changelog-mailing-address instead
;; (make-local-variable 'add-log-full-name)
;; (make-local-variable 'add-log-mailing-address)

(defun debian-changelog-finalise-last-version ()
  "Finalise maintainer's name and email and release date."
  (interactive)
  (and (debian-changelog-finalised-p)
       (debian-changelog-unfinalise-last-version))
  (if debian-changelog-local-variables-maybe-remove
      (debian-changelog-local-variables-maybe-remove))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\n --\\([ \t]*\\)")
    (delete-region (match-beginning 1) (match-end 1))
    (insert " " debian-changelog-full-name 
            " <" debian-changelog-mailing-address ">  " 
	    (debian-changelog-date-string))))

;;
;; interactive function to unfinalise changelog (so modifications can be made)
;;

(defun debian-changelog-unfinalise-last-version ()
  "Remove the `finalisation' information (maintainer's name and email
address and release date) so that new entries can be made."
  (interactive)
  (if (debian-changelog-finalised-p) nil
    (error "most recent version is not finalised"))
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\n --")
    (let ((dels (point)))
      (end-of-line)
      (delete-region dels (point)))))

;;
;; top level interactive function to activate mode
;;

(defun debian-changelog-mode ()
  "Major mode for editing Debian-style change logs.
Runs `debian-changelog-mode-hook' if it exists.

Key bindings:

\\{debian-changelog-mode-map}

If you want to use your debian.org email address for debian/changelog
entries without using it for the rest of your email, use the `customize`
interface to set it, or simply set the variable
debian-changelog-mailing-address in your ~/.emacs file, e.g.

 (setq debian-changelog-mailing-address \"myname@debian.org\"))
"

;;If neither add-log-mailing-address nor user-mail-address are
;;explicitly set by the user, the environment variables DEBEMAIL and
;;EMAIL are used.  DEBEMAIL takes precedence over EMAIL.
  (interactive)
  (kill-all-local-variables)
  (text-mode)
  (setq major-mode 'debian-changelog-mode
	mode-name "Debian changelog"
        left-margin 2
	fill-prefix "  "
	fill-column 74)
  (use-local-map debian-changelog-mode-map)
  ;; Let each entry behave as one paragraph:
; (set (make-local-variable 'paragraph-start) "\\*")
; (set (make-local-variable 'paragraph-separate) "\\*\\|\\s-*$|\\S-")
  ;; PSG: The following appears to get fill-paragraph to finally work!
  (set (make-local-variable 'paragraph-start) "\\*\\|\\s *$\\|\f\\|^\\<")
  (set (make-local-variable 'paragraph-separate) "\\s *$\\|\f\\|^\\<")
  ;; Let each version behave as one page.
  ;; Match null string on the heading line so that the heading line
  ;; is grouped with what follows.
  (set (make-local-variable 'page-delimiter) "^\\<")
  (set (make-local-variable 'version-control) 'never)
  (set (make-local-variable 'adaptive-fill-regexp) "\\s *")
  (set (make-local-variable 'font-lock-defaults) 
       '((debian-changelog-font-lock-keywords
          debian-changelog-font-lock-keywords-1
          debian-changelog-font-lock-keywords-2) t t))
  (set (make-local-variable
	'debian-changelog-local-variables-maybe-remove-done) nil)
  (debian-changelog-bug-menu-init)
  (easy-menu-add debian-changelog-menu)
  (cond
   (debian-changelog-use-imenu
    (require 'imenu)
    (setq imenu-create-index-function 'imenu--create-debian-changelog-index)
    (if (or window-system
            (fboundp 'tmm-menubar))
        (progn
          (imenu-add-to-menubar "History")
          ;(imenu-update-menubar)
          ))))
  (cond
   (debian-changelog-highlight-mouse-t
    (debian-changelog-setup-highlight-mouse-keymap)
    (debian-changelog-highlight-mouse)))
  (run-hooks 'debian-changelog-mode-hook))
;;(easy-menu-add debian-changelog-menu))

;;
;; font-lock face defs by Peter Galbraith

(defvar debian-changelog-warning-face 'debian-changelog-warning-face
  "Face to use for important keywords.")

(cond
 ((and (fboundp 'facep)
       (facep 'font-lock-warning-face))
  (copy-face 'font-lock-warning-face 'debian-changelog-warning-face))
 ((fboundp 'defface)
  (defface debian-changelog-warning-face
    '((((class grayscale)(background light))(:foreground "DimGray" :bold t))
      (((class grayscale)(background dark))(:foreground "LightGray" :bold t))
      (((class color)(background light))(:foreground "red" :bold t ))
      (((class color)(background dark))(:foreground "red" :bold t ))
      (t (:bold t)))
    "Face for debian-changelog important strings."
    :group 'debian-changelog-faces))
 (t
  ;;; XEmacs19:
  (make-face 'font-latex-warning-face "Face to use for LaTeX major keywords.")
  (make-face-bold 'font-latex-warning-face)
  ;; XEmacs uses a tag-list thingy to determine if we are using color
  ;;  or mono (and I assume a dark background).
  (set-face-foreground 'font-latex-warning-face "red" 'global nil 'append)))

;;
;; font-lock definition by Chris Waters, 
;;           revisited by Peter Galbraith (Apr 2001)

;; Available faces:
;; keyword-face, type-face, string-face, comment-face, 
;; variable-name-face, function-name-face
;; in emacs only:  builtin-face, constant-face, warning-face
;; in xemacs only: reference-face, doc-string-face, preprocessor-face

;; the mappings I've done below only use faces available in both emacsen.
;; this is somewhat limiting; I may consider adding my own faces later.

(defvar debian-changelog-font-lock-keywords-1
  (list
    ;; package name line: pkg (1.0-1) unstable; urgency=low
   '(debian-changelog-fontify-version
     (1 font-lock-function-name-face)
     (2 font-lock-type-face nil t)
     (3 font-lock-string-face nil t)
     (4 debian-changelog-warning-face nil t))
   '(debian-changelog-fontify-stable . debian-changelog-warning-face)
   '(debian-changelog-fontify-frozen . font-lock-type-face)
   '(debian-changelog-fontify-unstable . font-lock-string-face)
   '(debian-changelog-fontify-experimental . debian-changelog-warning-face)
   '(debian-changelog-fontify-urgency-crit . debian-changelog-warning-face)
   '(debian-changelog-fontify-urgency-high . debian-changelog-warning-face) 
   '(debian-changelog-fontify-urgency-med . font-lock-type-face)
   '(debian-changelog-fontify-urgency-low . font-lock-string-face)
   ;; bug closers
   '("\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
     (1 font-lock-keyword-face)
     (2 debian-changelog-warning-face))
   ;; maintainer line (enforce 2 space exactly between email and date)
   '("^ -- \\(.+\\) <\\(.+@.+\\)>  \\([^ ].+\\)$"
     (1 font-lock-variable-name-face)
     (2 font-lock-variable-name-face)
     (3 font-lock-string-face)))
  "first level highlighting for debian-changelog-mode.")

(defvar debian-changelog-font-lock-keywords-2
  (append 
   debian-changelog-font-lock-keywords-1
   ;; bullet lines
   '(("^  +\\(\\*\\)" 1 font-lock-comment-face)))
  "high level highlighting for debian-changelog-mode.")

(defvar debian-changelog-font-lock-keywords 
  debian-changelog-font-lock-keywords-1
  "Default expressions to highlight in debian-changelog-mode.")

;; Fontifier function by Peter Galbraith, Apr 24 2001

(defun debian-changelog-fontify-version (limit)
  "return matches for package name and version number
match 1 -> package name
      2 -> native vsn number 
      3 -> non-native vsn number
      4 -> non-native NMU vns number" 
  (when (re-search-forward 
;;; The following is not strictly correct.  The upstream version may actually
;;; contain a hyphen if a debian version number also exists, making two hyphens
;;; I'm assuming it begins with a digit, which is not enforced
         "^\\(\\S-+\\) +(\\([0-9]:\\)?\\([0-9][0-9a-zA-Z.+:]*\\)\\(-\\([0-9a-zA-Z.+]+\\)\\)*)" nil t)
;;                                                                                         ^
;; Note the asterix above, allowing more than one hyphen in the version
;; number, but wrongly assuming that all of it is the Debian version
;; instead of only the bit past the last hyphen.  I might get NMUs wrongly
;; for version numbers with multiple hyphens.

;; match 1: package name
;; match 2: epoch, if it exists
;; match 3: upstream version number
;; match 4: debian version number exists if matched
;; match 5: debian version number
    (cond
     ((not (match-string 4))
      ;; No Debian version number -> Debian native package
      (store-match-data 
       (list (match-beginning 1)(match-end 3)
             (match-beginning 1)(match-end 1)
             (match-beginning 3)(match-end 3)
             nil nil
             nil nil)))
     ((match-string 4)
      ;; Debian version number -> Let's see if NMU...
      (let* ((deb-vsn (match-string 5))
             (is-NMU (save-match-data (string-match "\\." deb-vsn))))
        (cond
         (is-NMU
          (store-match-data 
           (list (match-beginning 1)(match-end 5)
                 (match-beginning 1)(match-end 1)
                 nil nil
                 nil nil
                 (match-beginning 3)(match-end 5))))
         (t
          (store-match-data 
           (list (match-beginning 1)(match-end 5)
                 (match-beginning 1)(match-end 1)
                 nil nil
                 (match-beginning 3)(match-end 5)
                 nil nil)))))))
    t))

(defun debian-changelog-fontify-urgency-crit (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=critical\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-high (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=high\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-med (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=medium\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-urgency-low (limit)
  (when (re-search-forward "^\\sw.* (.+).*; \\(urgency=low\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-stable (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(stable\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-frozen (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(frozen\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-unstable (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(unstable\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

(defun debian-changelog-fontify-experimental (limit)
  (when (re-search-forward "^\\sw.* (.+).* \\(experimental\\)" limit t)
    (store-match-data 
     (list (match-beginning 1)(match-end 1)))
    t))

;;
;; browse-url interfaces, by Peter Galbraith, Feb 23 2001
;;

(defun debian-changelog-web-bugs ()
  "Browse the BTS for this package via browse-url"
  (interactive)
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or (debian-changelog-suggest-package-name)
                       (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up.")
      (browse-url (concat "http://bugs.debian.org/cgi-bin/pkgreport.cgi?src="
                          pkg-name))
      (message "Looking up bugs for source package %s via browse-url" 
               pkg-name))))

(defun debian-changelog-web-bug (&optional bug-number)
  "Browse the BTS for a bug report number via browse-url"
  (interactive (list (completing-read "Bug number to lookup: " 
                                      debian-changelog-bug-alist nil nil)))
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (if (or (not bug-number) (string-equal bug-number "none"))
      (setq bug-number (completing-read "Bug number to loopkup: " 
                                        debian-changelog-bug-alist nil nil)))
  (if (string-equal bug-number "")
      (message "No bug number to look up.")
    (browse-url
     (concat "http://bugs.debian.org/cgi-bin/bugreport.cgi?archive=yes&bug="
             bug-number))
    (message "Looking up bug number %s via browse-url" bug-number)))

(defun debian-changelog-web-this-bug ()
  "Browse the BTS via browse-url for the bug report number under point."
  (interactive)
  (if (not (looking-at "[0-9]"))
      (error "Not a number under point/mouse"))
  (save-excursion
    (skip-chars-backward "0123456789")
    (if (looking-at "[0-9]+")
        (let ((bug-number (match-string 0)))
          (debian-changelog-web-bug bug-number)))))

(defun debian-changelog-web-this-bug-under-mouse (EVENT)
  "Browse the BTS via browse-url for the bug report number under mouse."
  (interactive "e")
  (mouse-set-point EVENT)
  (debian-changelog-web-this-bug))

(defvar debian-changelog-is-XEmacs
  (not (null (save-match-data (string-match "XEmacs\\|Lucid" emacs-version)))))

(defun debian-changelog-setup-highlight-mouse-keymap ()
  (setq debian-changelog-mouse-keymap
   ;;; First, copy the local keymap so we don't have `disappearing' menus
   ;;; when the mouse is moved over a bug number.
        
   ;;; FIXME: Check out (mouse-major-mode-menu) to see how it grabs the local
   ;;;        menus to display.
        (let ((m (copy-keymap (current-local-map))))
;;          (cond
;;           ((and debian-changelog-use-imenu
;;                 (or window-system (fboundp 'tmm-menubar)))
;;            (imenu-add-to-menubar "History")))
          (cond
           (debian-changelog-is-XEmacs
            (set-keymap-name m 'debian-changelog-mouse-keymap)
            (define-key m [button3]
              'debian-changelog-web-this-bug-under-mouse))
           (t
            (define-key m [down-mouse-3]
              'debian-changelog-web-this-bug-under-mouse)))
          m)))

(defvar debian-changelog-ext-list nil 
  "xemacs buffer-local list of debian-changelog-cite extents.")
(make-variable-buffer-local 'debian-changelog-ext-list)
(put 'debian-changelog-ext-list 'permanent-local t)

(defun debian-changelog-highlight-mouse ()
  "Make that nice green highlight when the mouse a bug number, and set kaymap"
  (interactive)
  (save-excursion
    (let ((s)(e)(extent)(local-extent-list debian-changelog-ext-list)
          (inhibit-read-only t)
          (modified (buffer-modified-p))) ;put-text-property changing this?
      ;; Remove the mouse face properties first.
      (setq debian-changelog-ext-list nil)		;Reconstructed below...
      (if (string-match "XEmacs\\|Lucid" emacs-version)
          (while local-extent-list
	    (setq extent (car local-extent-list))
 	    (if (or (extent-detached-p extent)
 		    (and (<= (point-min)(extent-start-position extent))
 			 (>= (point-max)(extent-end-position extent))))
		(delete-extent extent)
	      (setq debian-changelog-ext-list 
                    (cons extent debian-changelog-ext-list)))
            (setq local-extent-list (cdr local-extent-list)))
        ;; Remove properties for regular emacs
        ;; FIXME This detroys all mouse-faces and local-maps!
        (let ((before-change-functions) (after-change-functions))
          (remove-text-properties (point-min) (point-max)
                                  '(mouse-face t local-map t))))
      (goto-char (point-min))
      ;; FIXME: Ideally, I want to hightlight _only_ the digit parts 
      ;; (skipping the coma, and the word "bug".
      (while 
          (re-search-forward 
           "\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
           nil t)
        (setq s (match-beginning 2))
        (setq e (match-end 2))
        (cond 
         ((string-match "XEmacs\\|Lucid" emacs-version)
          (setq extent (make-extent s e))
          (setq debian-changelog-ext-list 
                (cons extent debian-changelog-ext-list))
          (set-extent-property extent 'highlight t)
          (set-extent-property extent 'start-open t)
;	  (set-extent-property extent 'balloon-help 'debian-changelog-label-help)
;	  (set-extent-property extent 'help-echo 'debian-changelog-label-help-echo)
          (set-extent-property extent 'keymap debian-changelog-mouse-keymap))
         (t
          (let ((before-change-functions) (after-change-functions))
            (put-text-property s e 'local-map 
                               debian-changelog-mouse-keymap)
            (put-text-property s e 'mouse-face 'highlight)))))
      (set-buffer-modified-p modified))))

(defun debian-changelog-web-packages ()
  "Search Debian web page for this package via browse-url"
  (interactive)
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or (debian-changelog-suggest-package-name)
                       (read-string "Package name: "))))
    (if (string-equal "" pkg-name)
        (message "No package name to look up.")
      (browse-url 
       (concat 
        "http://packages.debian.org/cgi-bin/search_packages.pl?keywords="
        pkg-name
        "&searchon=names&version=all&release=all"))
      (message "Looking up web pages for package %s via browse-url" 
               pkg-name))))

(defvar debian-changelog-archive-alist
  '(("stable") ("testing") ("unstable"))
  "alist of valid Debian archives for web interface (excludes experimental).")

(defvar debian-changelog-archive-list
  '("stable" "testing" "unstable")
  "list of valid Debian archives.")

;; To do: handle multi-binary packages (to get the name!)
(defun debian-changelog-web-package (archive)
  "Search Debian web page in ARCHIVE for this package via browse-url"
  (interactive "P")
  (if (not (featurep 'browse-url))
      (progn
        (load "browse-url" nil t)
        (if (not (featurep 'browse-url))
            (error "This function requires the browse-url elisp package"))))
  (let ((pkg-name (or (debian-changelog-suggest-package-name)
                       (read-string "Package name: ")))
        (archiv))
    (if (string-equal "" pkg-name)
        (message "No package name to look up.")
      (if (not (member (list archive) debian-changelog-archive-alist))
          (setq archive
                (completing-read "Debian archive: "
                                 debian-changelog-archive-alist nil t nil)))
      (if (string-equal "" archive)
          (message "No archive name to look up.")
        (browse-url 
         (concat 
          "http://packages.debian.org/cgi-bin/search_packages.pl?keywords="
          pkg-name
          "&searchon=names&release=all&version=" archive))
        (message "Looking up %s web page for package %s via browse-url"
                 archive pkg-name)))))

;;;-------------
;;; wget bug from BTS stuff - Peter Galbraith, August 2001

(defvar debian-changelog-bug-alist nil
  "Buffer local alist of bug numbers (and description) for this package")
(make-variable-buffer-local 'debian-changelog-bug-alist)
;; -> Make close-bug use it if it exists

(defun debian-changelog-build-bug-menu (package)
  "Build a menu listing the bugs for this package"
  (setq debian-changelog-bug-alist nil)
  (let ((debian-changelog-tmp-buffer 
         (get-buffer-create "*debian-changelog-tmp-buffer*"))
        (bug-alist))
    (save-excursion
      (set-buffer debian-changelog-tmp-buffer)
      (insert "(setq debian-changelog-bug-menu-var\n'(\"Bugs\"\n")
      (insert "[\"* Regenerate list *\" (debian-changelog-build-bug-this-menu) t]\n\"--\"\n")
      (with-temp-buffer
	(call-process "wget" nil '(t t) nil "--quiet" "-O" "-" 
		      (concat 
                       "http://bugs.debian.org/cgi-bin/pkgreport.cgi?src="
                       package))
	(goto-char (point-min))
	(while 
	    (re-search-forward
	     "\\(<H2>\\(.+\\)</H2>\\)\\|\\(<li><a href=\"\\(http://bugs.debian.org/cgi-bin/bugreport.cgi\\?bug=\\([0-9]+\\).+\\)\">#\\(.+\\)</a>\\)"
	     nil t)
	  (let ((type (match-string 2))
		(URL (match-string 4))
		(bugnumber (match-string 5))
		(description (match-string 6)))
	    (cond
	     (type
	      (save-excursion
		(set-buffer debian-changelog-tmp-buffer)
		(insert "\"" type "\"\n")))
	     (t
	      (setq bug-alist (cons (list bugnumber description) bug-alist))
	      (save-excursion
		(set-buffer debian-changelog-tmp-buffer)
		(insert 
		 "[\"" description "\""
		 " (debian-changelog-web-bug \"" bugnumber "\") t]\n")))))))
      (set-buffer debian-changelog-tmp-buffer) ;Make sure we're here
      (insert "))")
      (eval-buffer)
      (kill-buffer nil))
    (setq debian-changelog-bug-alist bug-alist)
    ;; The following needs to be changed for debian-bug.el
    (easy-menu-define 
      debian-changelog-bugs-menu 
      debian-changelog-mode-map "Debian Changelog Mode Bugs Menu"
      debian-changelog-bug-menu-var)))

(defun debian-changelog-build-bug-this-menu ()
  "Regenerate Bugs list menu for this buffer's package"
  (let ((package (or (and (fboundp 'debian-changelog-suggest-package-name)
			  (debian-changelog-suggest-package-name))
		     (and (boundp 'debian-bug-package-name)
			  debian-bug-package-name)
		     (read-string "Package name: "))))
    (debian-changelog-build-bug-menu package)))


(defun debian-changelog-bug-menu-init ()
  "Initialize menu -- before it gets filled-in on request.
Add this function to the calling major-mode function."
  (easy-menu-define 
    debian-changelog-bugs-menu debian-changelog-mode-map 
    "Debian Changelog Mode Bugs Menu"
    '("Bugs"
      ["* Generate menu *" (debian-changelog-build-bug-this-menu) 
       (zerop (call-process "which" nil nil nil "wget"))]))
  (easy-menu-add debian-changelog-bugs-menu))

;;;-------------
;;; imenu stuff - Peter Galbraith, May 2001

(eval-when-compile
  (require 'cl))
(if (fboundp 'imenu)                    ;Make sure auto-load is loaded
    (eval-when-compile
      (require 'imenu)))

(defvar debian-changelog-imenu-doing-closebug nil 
  "Internal flag set when imenu is processing many bug closings")
(make-variable-buffer-local 'debian-changelog-imenu-doing-closebug)

(defun debian-changelog-imenu-prev-index-position-function ()
  (cond
   (debian-changelog-imenu-doing-closebug
    (if (not (posix-search-backward 
              "\\(closes:\\)\\|[^0-9]\\([0-9]+\\)" nil t))
        nil                             ; No match
      ;; match 1 -> "closes:"
      ;; match 2 -> a bug number
      (cond 
       ((match-string 1)
        (setq debian-changelog-imenu-doing-closebug nil)
        (debian-changelog-imenu-prev-index-position-function))
       (t
        ;; Return the bug number match
        t))))
   (t
    (if (not (re-search-backward 
              "\\(closes: *\\(bug\\)?#? *[0-9]+\\)\\|\\(^\\sw.* (\\(.+\\))\\)"
              nil t))
        nil                             ; No match
      ;; match 1 -> "closes:"
      ;; match 4 -> a version number
      (cond
       ((match-string 1)
        (setq debian-changelog-imenu-doing-closebug t)
        (forward-char -1)
        (re-search-forward
         "\\(closes:\\) *\\(\\(bug\\)?#? *[0-9]+\\(, *\\(bug\\)?#? *[0-9]+\\)*\\)"
         nil t)
        (forward-char 1)
        (debian-changelog-imenu-prev-index-position-function))
       (t
        ;; Return the version number match
        t))))))

(defvar debian-changelog-imenu-counter nil 
  "debian-changelog-mode internal variable for imenu support")

(defun imenu--create-debian-changelog-index ()
    (save-match-data
      (save-excursion
        (let ((index-alist '())
              (index-bug-alist '())
              (index-bugsorted-alist '())
              (prev-pos 0)
              (imenu-scanning-message "Scanning changelog for History (%3d%%)")
              )
          (setq debian-changelog-imenu-counter -99)
          (goto-char (point-max))
          (imenu-progress-message prev-pos 0 t)
;;;          (message "Scanning changelog history...")
          (setq debian-changelog-imenu-doing-closebug nil)
          (while (debian-changelog-imenu-prev-index-position-function)
            (imenu-progress-message prev-pos nil t)
            (let ((marker (make-marker)))
              (set-marker marker (point))
              (cond
               ((match-beginning 2)     ;bug number
                (push (cons (match-string-no-properties 2) marker)
                      index-bug-alist))
               ((match-beginning 4)     ;version number
                (push (cons (match-string-no-properties 4) marker)
                      index-alist)))))
          (imenu-progress-message prev-pos 100 t)
;;;       (message "Scanning changelog history... done.")
          (cond
           (index-bug-alist
            (push (cons "Closed Bugs (chrono)"
                        index-bug-alist) 
                  index-alist)
            (setq index-bugsorted-alist (copy-alist index-bug-alist))
            (push (cons "Closed Bugs (sorted)"
                        (sort index-bugsorted-alist 
                              'debian-changelog-imenu-sort))
                  index-alist)))
          index-alist))))

(defun debian-changelog-imenu-sort (el1 el2)
  "Predicate to compare labels in lists."
  (string< (car el2) (car el1) ))

;;; end of imenu stuff
;;;-------------

;;; Setup auto-mode-alist 
;; (in case /etc/emacs/site-start.d/50dpkg-dev.el not used)

(if (not (assoc '"/debian/changelog\\'" auto-mode-alist))
    (setq auto-mode-alist
          (cons '("/debian/changelog\\'" . debian-changelog-mode)
                auto-mode-alist)))
(if (not (assoc '"changelog.Debian" auto-mode-alist))
    (setq auto-mode-alist
          (cons '("changelog.Debian" . debian-changelog-mode)
                auto-mode-alist)))
(if (not (assoc '"changelog.Debian.gz" auto-mode-alist))
    (setq auto-mode-alist
          (cons '("changelog.Debian.gz" . debian-changelog-mode)
                auto-mode-alist)))

(provide 'debian-changelog-mode)
