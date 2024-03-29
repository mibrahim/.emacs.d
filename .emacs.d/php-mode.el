;;; php-mode.el --- major mode for editing PHP code

;; Copyright (C) 1999-2001 Turadg Aleahmad

;; Maintainer: Turadg Aleahmad <turadg at users.sourceforge.net>
;; Keywords: php languages oop
;; Created: 1999-05-17
;; Modified: 2002-01-22
;; X-URL:   http://php-mode.sf.net/

(defconst php-version "1.0.2"
  "PHP Mode version number.")

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Usage

;; Place this file in your Emacs lisp path (eg. site-lisp)
;; and add to your .emacs file:
;;   (load-library "php-mode-102")

;; If you want colorization, turn on global-font-lock or
;; add this to your .emacs:
;;   (add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; To make php-mode compatible with html-mode, see php-mode.sf.net

;; Many options available under Help:Customize
;; Options specific to php-mode are in
;;  Programming/Languages/Php
;; Since it inherits much functionality from c-mode, look there too
;;  Programming/Languages/C

;;; Commentary:

;; PHP mode is a major mode for editing PHP 3 and 4 source code.  It's
;; an extension of C mode; thus it inherits all C mode's navigation
;; functionality.  But it colors according to the PHP grammar and indents
;; according to the PEAR coding guidelines.  It also includes a couple
;; handy IDE-type features such as documentation search and a source
;; and class browser.


;;; Changelog:

;; 1.02
;;   Highlight HTML tags and entities (see site for more on HTML)
;;   Highlight more constants by default
;;   Fixed problem with $-line indenting inside switch (see comments)
;;   Fix to show keymap in help mode (Ryan Sammartino)
;;   Added .phps to default extensions to match (John Keller)

;; 1.0.1
;;   Fix for some speedbar problems

;; 1.0.0
;;   Function indentation fix (Craig Andrews)
;;   Added option to open speedbar on buffer load
;;   Fixed PEAR filename matching regexps and bug with mmm-mode
;;   Added preference option to always use PEAR standards
;;   Fixed PEAR hook bug with XEmacs
;;   Took out warning for empty parens in "new Object()"
;;   Fixed keyword highlighting of 'this' at start of variable
;;   Removed "goto" references
;;   Added 'next' as keyword

;; 0.9.9
;;   Fixed highlights for targets of gotos and 'default' within a switch
;;   Changed my e-mail and URL to SourceForge
;;   Changed name of hook variable (update your .emacs)
;;   Wrapped PEAR standards into a PEAR hook
;;   Disabled coloring non-core constants (see comments to re-enable)

;; 0.9.8
;;   Thanks to a patch from Stig Saether Bakken:
;;   Checks for PEAR before setting indent-tabs-mode to nil
;;   Adds some keywords that got lost
;;   Highlights function $foo and class::method
;;   Removes C++ template from speedbar search expression :)
;;   Adds about a thousand constants
;;     So many that if you want to recompile php-mode-098.el
;;     you'll have to greatly increase max-specpdl-size

;; 0.9.7
;;      This version marks a huge advance and will become 1.0.0
;;    after I wait a bit for possible bug reports.
;;      
;;      The primary improvement is support for shell-style comments
;;    in both GNU Emacs and XEmacs.  Thank for this goes to Fred
;;    Yankowski <fred@ontosys.com>.
;; 
;;    Customization of mode hook (Roland Rosenfeld)
;;    Fontifies object references much more intelligently
;;    Added .inc to filename and Speedbar patterns
;;    PEAR coding standards (tabs -> 4 spaces)
;;    Fontify ASP-style tag (Fred)
;;    Rudimentary coloration of HTML tags (Fred)
;;    Buges fixed:
;;      Extraneous newline at end of file (Fred)
;;      Colored functions within comments
;;      Didn't recognize keywords in caps
;;      Colored functions as variables when referenced by an object

;; 0.9.6 2001-01-28
;;    Keyboard shortcuts for menu functions (Boris Folgmann)
;;    Fixed default regexp for detecting PHP files (Mathias Meyer)
;;    Changed PHP3 menu to simply PHP

;; 0.9.5 2001-01-14
;;    Been told that problems with XEmacs are no longer
;;    Search documention command actually works
;;    Browse manual customization improved

;; 0.9.4 2001-01-08
;;    Search documentation command
;;    Browse manual function
;;    Simplified file patterns for which to load php-mode
;;    PHP awareness in Speedbar
;;    Customization options for all of the above

;; 0.9.3 2000-11-12
;;    imenu support for classes and functions (Rex McMaster)
;;    Dramatically improved regexps (Kevin Blake)
;;    Fix for XEmacs font-lock-pre-idle-hook problem? (Doug Marcey)
;;    Progress on PHP3 menu functions (Sean Champ)
;;    Added "foreach" to list of keywords (Sean Champ)
;;    More file suffixes observed (Vinai Kopp)

;; 0.9.2 2000-03-08
;;    Fixed bug with 1-character identifiers
;;    Fixed bug with class declaration coloring
;;    Added coloring for true, false, null
;;    Officially not supporting XEmacs

;; 0.9.1 2000-02-21
;;    Disabled keywords in XEmacs for compatibility
;;    Added usage info to comments

;; 0.9 2000-01-09
;;    Clarified bug with XEmacs (Juanjo)
;;    Fixed minor bug with comment highlighting (Juanjo)
;;    Syntax parsing from PHP3 lexical scanner (Torsten Martinsen)
;;    Highlights function and method names now
;;    Highlights "this" as keyword when used as an object in variable names

;; 0.8 1999-05-17
;;	Initial release.


;;; Code:

(require 'speedbar)
(require 'font-lock)
(require 'regexp-opt)
(require 'cc-mode)
(require 'custom)

;; Local variables
(defgroup php nil
  "Major mode for editing PHP code."
  :prefix "php-"
  :group 'languages)

(defcustom php-speedbar-config t
  "*When set to true automatically configures Speedbar to observe PHP files.\
Ignores php-file patterns option; fixed to expression \"\\.\\(inc\\|php[s34]?\\)\""
  :type 'boolean
  :group 'php)

(defcustom php-mode-speedbar-open nil
  "Normally php-mode starts with the speedbar closed.\
Turning this on will open it whenever php-mode is loaded."
  :type 'boolean
  :group 'php)

(defcustom php-manual-url "http://www.php.net/manual/en/manual.php"
  "*URL at which to find PHP manual.\
You can replace \"en\" with your ISO language code."
  :type 'string
  :group 'php)

(defcustom php-search-url "http://www.php.net/"
  "*URL at which to search for documentation on a word"
  :type 'string
  :group 'php)

(defcustom php-file-patterns (list "\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'")
  "*List of file patterns for which to automatically invoke php-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'php)

(defcustom php-mode-user-hook nil
  "List of functions to be executed on entry to php-mode"
  :type 'hook
  :group 'php)

(defcustom php-mode-force-pear nil
  "Normally PEAR coding rules are enforced only when the filename contains \"PEAR\"\
Turning this on will force PEAR rules on all PHP files."
  :type 'boolean
  :group 'php)

;; Note whether we're in XEmacs
(defconst xemacsp (string-match "Lucid\\|XEmacs" emacs-version)
  "Non nil if using XEmacs.")

;;;###autoload
(define-derived-mode php-mode c-mode "PHP"
  "Major mode for editing PHP code.\n\n\\{php-mode-map}"
  
  (setq comment-start "// "
	comment-end   ""
	comment-start-skip "// *")
  
  (defvar php-mode-syntax-table php-mode-syntax-table)
  ;; underscore considered part of word
  (modify-syntax-entry ?_ "w" php-mode-syntax-table)
  ;; dollar-sign considered punctuation, not part of word
  ;; 2002-01-22: I don't remember saying the above so I'm commenting
  ;; this out because it kills indenting of case lines that begin with $
  ;; Let me know if you have a case for keeping it in, preferably
  ;; along with a solution to the switch indenting problem
;  (modify-syntax-entry ?$ "." php-mode-syntax-table)
  
  ;; The above causes Xemacs to handle shell-style comments correctly,
  ;; but fails to work in GNU Emacs which fails to interpret \n as the
  ;; end of the comment.
  (if xemacsp (progn
		(modify-syntax-entry ?# "< b" php-mode-syntax-table)
		(modify-syntax-entry ?\n "> b" php-mode-syntax-table)
		))
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
 	'((php-font-lock-keywords-1
 	   php-font-lock-keywords-2
 	   ;; Comment-out the next line if the font-coloring is too
 	   ;; extreme/ugly for you.
 	   php-font-lock-keywords-3
 	   )
 	  nil				; KEYWORDS-ONLY
 	  T				; CASE-FOLD
 	  nil				; SYNTAX-ALIST
 	  nil				; SYNTAX-BEGIN
 	  (font-lock-syntactic-keywords . php-font-lock-syntactic-keywords)))
  
  (setq font-lock-maximum-decoration t
	case-fold-search t		; PHP vars are case-sensitive
	imenu-generic-expression cc-imenu-php-generic-expression)
  
  ;; Do not force newline at end of file.  Such newlines can cause
  ;; trouble if the PHP file is included in another file before calls
  ;; to header() or cookie().
  (set (make-local-variable 'require-final-newline) nil)
  (set (make-local-variable 'next-line-add-newlines) nil)
  
  ;; PEAR coding standards
  (make-local-hook 'php-mode-pear-hook)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (set (make-local-variable 'tab-width) 4)) nil t)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (set (make-local-variable 'c-basic-offset) 4)) nil t)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (set (make-local-variable 'c-hanging-comment-ender-p) nil)) nil t)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (set (make-local-variable 'indent-tabs-mode) nil)) nil t)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (c-set-offset 'block-open' - )) nil t)
  (add-hook 'php-mode-pear-hook
	    (lambda nil (c-set-offset 'block-close' 0 )) nil t)

  (if (or php-mode-force-pear
	  (and (stringp buffer-file-name)
	       (string-match "PEAR\\|pear"
			     (buffer-file-name))
	       (string-match "\\.php$" (buffer-file-name))))
      (run-hooks 'php-mode-pear-hook))
  
  (run-hooks 'php-mode-user-hook)
  )

;; Make php-mode the default mode for PHP source code buffers.
;;;###autoload
(let ((php-file-patterns-temp php-file-patterns))
  (while php-file-patterns-temp
    (add-to-list 'auto-mode-alist
		  (cons (car php-file-patterns-temp) 'php-mode))
    (setq php-file-patterns-temp (cdr php-file-patterns-temp))))

;; Handle Speedbar
(if php-mode-speedbar-open
    (speedbar 1))
(if (and php-speedbar-config (symbolp 'speedbar))
    (speedbar-add-supported-extension "\\.\\(inc\\|php[s34]?\\)"))

;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
(define-key php-mode-map [menu-bar] (make-sparse-keymap))
(define-key php-mode-map [menu-bar php]
  (cons "PHP" (make-sparse-keymap "PHP")))

;; Define specific subcommands in this menu.
(define-key php-mode-map [menu-bar php complete-function]
  '("Complete function name" . php-complete-function))
(define-key php-mode-map
  [menu-bar php browse-manual]
  '("Browse manual" . php-browse-manual))
(define-key php-mode-map
  [menu-bar php search-documentation]
  '("Search documentation" . php-search-documentation))

;; Define function name completion function
(defun php-complete-function ()
  "Complete the function name at the point from known PHP functions."
  (interactive)
  (message "php-complete-function not implemented yet")
  ;; how to read the list of functions from a separate file?
  )

;; Define function documentation function
(defun php-search-documentation ()
  "Search PHP documentation for the word at the point."
  (interactive)
  (browse-url (concat php-search-url (current-word t)))
  )

;; Define function for browsing manual
(defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (browse-url php-manual-url)
  )

;; Define shortcut
(define-key php-mode-map
  "\C-c\C-f"
  'php-search-documentation)

;; Define shortcut
(define-key php-mode-map
  "\C-c\C-m"
  'php-browse-manual)

;; Define abbreviations and their expansions
(define-abbrev php-mode-abbrev-table "ex" "extends")
(define-abbrev php-mode-abbrev-table "fu" "function")
(define-abbrev php-mode-abbrev-table "GL" "GLOBAL")
(define-abbrev php-mode-abbrev-table "req" "require(")
(define-abbrev php-mode-abbrev-table "ret" "return")

(defconst php-constants
  (eval-when-compile
    (regexp-opt
     '(;; core constants
       "__LINE__" "__FILE__"
       "PHP_OS" "PHP_VERSION" 
       "TRUE" "FALSE" "NULL"
       "E_ERROR" "E_NOTICE" "E_PARSE" "E_WARNING" "E_ALL"
       "E_USER_ERROR" "E_USER_WARNING" "E_USER_NOTICE"
       "DEFAULT_INCLUDE_PATH" "PEAR_INSTALL_DIR" "PEAR_EXTENSION_DIR"
       "PHP_BINDIR" "PHP_LIBDIR" "PHP_DATADIR" "PHP_SYSCONFDIR"
       "PHP_LOCALSTATEDIR" "PHP_CONFIG_FILE_PATH"

       ;; from ext/standard:
       "EXTR_OVERWRITE" "EXTR_SKIP" "EXTR_PREFIX_SAME"
       "EXTR_PREFIX_ALL" "EXTR_PREFIX_INVALID" "SORT_ASC" "SORT_DESC"
       "SORT_REGULAR" "SORT_NUMERIC" "SORT_STRING" "ASSERT_ACTIVE"
       "ASSERT_CALLBACK" "ASSERT_BAIL" "ASSERT_WARNING"
       "ASSERT_QUIET_EVAL" "CONNECTION_ABORTED" "CONNECTION_NORMAL"
       "CONNECTION_TIMEOUT" "M_E" "M_LOG2E" "M_LOG10E" "M_LN2"
       "M_LN10" "M_PI" "M_PI_2" "M_PI_4" "M_1_PI" "M_2_PI"
       "M_2_SQRTPI" "M_SQRT2" "M_SQRT1_2" "CRYPT_SALT_LENGTH"
       "CRYPT_STD_DES" "CRYPT_EXT_DES" "CRYPT_MD5" "CRYPT_BLOWFISH"
       "DIRECTORY_SEPARATOR" "SEEK_SET" "SEEK_CUR" "SEEK_END"
       "LOCK_SH" "LOCK_EX" "LOCK_UN" "LOCK_NB" "HTML_SPECIALCHARS"
       "HTML_ENTITIES" "ENT_COMPAT" "ENT_QUOTES" "ENT_NOQUOTES"
       "INFO_GENERAL" "INFO_CREDITS" "INFO_CONFIGURATION"
       "INFO_ENVIRONMENT" "INFO_VARIABLES" "INFO_LICENSE" "INFO_ALL"
       "CREDITS_GROUP" "CREDITS_GENERAL" "CREDITS_SAPI"
       "CREDITS_MODULES" "CREDITS_DOCS" "CREDITS_FULLPAGE"
       "CREDITS_QA" "CREDITS_ALL" "PHP_OUTPUT_HANDLER_START"
       "PHP_OUTPUT_HANDLER_CONT" "PHP_OUTPUT_HANDLER_END"
       "STR_PAD_LEFT" "STR_PAD_RIGHT" "STR_PAD_BOTH"
       "PATHINFO_DIRNAME" "PATHINFO_BASENAME" "PATHINFO_EXTENSION"
       "CHAR_MAX" "LC_CTYPE" "LC_NUMERIC" "LC_TIME" "LC_COLLATE"
       "LC_MONETARY" "LC_ALL" "LC_MESSAGES" "LOG_EMERG" "LOG_ALERT"
       "LOG_CRIT" "LOG_ERR" "LOG_WARNING" "LOG_NOTICE" "LOG_INFO"
       "LOG_DEBUG" "LOG_KERN" "LOG_USER" "LOG_MAIL" "LOG_DAEMON"
       "LOG_AUTH" "LOG_SYSLOG" "LOG_LPR" "LOG_NEWS" "LOG_UUCP"
       "LOG_CRON" "LOG_AUTHPRIV" "LOG_LOCAL0" "LOG_LOCAL1"
       "LOG_LOCAL2" "LOG_LOCAL3" "LOG_LOCAL4" "LOG_LOCAL5"
       "LOG_LOCAL6" "LOG_LOCAL7" "LOG_PID" "LOG_CONS" "LOG_ODELAY"
       "LOG_NDELAY" "LOG_NOWAIT" "LOG_PERROR"
       
       ;; Disabled by default because they slow buffer loading
       ;; If you have use for them, decomment the strings 
       ;; that you want colored.
       ;; To compile, you may have to increase 'max-specpdl-size'

       ;; from other bundled extensions:
;        "CAL_EASTER_TO_xxx" "VT_NULL" "VT_EMPTY" "VT_UI1" "VT_I2"
;        "VT_I4" "VT_R4" "VT_R8" "VT_BOOL" "VT_ERROR" "VT_CY" "VT_DATE"
;        "VT_BSTR" "VT_DECIMAL" "VT_UNKNOWN" "VT_DISPATCH" "VT_VARIANT"
;        "VT_I1" "VT_UI2" "VT_UI4" "VT_INT" "VT_UINT" "VT_ARRAY"
;        "VT_BYREF" "CP_ACP" "CP_MACCP" "CP_OEMCP" "CP_SYMBOL"
;        "CP_THREAD_ACP" "CP_UTF7" "CP_UTF8" "CPDF_PM_NONE"
;        "CPDF_PM_OUTLINES" "CPDF_PM_THUMBS" "CPDF_PM_FULLSCREEN"
;        "CPDF_PL_SINGLE" "CPDF_PL_1COLUMN" "CPDF_PL_2LCOLUMN"
;        "CPDF_PL_2RCOLUMN" "CURLOPT_PORT" "CURLOPT_FILE"
;        "CURLOPT_INFILE" "CURLOPT_INFILESIZE" "CURLOPT_URL"
;        "CURLOPT_PROXY" "CURLOPT_VERBOSE" "CURLOPT_HEADER"
;        "CURLOPT_HTTPHEADER" "CURLOPT_NOPROGRESS" "CURLOPT_NOBODY"
;        "CURLOPT_FAILONERROR" "CURLOPT_UPLOAD" "CURLOPT_POST"
;        "CURLOPT_FTPLISTONLY" "CURLOPT_FTPAPPEND" "CURLOPT_NETRC"
;        "CURLOPT_FOLLOWLOCATION" "CURLOPT_FTPASCII" "CURLOPT_PUT"
;        "CURLOPT_MUTE" "CURLOPT_USERPWD" "CURLOPT_PROXYUSERPWD"
;        "CURLOPT_RANGE" "CURLOPT_TIMEOUT" "CURLOPT_POSTFIELDS"
;        "CURLOPT_REFERER" "CURLOPT_USERAGENT" "CURLOPT_FTPPORT"
;        "CURLOPT_LOW_SPEED_LIMIT" "CURLOPT_LOW_SPEED_TIME"
;        "CURLOPT_RESUME_FROM" "CURLOPT_COOKIE" "CURLOPT_SSLCERT"
;        "CURLOPT_SSLCERTPASSWD" "CURLOPT_WRITEHEADER"
;        "CURLOPT_COOKIEFILE" "CURLOPT_SSLVERSION"
;        "CURLOPT_TIMECONDITION" "CURLOPT_TIMEVALUE"
;        "CURLOPT_CUSTOMREQUEST" "CURLOPT_STDERR" "CURLOPT_TRANSFERTEXT"
;        "CURLOPT_RETURNTRANSFER" "CURLOPT_QUOTE" "CURLOPT_POSTQUOTE"
;        "CURLOPT_INTERFACE" "CURLOPT_KRB4LEVEL"
;        "CURLOPT_HTTPPROXYTUNNEL" "CURLOPT_FILETIME"
;        "CURLOPT_WRITEFUNCTION" "CURLOPT_READFUNCTION"
;        "CURLOPT_PASSWDFUNCTION" "CURLOPT_HEADERFUNCTION"
;        "CURLOPT_MAXREDIRS" "CURLOPT_MAXCONNECTS" "CURLOPT_CLOSEPOLICY"
;        "CURLOPT_FRESH_CONNECT" "CURLOPT_FORBID_REUSE"
;        "CURLOPT_RANDOM_FILE" "CURLOPT_EGDSOCKET"
;        "CURLOPT_CONNECTTIMEOUT" "CURLOPT_SSL_VERIFYPEER"
;        "CURLOPT_CAINFO" "CURLOPT_BINARYTRANSER"
;        "CURLCLOSEPOLICY_LEAST_RECENTLY_USED" "CURLCLOSEPOLICY_OLDEST"
;        "CURLINFO_EFFECTIVE_URL" "CURLINFO_HTTP_CODE"
;        "CURLINFO_HEADER_SIZE" "CURLINFO_REQUEST_SIZE"
;        "CURLINFO_TOTAL_TIME" "CURLINFO_NAMELOOKUP_TIME"
;        "CURLINFO_CONNECT_TIME" "CURLINFO_PRETRANSFER_TIME"
;        "CURLINFO_SIZE_UPLOAD" "CURLINFO_SIZE_DOWNLOAD"
;        "CURLINFO_SPEED_DOWNLOAD" "CURLINFO_SPEED_UPLOAD"
;        "CURLINFO_FILETIME" "CURLE_OK" "CURLE_UNSUPPORTED_PROTOCOL"
;        "CURLE_FAILED_INIT" "CURLE_URL_MALFORMAT"
;        "CURLE_URL_MALFORMAT_USER" "CURLE_COULDNT_RESOLVE_PROXY"
;        "CURLE_COULDNT_RESOLVE_HOST" "CURLE_COULDNT_CONNECT"
;        "CURLE_FTP_WEIRD_SERVER_REPLY" "CURLE_FTP_ACCESS_DENIED"
;        "CURLE_FTP_USER_PASSWORD_INCORRECT"
;        "CURLE_FTP_WEIRD_PASS_REPLY" "CURLE_FTP_WEIRD_USER_REPLY"
;        "CURLE_FTP_WEIRD_PASV_REPLY" "CURLE_FTP_WEIRD_227_FORMAT"
;        "CURLE_FTP_CANT_GET_HOST" "CURLE_FTP_CANT_RECONNECT"
;        "CURLE_FTP_COULDNT_SET_BINARY" "CURLE_PARTIAL_FILE"
;        "CURLE_FTP_COULDNT_RETR_FILE" "CURLE_FTP_WRITE_ERROR"
;        "CURLE_FTP_QUOTE_ERROR" "CURLE_HTTP_NOT_FOUND"
;        "CURLE_WRITE_ERROR" "CURLE_MALFORMAT_USER"
;        "CURLE_FTP_COULDNT_STOR_FILE" "CURLE_READ_ERROR"
;        "CURLE_OUT_OF_MEMORY" "CURLE_OPERATION_TIMEOUTED"
;        "CURLE_FTP_COULDNT_SET_ASCII" "CURLE_FTP_PORT_FAILED"
;        "CURLE_FTP_COULDNT_USE_REST" "CURLE_FTP_COULDNT_GET_SIZE"
;        "CURLE_HTTP_RANGE_ERROR" "CURLE_HTTP_POST_ERROR"
;        "CURLE_SSL_CONNECT_ERROR" "CURLE_FTP_BAD_DOWNLOAD_RESUME"
;        "CURLE_FILE_COULDNT_READ_FILE" "CURLE_LDAP_CANNOT_BIND"
;        "CURLE_LDAP_SEARCH_FAILED" "CURLE_LIBRARY_NOT_FOUND"
;        "CURLE_FUNCTION_NOT_FOUND" "CURLE_ABORTED_BY_CALLBACK"
;        "CURLE_BAD_FUNCTION_ARGUMENT" "CURLE_BAD_CALLING_ORDER"
;        "CURLE_HTTP_PORT_FAILED" "CURLE_BAD_PASSWORD_ENTERED"
;        "CURLE_TOO_MANY_REDIRECTS" "CURLE_UNKOWN_TELNET_OPTION"
;        "CURLE_TELNET_OPTION_SYNTAX" "CURLE_ALREADY_COMPLETE"
;        "DBX_MYSQL" "DBX_ODBC" "DBX_PGSQL" "DBX_MSSQL" "DBX_PERSISTENT"
;        "DBX_RESULT_INFO" "DBX_RESULT_INDEX" "DBX_RESULT_ASSOC"
;        "DBX_CMP_TEXT" "DBX_CMP_NUMBER" "XML_ELEMENT_NODE"
;        "XML_ATTRIBUTE_NODE" "XML_TEXT_NODE" "XML_CDATA_SECTION_NODE"
;        "XML_ENTITY_REF_NODE" "XML_ENTITY_NODE" "XML_PI_NODE"
;        "XML_COMMENT_NODE" "XML_DOCUMENT_NODE" "XML_DOCUMENT_TYPE_NODE"
;        "XML_DOCUMENT_FRAG_NODE" "XML_NOTATION_NODE"
;        "XML_HTML_DOCUMENT_NODE" "XML_DTD_NODE" "XML_ELEMENT_DECL_NODE"
;        "XML_ATTRIBUTE_DECL_NODE" "XML_ENTITY_DECL_NODE"
;        "XML_NAMESPACE_DECL_NODE" "XML_GLOBAL_NAMESPACE"
;        "XML_LOCAL_NAMESPACE" "XML_ATTRIBUTE_CDATA" "XML_ATTRIBUTE_ID"
;        "XML_ATTRIBUTE_IDREF" "XML_ATTRIBUTE_IDREFS"
;        "XML_ATTRIBUTE_ENTITY" "XML_ATTRIBUTE_NMTOKEN"
;        "XML_ATTRIBUTE_NMTOKENS" "XML_ATTRIBUTE_ENUMERATION"
;        "XML_ATTRIBUTE_NOTATION" "XPATH_UNDEFINED" "XPATH_NODESET"
;        "XPATH_BOOLEAN" "XPATH_NUMBER" "XPATH_STRING" "XPATH_POINT"
;        "XPATH_RANGE" "XPATH_LOCATIONSET" "XPATH_USERS" "FBSQL_ASSOC"
;        "FBSQL_NUM" "FBSQL_BOTH" "FDFValue" "FDFStatus" "FDFFile"
;        "FDFID" "FDFFf" "FDFSetFf" "FDFClearFf" "FDFFlags" "FDFSetF"
;        "FDFClrF" "FDFAP" "FDFAS" "FDFAction" "FDFAA" "FDFAPRef"
;        "FDFIF" "FDFEnter" "FDFExit" "FDFDown" "FDFUp" "FDFFormat"
;        "FDFValidate" "FDFKeystroke" "FDFCalculate"
;        "FRIBIDI_CHARSET_UTF8" "FRIBIDI_CHARSET_8859_6"
;        "FRIBIDI_CHARSET_8859_8" "FRIBIDI_CHARSET_CP1255"
;        "FRIBIDI_CHARSET_CP1256" "FRIBIDI_CHARSET_ISIRI_3342"
;        "FTP_ASCII" "FTP_BINARY" "FTP_IMAGE" "FTP_TEXT" "IMG_GIF"
;        "IMG_JPG" "IMG_JPEG" "IMG_PNG" "IMG_WBMP" "IMG_COLOR_TILED"
;        "IMG_COLOR_STYLED" "IMG_COLOR_BRUSHED"
;        "IMG_COLOR_STYLEDBRUSHED" "IMG_COLOR_TRANSPARENT"
;        "IMG_ARC_ROUNDED" "IMG_ARC_PIE" "IMG_ARC_CHORD"
;        "IMG_ARC_NOFILL" "IMG_ARC_EDGED" "GMP_ROUND_ZERO"
;        "GMP_ROUND_PLUSINF" "GMP_ROUND_MINUSINF" "HW_ATTR_LANG"
;        "HW_ATTR_NR" "HW_ATTR_NONE" "IIS_READ" "IIS_WRITE"
;        "IIS_EXECUTE" "IIS_SCRIPT" "IIS_ANONYMOUS" "IIS_BASIC"
;        "IIS_NTLM" "NIL" "OP_DEBUG" "OP_READONLY" "OP_ANONYMOUS"
;        "OP_SHORTCACHE" "OP_SILENT" "OP_PROTOTYPE" "OP_HALFOPEN"
;        "OP_EXPUNGE" "OP_SECURE" "CL_EXPUNGE" "FT_UID" "FT_PEEK"
;        "FT_NOT" "FT_INTERNAL" "FT_PREFETCHTEXT" "ST_UID" "ST_SILENT"
;        "ST_SET" "CP_UID" "CP_MOVE" "SE_UID" "SE_FREE" "SE_NOPREFETCH"
;        "SO_FREE" "SO_NOSERVER" "SA_MESSAGES" "SA_RECENT" "SA_UNSEEN"
;        "SA_UIDNEXT" "SA_UIDVALIDITY" "SA_ALL" "LATT_NOINFERIORS"
;        "LATT_NOSELECT" "LATT_MARKED" "LATT_UNMARKED" "SORTDATE"
;        "SORTARRIVAL" "SORTFROM" "SORTSUBJECT" "SORTTO" "SORTCC"
;        "SORTSIZE" "TYPETEXT" "TYPEMULTIPART" "TYPEMESSAGE"
;        "TYPEAPPLICATION" "TYPEAUDIO" "TYPEIMAGE" "TYPEVIDEO"
;        "TYPEOTHER" "ENC7BIT" "ENC8BIT" "ENCBINARY" "ENCBASE64"
;        "ENCQUOTEDPRINTABLE" "ENCOTHER" "INGRES_ASSOC" "INGRES_NUM"
;        "INGRES_BOTH" "IBASE_DEFAULT" "IBASE_TEXT" "IBASE_UNIXTIME"
;        "IBASE_READ" "IBASE_COMMITTED" "IBASE_CONSISTENCY"
;        "IBASE_NOWAIT" "IBASE_TIMESTAMP" "IBASE_DATE" "IBASE_TIME"
;        "LDAP_DEREF_NEVER" "LDAP_DEREF_SEARCHING" "LDAP_DEREF_FINDING"
;        "LDAP_DEREF_ALWAYS" "LDAP_OPT_DEREF" "LDAP_OPT_SIZELIMIT"
;        "LDAP_OPT_TIMELIMIT" "LDAP_OPT_PROTOCOL_VERSION"
;        "LDAP_OPT_ERROR_NUMBER" "LDAP_OPT_REFERRALS" "LDAP_OPT_RESTART"
;        "LDAP_OPT_HOST_NAME" "LDAP_OPT_ERROR_STRING"
;        "LDAP_OPT_MATCHED_DN" "LDAP_OPT_SERVER_CONTROLS"
;        "LDAP_OPT_CLIENT_CONTROLS" "GSLC_SSL_NO_AUTH"
;        "GSLC_SSL_ONEWAY_AUTH" "GSLC_SSL_TWOWAY_AUTH" "MCAL_SUNDAY"
;        "MCAL_MONDAY" "MCAL_TUESDAY" "MCAL_WEDNESDAY" "MCAL_THURSDAY"
;        "MCAL_FRIDAY" "MCAL_SATURDAY" "MCAL_JANUARY" "MCAL_FEBRUARY"
;        "MCAL_MARCH" "MCAL_APRIL" "MCAL_MAY" "MCAL_JUNE" "MCAL_JULY"
;        "MCAL_AUGUST" "MCAL_SEPTEMBER" "MCAL_OCTOBER" "MCAL_NOVEMBER"
;        "MCAL_RECUR_NONE" "MCAL_RECUR_DAILY" "MCAL_RECUR_WEEKLY"
;        "MCAL_RECUR_MONTHLY_MDAY" "MCAL_RECUR_MONTHLY_WDAY"
;        "MCAL_RECUR_YEARLY" "MCAL_M_SUNDAY" "MCAL_M_MONDAY"
;        "MCAL_M_TUESDAY" "MCAL_M_WEDNESDAY" "MCAL_M_THURSDAY"
;        "MCAL_M_FRIDAY" "MCAL_M_SATURDAY" "MCAL_M_WEEKDAYS"
;        "MCAL_M_WEEKEND" "MCAL_M_ALLDAYS" "MCRYPT_" "MCRYPT_"
;        "MCRYPT_ENCRYPT" "MCRYPT_DECRYPT" "MCRYPT_DEV_RANDOM"
;        "MCRYPT_DEV_URANDOM" "MCRYPT_RAND" "SWFBUTTON_HIT"
;        "SWFBUTTON_DOWN" "SWFBUTTON_OVER" "SWFBUTTON_UP"
;        "SWFBUTTON_MOUSEUPOUTSIDE" "SWFBUTTON_DRAGOVER"
;        "SWFBUTTON_DRAGOUT" "SWFBUTTON_MOUSEUP" "SWFBUTTON_MOUSEDOWN"
;        "SWFBUTTON_MOUSEOUT" "SWFBUTTON_MOUSEOVER"
;        "SWFFILL_RADIAL_GRADIENT" "SWFFILL_LINEAR_GRADIENT"
;        "SWFFILL_TILED_BITMAP" "SWFFILL_CLIPPED_BITMAP"
;        "SWFTEXTFIELD_HASLENGTH" "SWFTEXTFIELD_NOEDIT"
;        "SWFTEXTFIELD_PASSWORD" "SWFTEXTFIELD_MULTILINE"
;        "SWFTEXTFIELD_WORDWRAP" "SWFTEXTFIELD_DRAWBOX"
;        "SWFTEXTFIELD_NOSELECT" "SWFTEXTFIELD_HTML"
;        "SWFTEXTFIELD_ALIGN_LEFT" "SWFTEXTFIELD_ALIGN_RIGHT"
;        "SWFTEXTFIELD_ALIGN_CENTER" "SWFTEXTFIELD_ALIGN_JUSTIFY"
;        "UDM_FIELD_URLID" "UDM_FIELD_URL" "UDM_FIELD_CONTENT"
;        "UDM_FIELD_TITLE" "UDM_FIELD_KEYWORDS" "UDM_FIELD_DESC"
;        "UDM_FIELD_DESCRIPTION" "UDM_FIELD_TEXT" "UDM_FIELD_SIZE"
;        "UDM_FIELD_RATING" "UDM_FIELD_SCORE" "UDM_FIELD_MODIFIED"
;        "UDM_FIELD_ORDER" "UDM_FIELD_CRC" "UDM_FIELD_CATEGORY"
;        "UDM_PARAM_PAGE_SIZE" "UDM_PARAM_PAGE_NUM"
;        "UDM_PARAM_SEARCH_MODE" "UDM_PARAM_CACHE_MODE"
;        "UDM_PARAM_TRACK_MODE" "UDM_PARAM_PHRASE_MODE"
;        "UDM_PARAM_CHARSET" "UDM_PARAM_STOPTABLE"
;        "UDM_PARAM_STOP_TABLE" "UDM_PARAM_STOPFILE"
;        "UDM_PARAM_STOP_FILE" "UDM_PARAM_WEIGHT_FACTOR"
;        "UDM_PARAM_WORD_MATCH" "UDM_PARAM_MAX_WORD_LEN"
;        "UDM_PARAM_MAX_WORDLEN" "UDM_PARAM_MIN_WORD_LEN"
;        "UDM_PARAM_MIN_WORDLEN" "UDM_PARAM_ISPELL_PREFIXES"
;        "UDM_PARAM_ISPELL_PREFIX" "UDM_PARAM_PREFIXES"
;        "UDM_PARAM_PREFIX" "UDM_PARAM_CROSS_WORDS"
;        "UDM_PARAM_CROSSWORDS" "UDM_LIMIT_CAT" "UDM_LIMIT_URL"
;        "UDM_LIMIT_TAG" "UDM_LIMIT_LANG" "UDM_LIMIT_DATE"
;        "UDM_PARAM_FOUND" "UDM_PARAM_NUM_ROWS" "UDM_PARAM_WORDINFO"
;        "UDM_PARAM_WORD_INFO" "UDM_PARAM_SEARCHTIME"
;        "UDM_PARAM_SEARCH_TIME" "UDM_PARAM_FIRST_DOC"
;        "UDM_PARAM_LAST_DOC" "UDM_MODE_ALL" "UDM_MODE_ANY"
;        "UDM_MODE_BOOL" "UDM_MODE_PHRASE" "UDM_CACHE_ENABLED"
;        "UDM_CACHE_DISABLED" "UDM_TRACK_ENABLED" "UDM_TRACK_DISABLED"
;        "UDM_PHRASE_ENABLED" "UDM_PHRASE_DISABLED"
;        "UDM_CROSS_WORDS_ENABLED" "UDM_CROSSWORDS_ENABLED"
;        "UDM_CROSS_WORDS_DISABLED" "UDM_CROSSWORDS_DISABLED"
;        "UDM_PREFIXES_ENABLED" "UDM_PREFIX_ENABLED"
;        "UDM_ISPELL_PREFIXES_ENABLED" "UDM_ISPELL_PREFIX_ENABLED"
;        "UDM_PREFIXES_DISABLED" "UDM_PREFIX_DISABLED"
;        "UDM_ISPELL_PREFIXES_DISABLED" "UDM_ISPELL_PREFIX_DISABLED"
;        "UDM_ISPELL_TYPE_AFFIX" "UDM_ISPELL_TYPE_SPELL"
;        "UDM_ISPELL_TYPE_DB" "UDM_ISPELL_TYPE_SERVER" "UDM_MATCH_WORD"
;        "UDM_MATCH_BEGIN" "UDM_MATCH_SUBSTR" "UDM_MATCH_END"
;        "MSQL_ASSOC" "MSQL_NUM" "MSQL_BOTH" "MYSQL_ASSOC" "MYSQL_NUM"
;        "MYSQL_BOTH" "MYSQL_USE_RESULT" "MYSQL_STORE_RESULT"
;        "OCI_DEFAULT" "OCI_DESCRIBE_ONLY" "OCI_COMMIT_ON_SUCCESS"
;        "OCI_EXACT_FETCH" "SQLT_BFILEE" "SQLT_CFILEE" "SQLT_CLOB"
;        "SQLT_BLOB" "SQLT_RDD" "OCI_B_SQLT_NTY" "OCI_SYSDATE"
;        "OCI_B_BFILE" "OCI_B_CFILEE" "OCI_B_CLOB" "OCI_B_BLOB"
;        "OCI_B_ROWID" "OCI_B_CURSOR" "OCI_B_BIN" "OCI_ASSOC" "OCI_NUM"
;        "OCI_BOTH" "OCI_RETURN_NULLS" "OCI_RETURN_LOBS"
;        "OCI_DTYPE_FILE" "OCI_DTYPE_LOB" "OCI_DTYPE_ROWID" "OCI_D_FILE"
;        "OCI_D_LOB" "OCI_D_ROWID" "ODBC_TYPE" "ODBC_BINMODE_PASSTHRU"
;        "ODBC_BINMODE_RETURN" "ODBC_BINMODE_CONVERT" "SQL_ODBC_CURSORS"
;        "SQL_CUR_USE_DRIVER" "SQL_CUR_USE_IF_NEEDED" "SQL_CUR_USE_ODBC"
;        "SQL_CONCURRENCY" "SQL_CONCUR_READ_ONLY" "SQL_CONCUR_LOCK"
;        "SQL_CONCUR_ROWVER" "SQL_CONCUR_VALUES" "SQL_CURSOR_TYPE"
;        "SQL_CURSOR_FORWARD_ONLY" "SQL_CURSOR_KEYSET_DRIVEN"
;        "SQL_CURSOR_DYNAMIC" "SQL_CURSOR_STATIC" "SQL_KEYSET_SIZE"
;        "SQL_CHAR" "SQL_VARCHAR" "SQL_LONGVARCHAR" "SQL_DECIMAL"
;        "SQL_NUMERIC" "SQL_BIT" "SQL_TINYINT" "SQL_SMALLINT"
;        "SQL_INTEGER" "SQL_BIGINT" "SQL_REAL" "SQL_FLOAT" "SQL_DOUBLE"
;        "SQL_BINARY" "SQL_VARBINARY" "SQL_LONGVARBINARY" "SQL_DATE"
;        "SQL_TIME" "SQL_TIMESTAMP" "SQL_TYPE_DATE" "SQL_TYPE_TIME"
;        "SQL_TYPE_TIMESTAMP" "SQL_BEST_ROWID" "SQL_ROWVER"
;        "SQL_SCOPE_CURROW" "SQL_SCOPE_TRANSACTION" "SQL_SCOPE_SESSION"
;        "SQL_NO_NULLS" "SQL_NULLABLE" "SQL_INDEX_UNIQUE"
;        "SQL_INDEX_ALL" "SQL_ENSURE" "SQL_QUICK"
;        "X509_PURPOSE_SSL_CLIENT" "X509_PURPOSE_SSL_SERVER"
;        "X509_PURPOSE_NS_SSL_SERVER" "X509_PURPOSE_SMIME_SIGN"
;        "X509_PURPOSE_SMIME_ENCRYPT" "X509_PURPOSE_CRL_SIGN"
;        "X509_PURPOSE_ANY" "PKCS7_DETACHED" "PKCS7_TEXT"
;        "PKCS7_NOINTERN" "PKCS7_NOVERIFY" "PKCS7_NOCHAIN"
;        "PKCS7_NOCERTS" "PKCS7_NOATTR" "PKCS7_BINARY" "PKCS7_NOSIGS"
;        "OPENSSL_PKCS1_PADDING" "OPENSSL_SSLV23_PADDING"
;        "OPENSSL_NO_PADDING" "OPENSSL_PKCS1_OAEP_PADDING"
;        "ORA_BIND_INOUT" "ORA_BIND_IN" "ORA_BIND_OUT"
;        "ORA_FETCHINTO_ASSOC" "ORA_FETCHINTO_NULLS"
;        "PREG_PATTERN_ORDER" "PREG_SET_ORDER" "PREG_SPLIT_NO_EMPTY"
;        "PREG_SPLIT_DELIM_CAPTURE" "PGSQL_ASSOC" "PGSQL_NUM"
;        "PGSQL_BOTH" "PRINTER_COPIES" "PRINTER_MODE" "PRINTER_TITLE"
;        "PRINTER_DEVICENAME" "PRINTER_DRIVERVERSION"
;        "PRINTER_RESOLUTION_Y" "PRINTER_RESOLUTION_X" "PRINTER_SCALE"
;        "PRINTER_BACKGROUND_COLOR" "PRINTER_PAPER_LENGTH"
;        "PRINTER_PAPER_WIDTH" "PRINTER_PAPER_FORMAT"
;        "PRINTER_FORMAT_CUSTOM" "PRINTER_FORMAT_LETTER"
;        "PRINTER_FORMAT_LEGAL" "PRINTER_FORMAT_A3" "PRINTER_FORMAT_A4"
;        "PRINTER_FORMAT_A5" "PRINTER_FORMAT_B4" "PRINTER_FORMAT_B5"
;        "PRINTER_FORMAT_FOLIO" "PRINTER_ORIENTATION"
;        "PRINTER_ORIENTATION_PORTRAIT" "PRINTER_ORIENTATION_LANDSCAPE"
;        "PRINTER_TEXT_COLOR" "PRINTER_TEXT_ALIGN" "PRINTER_TA_BASELINE"
;        "PRINTER_TA_BOTTOM" "PRINTER_TA_TOP" "PRINTER_TA_CENTER"
;        "PRINTER_TA_LEFT" "PRINTER_TA_RIGHT" "PRINTER_PEN_SOLID"
;        "PRINTER_PEN_DASH" "PRINTER_PEN_DOT" "PRINTER_PEN_DASHDOT"
;        "PRINTER_PEN_DASHDOTDOT" "PRINTER_PEN_INVISIBLE"
;        "PRINTER_BRUSH_SOLID" "PRINTER_BRUSH_CUSTOM"
;        "PRINTER_BRUSH_DIAGONAL" "PRINTER_BRUSH_CROSS"
;        "PRINTER_BRUSH_DIAGCROSS" "PRINTER_BRUSH_FDIAGONAL"
;        "PRINTER_BRUSH_HORIZONTAL" "PRINTER_BRUSH_VERTICAL"
;        "PRINTER_FW_THIN" "PRINTER_FW_ULTRALIGHT" "PRINTER_FW_LIGHT"
;        "PRINTER_FW_NORMAL" "PRINTER_FW_MEDIUM" "PRINTER_FW_BOLD"
;        "PRINTER_FW_ULTRABOLD" "PRINTER_FW_HEAVY" "PRINTER_ENUM_LOCAL"
;        "PRINTER_ENUM_NAME" "PRINTER_ENUM_SHARED"
;        "PRINTER_ENUM_DEFAULT" "PRINTER_ENUM_CONNECTIONS"
;        "PRINTER_ENUM_NETWORK" "PRINTER_ENUM_REMOTE" "PSPELL_FAST"
;        "PSPELL_NORMAL" "PSPELL_BAD_SPELLERS" "PSPELL_RUN_TOGETHER"
;        "SID" "SID" "AF_UNIX" "AF_INET" "SOCK_STREAM" "SOCK_DGRAM"
;        "SOCK_RAW" "SOCK_SEQPACKET" "SOCK_RDM" "MSG_OOB" "MSG_WAITALL"
;        "MSG_PEEK" "MSG_DONTROUTE" "SO_DEBUG" "SO_REUSEADDR"
;        "SO_KEEPALIVE" "SO_DONTROUTE" "SO_LINGER" "SO_BROADCAST"
;        "SO_OOBINLINE" "SO_SNDBUF" "SO_RCVBUF" "SO_SNDLOWAT"
;        "SO_RCVLOWAT" "SO_SNDTIMEO" "SO_RCVTIMEO" "SO_TYPE" "SO_ERROR"
;        "SOL_SOCKET" "PHP_NORMAL_READ" "PHP_BINARY_READ"
;        "PHP_SYSTEM_READ" "SOL_TCP" "SOL_UDP" "MOD_COLOR" "MOD_MATRIX"
;        "TYPE_PUSHBUTTON" "TYPE_MENUBUTTON" "BSHitTest" "BSDown"
;        "BSOver" "BSUp" "OverDowntoIdle" "IdletoOverDown"
;        "OutDowntoIdle" "OutDowntoOverDown" "OverDowntoOutDown"
;        "OverUptoOverDown" "OverUptoIdle" "IdletoOverUp" "ButtonEnter"
;        "ButtonExit" "MenuEnter" "MenuExit" "XML_ERROR_NONE"
;        "XML_ERROR_NO_MEMORY" "XML_ERROR_SYNTAX"
;        "XML_ERROR_NO_ELEMENTS" "XML_ERROR_INVALID_TOKEN"
;        "XML_ERROR_UNCLOSED_TOKEN" "XML_ERROR_PARTIAL_CHAR"
;        "XML_ERROR_TAG_MISMATCH" "XML_ERROR_DUPLICATE_ATTRIBUTE"
;        "XML_ERROR_JUNK_AFTER_DOC_ELEMENT" "XML_ERROR_PARAM_ENTITY_REF"
;        "XML_ERROR_UNDEFINED_ENTITY" "XML_ERROR_RECURSIVE_ENTITY_REF"
;        "XML_ERROR_ASYNC_ENTITY" "XML_ERROR_BAD_CHAR_REF"
;        "XML_ERROR_BINARY_ENTITY_REF"
;        "XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF"
;        "XML_ERROR_MISPLACED_XML_PI" "XML_ERROR_UNKNOWN_ENCODING"
;        "XML_ERROR_INCORRECT_ENCODING"
;        "XML_ERROR_UNCLOSED_CDATA_SECTION"
;        "XML_ERROR_EXTERNAL_ENTITY_HANDLING" "XML_OPTION_CASE_FOLDING"
;        "XML_OPTION_TARGET_ENCODING" "XML_OPTION_SKIP_TAGSTART"
;        "XML_OPTION_SKIP_WHITE" "YPERR_BADARGS" "YPERR_BADDB"
;        "YPERR_BUSY" "YPERR_DOMAIN" "YPERR_KEY" "YPERR_MAP"
;        "YPERR_NODOM" "YPERR_NOMORE" "YPERR_PMAP" "YPERR_RESRC"
;        "YPERR_RPC" "YPERR_YPBIND" "YPERR_YPERR" "YPERR_YPSERV"
;        "YPERR_VERS" "FORCE_GZIP" "FORCE_DEFLATE"

       ;; PEAR constants
;        "PEAR_ERROR_RETURN" "PEAR_ERROR_PRINT" "PEAR_ERROR_TRIGGER"
;        "PEAR_ERROR_DIE" "PEAR_ERROR_CALLBACK" "OS_WINDOWS" "OS_UNIX"
;        "PEAR_OS" "DB_OK" "DB_ERROR" "DB_ERROR_SYNTAX"
;        "DB_ERROR_CONSTRAINT" "DB_ERROR_NOT_FOUND"
;        "DB_ERROR_ALREADY_EXISTS" "DB_ERROR_UNSUPPORTED"
;        "DB_ERROR_MISMATCH" "DB_ERROR_INVALID" "DB_ERROR_NOT_CAPABLE"
;        "DB_ERROR_TRUNCATED" "DB_ERROR_INVALID_NUMBER"
;        "DB_ERROR_INVALID_DATE" "DB_ERROR_DIVZERO"
;        "DB_ERROR_NODBSELECTED" "DB_ERROR_CANNOT_CREATE"
;        "DB_ERROR_CANNOT_DELETE" "DB_ERROR_CANNOT_DROP"
;        "DB_ERROR_NOSUCHTABLE" "DB_ERROR_NOSUCHFIELD"
;        "DB_ERROR_NEED_MORE_DATA" "DB_ERROR_NOT_LOCKED"
;        "DB_ERROR_VALUE_COUNT_ON_ROW" "DB_ERROR_INVALID_DSN"
;        "DB_ERROR_CONNECT_FAILED" "DB_WARNING" "DB_WARNING_READ_ONLY"
;        "DB_PARAM_SCALAR" "DB_PARAM_OPAQUE" "DB_BINMODE_PASSTHRU"
;        "DB_BINMODE_RETURN" "DB_BINMODE_CONVERT" "DB_FETCHMODE_DEFAULT"
;        "DB_FETCHMODE_ORDERED" "DB_FETCHMODE_ASSOC"
;        "DB_FETCHMODE_FLIPPED" "DB_GETMODE_ORDERED" "DB_GETMODE_ASSOC"
;        "DB_GETMODE_FLIPPED" "DB_TABLEINFO_ORDER"
;        "DB_TABLEINFO_ORDERTABLE" "DB_TABLEINFO_FULL"
       
       ) t))
  "PHP constants.")

(defconst php-keywords
  (eval-when-compile
    (regexp-opt
     ;; "class", "new" and "extends" get special treatment
     ;; "case" and  "default" get special treatment elsewhere
     '("and" "as" "break" "continue" "declare" "do" "echo" "else" "elseif"
       "endfor" "endforeach" "endif" "endswitch" "endwhile" "exit"
       "extends" "for" "foreach" "global" "if" "include" "include_once"
       "next" "or" "require" "require_once" "return" "static" "switch"
       "then" "var" "while" "xor") t))
  "PHP keywords.")

(defconst php-identifier
  (eval-when-compile
    '"[a-zA-Z\_\x7f-\xff][a-zA-Z0-9\_\x7f-\xff]*")
  "Characters in a PHP identifier.")

(defconst php-types
  (eval-when-compile
    (regexp-opt '("array" "bool" "char" "const" "double" "float" "int"
		  "integer" "long" "mixed" "object" "real" 
		  "string") t))
  "PHP types.")

;; Set up font locking
(defconst php-font-lock-keywords-1
  (list
   ;; Fontify constants
   (cons
    (concat "\\<\\(" php-constants "\\)\\>")
    'font-lock-constant-face)
   
   ;; Fontify keywords
   (cons
    (concat "\\<\\(" php-keywords "\\)\\>")
    'font-lock-keyword-face)
   
   ;; Fontify keywords and targets, and case default tags.
   (list "\\<\\(break\\|case\\|continue\\)\\>[ \t]*\\(-?\\sw+\\)?"
	 '(1 font-lock-keyword-face) '(2 font-lock-constant-face t t))
   ;; This must come after the one for keywords and targets.
   '(":" ("^[ \t]*\\(\\sw+\\)[ \t]*:[ \t]*$"
	  (beginning-of-line) (end-of-line)
	  (1 font-lock-constant-face)))
   
   ;; treat 'print' as keyword only when not used like a function name
   '("\\<print\\s-*(" . default)
   '("\\<print\\>" . font-lock-keyword-face)
   
   ;; Fontify PHP tag
   '("<\\?\\(php\\)?" . font-lock-constant-face)
   '("\\?>" . font-lock-constant-face)
   
   ;; Fontify ASP-style tag
   '("<\\%\\(=\\)?" . font-lock-constant-face)
   '("\\%>" . font-lock-constant-face)

   )
  "Subdued level highlighting for PHP mode.")

(defconst php-font-lock-keywords-2
  (append
   php-font-lock-keywords-1
   (list
    
    ;; Fontify class declaration
    '("^[ \t]*\\(class\\)[ \t]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face) (2 font-lock-type-face nil t))
    
    ;; Fontify function declaration
    '("^[ \t]*\\(function\\)\\s-+&?\\(\\sw+\\)\\s-*("
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
    
    ;; handle several words specially, to include following word,
    ;; thereby excluding it from unknown-symbol checks later
    '("\\<\\(new\\|extends\\)\\s-+\\$?\\(\\sw+\\)"
      (1 font-lock-keyword-face) (2 font-lock-type-face))
    ))
  "Medium level highlighting for PHP mode.")

(defconst php-font-lock-keywords-3
  (append
   php-font-lock-keywords-2
   (list
    
    ;; <word> or </word> for HTML
    '("</?\\sw+[^>]*>" . font-lock-constant-face)

    ;; HTML entities
    '("&\\w+;" . font-lock-variable-name-face)

    ;; warn about '$' immediately after ->
    '("\\$\\sw+->\\s-*\\(\\$\\)\\(\\sw+\\)"
      (1 font-lock-warning-face) (2 default))
    
    ;; warn about $word.word -- it could be a valid concatenation,
    ;; but without any spaces we'll assume $word->word was meant.
    '("\\$\\sw+\\(\\.\\)\\sw"
      1 font-lock-warning-face)
    
    ;; Warn about ==> instead of =>
    '("==+>" . font-lock-warning-face)
    
    ;; exclude casts from bare-word treatment
    `(,(concat "(\\(" php-types "\\))")
      1 font-lock-type-face)
    
    ;; Fontify variables and function calls
    '("\\$\\(this\\)\\W" (1 font-lock-constant-face nil nil)) ; "this" as constant
    '("\\$\\(\\sw+\\)" (1 font-lock-variable-name-face)) ; $variable
    '("->\\(\\sw+\\)" (1 font-lock-variable-name-face t t)) ; ->variable
    '("->\\(\\sw+\\)\\s-*(" . (1 default t t)) ; ->function_call
    '("\\sw+::\\sw+\\s-*(" . default) ; class::method call
    '("\\<\\sw+\\s-*[[(]" . default)	; word( or word[
    '("\\<[0-9]+" . default)		; number (also matches word)
    
    ;; Warn on any words not already fontified
    '("\\<\\sw+\\>" . font-lock-warning-face)
    ))
  "Gauchy level highlighting for PHP mode.")

(defconst php-font-lock-syntactic-keywords
  (if xemacsp nil
    ;; Mark shell-style comments.  font-lock handles this in a
    ;; separate pass from normal syntactic scanning (somehow), so we
    ;; get a chance to mark these in addition to C and C++ style
    ;; comments.  This only works in GNU Emacs, not Xemacs 21 which
    ;; seems to ignore this same code if we try to use it.
    (list
     ;; Mark _all_ # chars as being comment-start.  That will be
     ;; ignored when inside a quoted string.
     '("\\(\#\\)"
       (1 (11 . nil)))
     ;; Mark all newlines ending a line with # as being comment-end.
     ;; This causes a problem, premature end-of-comment, when '#'
     ;; appears inside a multiline C-style comment.  Oh well.
     '("#.*\\([\n]\\)"
       (1 (12 . nil)))
     )))

;; imenu- from cc-mode
(defvar cc-imenu-php-generic-expression
  (`
   (("class"
     (, (concat
	 "^"				; beginning of line is required
	 "class[ \t\n]+"
	 "\\([a-zA-Z0-9_]+\\)"		; this is the string we want to get
	 "\\([ \t]+extends[ \t]+[a-zA-Z0-9_]+\\)?" ; may be inherited
	 "[ \t\n]*{"
	 )) 2)
    ("function"
     (, (concat
	 "^"				; beginning of line is required
	 "[ \t]*function[ \t\n]+&?"
	 "\\([a-zA-Z0-9_]+\\)[ \t\n]*(.*)" ; this is the string we want to get
	 "[ \t\n]*{"
	 )) 2)
    ))
  )

;; Create "default" symbol for GNU Emacs so that both Xemacs and GNU
;; emacs can refer to the default face by a variable named "default".
(unless (boundp 'default)
  (defvar default 'default))

;; Create faces for XEmacs
(unless (boundp 'font-lock-keyword-face)
  (copy-face 'bold 'font-lock-keyword-face))
(unless (boundp 'font-lock-constant-face)
  (copy-face 'font-lock-keyword-face 'font-lock-constant-face))

(provide 'php-mode)

;;; php-mode.el ends here
