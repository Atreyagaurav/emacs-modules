
(defconst remind-key-keywords (regexp-opt '("RUN" "REM" "ONCE"
	 "SATISFY" "BEFORE" "UNSET" "OMIT" "FIRST" "FROM" "SATISFY"
	 "OMIT" "DATE" "SKIP" "ONCE" "AFTER" "WARN" "PRIORITY" "AT"
	 "SCHED" "IF" "ELSE" "ENDIF" "WARN" "UNTIL" "THROUGH"
	 "SCANFROM" "DURATION" "TAG" "MSG" "MSF" "CAL" "SPECIAL"
	 "IFTRIG" "PS" "PSFILE" "BANNER" "INCLUDE" "PUSH-OMIT-CONTEXT"
	 "DEBUG" "DUMPVARS" "PUSH" "CLEAR" "POP" "CLEAR-OMIT-CONTEXT"
	 "POP-OMIT-CONTEXT" "SET" "ERRMSG" "FSET" "DUMP" "BAN" "INC"
	 "SCAN" "EXIT" "FLUSH" "PRESERVE" "MOON" "COLOR" "UNSET"
	 "ADDOMIT" "OMITFUNC") t))

(defconst remind-key-types (regexp-opt '("INT" "STRING" "TIME" "DATE"
         "SHADE" "DATETIME") t))

(defconst remind-key-variables (regexp-opt '("$CalcUTC" "$CalMode"
	 "$Daemon" "$DateSep" "$DefaultPrio" "$DontFork"
	 "$DontTrigAts" "$DontQueue" "$EndSent" "$EndSentIg"
	 "$NumTrig" "$FirstIndent" "$FoldYear" "$FormWidth"
	 "$HushMode" "$IgnoreOnce" "$InfDelta" "$NextMode"
	 "$NumQueued" "$NumTrig" "$PrefixLineNo" "$PSCal" "$RunOff"
	 "$SimpleCal" "$SortByDate" "$SortByPrio" "$MinsFromUTC"
	 "$LatDeg" "$LatMin" "$LatSec" "$EndSent" "$EndSentIg"
	 "$Location" "$LongDeg" "$LongMin" "$LongSec" "$MaxSatIter"
	 "$SubsIndent" "$T" "$Td" "$Tm" "$Tw" "$Ty" "$TimeSep"
	 "$UntimedFirst" "$U" "$Ud" "$Um" "$Uw" "$Uy") t))

(defconst remind-key-datetime-names (regexp-opt '("Jan" "January"
	 "Feb" "Mar" "Apr" "Jun" "Jul" "Aug" "Sept" "Sep" "Oct" "Nov"
	 "Dec" "February" "March" "April" "May" "June" "July" "August"
	 "September" "October" "November" "December" "Mon" "Monday"
	 "Tue" "Tues" "Tuesday" "Wed" "Wednesday" "Thu" "Thursday"
	 "Thurs" "Fri" "Friday" "Saturday" "Sat" "Sun" "Sunday") t))

(defconst remind-key-msg (regexp-opt '("MSG" "MSF" "RUN" "CAL"
         "SPECIAL" "PS" "PSFILE") t))

(defconst remind-key-builtin-functions (regexp-opt '("abs" "access"
	 "args" "asc" "baseyr" "char" "choose" "coerce" "current"
	 "date" "datetime" "datepart" "dawn" "day" "daysinmon"
	 "defined" "dosubst" "dusk" "easter" "easterdate" "evaltrig"
	 "filedate" "filedatetime" "filedir" "filename" "getenv"
	 "hebdate" "hebday" "hebmon" "hebyear" "hour" "iif" "index"
	 "isdst" "isleap" "isomitted" "language" "lower" "max" "min"
	 "minute" "minsfromutc" "mon" "monnum" "moondate"
	 "moondatetime" "moonphase" "moontime" "msgprefix" "msgsuffix"
	 "nonomitted" "now" "ord" "ostype" "plural" "psmoon" "psshade"
	 "realcurrent" "realnow" "realtoday" "sgn" "shell" "slide"
	 "strlen" "substr" "sunrise" "sunset" "time" "timepart"
	 "thisyear" "today" "trigdate" "trigdatetime" "trigger"
	 "trigger" "trigtime" "trigvalid" "typeof" "tzconvert" "upper"
	 "value" "version" "weekno" "wkday" "wkdaynum" "year") t))

(defconst remind-font-lock-keywords-1
  (list
   `(,(lambda (limit)
       (let ((case-fold-search t))
	 (re-search-forward
	  (concat "\\<" remind-key-msg "\\> \\(.*\\)") limit t)))
     2 'font-lock-string-face) 		;highlight the string msg
   '("\\[\\(.*\\)\\]" 1 font-lock-negation-char-face t) ;remove highlight for expression
   `(,(lambda (limit)
       (let ((case-fold-search t))
	 (re-search-forward (concat "\\<" remind-key-keywords "\\>") limit t)))
     . font-lock-keyword-face)
   `(,(lambda (limit)
       (let ((case-fold-search t))
	 (re-search-forward (concat "\\<" remind-key-types "\\>") limit t)))
     . font-lock-type-face)
   `(,(concat "\\<" remind-key-variables "\\>")
     0 font-lock-variable-name-face t)
   `(,(lambda (limit)
       (let ((case-fold-search t))
	 (re-search-forward (concat "\\<" remind-key-datetime-names "\\>") limit t)))
     0 font-lock-constant-face t)
   `(,(concat "\\<" remind-key-builtin-functions "\\>")
     0 font-lock-function-name-face t)
   ;; time
   '("\\<\\(2[0-4]\\|[01]?[0-9][.:][0-5][0-9]\\)\\(\\(A\\|P\\)M\\)?\\>" 0 font-lock-constant-face t)
   ;; date
   '("\\<\\([12][09][0-9][0-9][-/]\\(0[1-9]\\|1[0-2]\\)[-/]\\([12][0-9]\\|0[1-9]\\|3[01]\\)\\)\\(@\\(2[0-4]\\|[01]?[0-9][.:][0-5][0-9]\\)\\)?\\>" 0 font-lock-constant-face t)
   '("\\<\\([0-9]+\\)\\>" 0 font-lock-constant-face t)
   )
  
  "Minimal highlighting expressions for Remind mode")

(defvar remind-font-lock-keywords remind-font-lock-keywords-1
  "Default highlighting expressions for Remind mode")

(define-derived-mode
  remind-mode
  prog-mode
  "Remind"
  "Major Mode to edit remind files"
  (modify-syntax-entry ?\; "< 1b" remind-mode-syntax-table)
  (modify-syntax-entry ?\# "< 1b" remind-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" remind-mode-syntax-table)
  (modify-syntax-entry ?_ "w" remind-mode-syntax-table)
  (modify-syntax-entry ?. "w" remind-mode-syntax-table)

  ;; this line didn't work idk why
  ;; (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  
  (set (make-local-variable 'font-lock-defaults) '(remind-font-lock-keywords))
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "\n")
  )


(provide 'remind-mode)
