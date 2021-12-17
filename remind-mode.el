

;; keywords
;; (regexp-opt '("RUN" "REM" "ONCE" "SATISFY" "BEFORE" "UNSET" "OMIT" "FIRST" "FROM" "SATISFY"
;; 	 "OMIT" "DATE" "SKIP" "ONCE" "AFTER" "WARN" "PRIORITY" "AT" "SCHED" "IF" "ELSE" "ENDIF"
;; 	 "WARN" "UNTIL" "THROUGH" "SCANFROM" "DURATION" "TAG" "MSG" "MSF" "CAL" "SPECIAL" "IFTRIG"
;; 	 "PS" "PSFILE" "BANNER" "INCLUDE" "PUSH-OMIT-CONTEXT" "DEBUG" "DUMPVARS" "PUSH" "CLEAR" "POP"
;; 	 "CLEAR-OMIT-CONTEXT" "POP-OMIT-CONTEXT"  "SET" "ERRMSG" "FSET" "DUMP" "BAN" "INC" "SCAN"
;; 	 "EXIT" "FLUSH" "PRESERVE" "MOON" "COLOR" "UNSET" "ADDOMIT" "OMITFUNC") t)
;; (regexp-opt '("INT" "STRING" "TIME" "DATE" "SHADE" "DATETIME") t)
;; (regexp-opt '("$CalcUTC" "$CalMode" "$Daemon" "$DateSep" "$DefaultPrio" "$DontFork" "$DontTrigAts" "$DontQueue"
;; 	 "$EndSent" "$EndSentIg" "$NumTrig" "$FirstIndent" "$FoldYear" "$FormWidth" "$HushMode"
;; 	 "$IgnoreOnce" "$InfDelta" "$NextMode" "$NumQueued" "$NumTrig" "$PrefixLineNo" "$PSCal" "$RunOff"
;; 	 "$SimpleCal" "$SortByDate" "$SortByPrio" "$MinsFromUTC" "$LatDeg" "$LatMin" "$LatSec" "$EndSent"
;; 	 "$EndSentIg" "$Location" "$LongDeg" "$LongMin" "$LongSec" "$MaxSatIter" "$SubsIndent" "$T" "$Td"
;; 	 "$Tm" "$Tw" "$Ty" "$TimeSep" "$UntimedFirst" "$U" "$Ud" "$Um" "$Uw" "$Uy") t)
;; (regexp-opt '("Jan" "January" "Feb" "Mar" "Apr" "Jun" "Jul" "Aug" "Sept" "Sep" "Oct" "Nov" "Dec"
;; 	 "February" "March" "April" "May" "June" "July" "August" "September" "October"
;; 	 "November" "December" "Mon" "Monday" "Tue" "Tues" "Tuesday" "Wed" "Wednesday"
;; 	 "Thu" "Thursday" "Thurs" "Fri" "Friday" "Saturday" "Sat" "Sun" "Sunday") t)
;; (regexp-opt '("MSG" "MSF" "RUN" "CAL" "SPECIAL" "PS" "PSFILE") t)
;; (regexp-opt '("abs" "access" "args" "asc" "baseyr" "char" "choose" "coerce" "current" "date" "datetime" "datepart" 
;; 	 "dawn" "day" "daysinmon" "defined" "dosubst" "dusk" "easter" "easterdate" "evaltrig" "filedate" 
;; 	 "filedatetime" "filedir" "filename" "getenv" "hebdate" "hebday" "hebmon" "hebyear" "hour" "iif" "index" "isdst" 
;; 	 "isleap" "isomitted" "language" "lower" "max" "min" "minute" "minsfromutc" "mon" "monnum" "moondate" "moondatetime"
;; 	 "moonphase" "moontime" "msgprefix" "msgsuffix" "nonomitted" "now" "ord" "ostype" "plural" 
;; 	 "psmoon" "psshade" "realcurrent" "realnow" "realtoday" "sgn" "shell" "slide" "strlen" "substr" "sunrise" "sunset" "time" "timepart" 
;; 	 "thisyear" "today" "trigdate" "trigdatetime" "trigger" "trigger" "trigtime" "trigvalid" "typeof" "tzconvert" "upper" "value" 
;; 	 "version" "weekno" "wkday" "wkdaynum" "year") t)
;; (regexp-opt '() t)


(defconst remind-font-lock-keywords-1
  (list
   '("\\<\\(A\\(?:DDOMIT\\|FTER\\|T\\)\\|B\\(?:AN\\(?:NER\\)?\\|EFORE\\)\\|C\\(?:AL\\|LEAR\\(?:-OMIT-CONTEXT\\)?\\|OLOR\\)\\|D\\(?:ATE\\|EBUG\\|U\\(?:MP\\(?:VARS\\)?\\|RATION\\)\\)\\|E\\(?:LSE\\|NDIF\\|RRMSG\\|XIT\\)\\|F\\(?:IRST\\|LUSH\\|ROM\\|SET\\)\\|I\\(?:F\\(?:TRIG\\)?\\|NC\\(?:LUDE\\)?\\)\\|M\\(?:OON\\|S[FG]\\)\\|O\\(?:MIT\\(?:FUNC\\)?\\|NCE\\)\\|P\\(?:OP\\(?:-OMIT-CONTEXT\\)?\\|R\\(?:ESERVE\\|IORITY\\)\\|S\\(?:FILE\\)?\\|USH\\(?:-OMIT-CONTEXT\\)?\\)\\|R\\(?:EM\\|UN\\)\\|S\\(?:ATISFY\\|C\\(?:AN\\(?:FROM\\)?\\|HED\\)\\|ET\\|KIP\\|PECIAL\\)\\|T\\(?:AG\\|HROUGH\\)\\|UN\\(?:SET\\|TIL\\)\\|WARN\\)\\>"
     . font-lock-keyword-face)
   '("\\<\\(DATE\\(?:TIME\\)?\\|INT\\|S\\(?:HADE\\|TRING\\)\\|TIME\\)\\>"
     . font-lock-type-face)
   '("\\<\\(\\$\\(?:Cal\\(?:Mode\\|cUTC\\)\\|D\\(?:a\\(?:emon\\|teSep\\)\\|efaultPrio\\|ont\\(?:Fork\\|Queue\\|TrigAts\\)\\)\\|EndSent\\(?:Ig\\)?\\|F\\(?:irstIndent\\|o\\(?:ldYear\\|rmWidth\\)\\)\\|HushMode\\|I\\(?:gnoreOnce\\|nfDelta\\)\\|L\\(?:at\\(?:Deg\\|Min\\|Sec\\)\\|o\\(?:cation\\|ng\\(?:Deg\\|Min\\|Sec\\)\\)\\)\\|M\\(?:axSatIter\\|insFromUTC\\)\\|N\\(?:extMode\\|um\\(?:Queued\\|Trig\\)\\)\\|P\\(?:SCal\\|refixLineNo\\)\\|RunOff\\|S\\(?:impleCal\\|ortBy\\(?:Date\\|Prio\\)\\|ubsIndent\\)\\|T\\(?:imeSep\\|[dmwy]\\)\\|U\\(?:ntimedFirst\\|[dmwy]\\)\\|[TU]\\)\\)\\>"
     . (0 font-lock-variable-name-face t))
   '("\\<\\(A\\(?:pr\\(?:il\\)?\\|ug\\(?:ust\\)?\\)\\|Dec\\(?:ember\\)?\\|F\\(?:eb\\(?:ruary\\)?\\|ri\\(?:day\\)?\\)\\|J\\(?:an\\(?:uary\\)?\\|u\\(?:ly\\|ne\\|[ln]\\)\\)\\|M\\(?:a\\(?:rch\\|[ry]\\)\\|on\\(?:day\\)?\\)\\|Nov\\(?:ember\\)?\\|Oct\\(?:ober\\)?\\|S\\(?:at\\(?:urday\\)?\\|ep\\(?:t\\(?:ember\\)?\\)?\\|un\\(?:day\\)?\\)\\|T\\(?:hu\\(?:rs\\(?:day\\)?\\)?\\|ue\\(?:s\\(?:day\\)?\\)?\\)\\|Wed\\(?:nesday\\)?\\)\\>"
     . font-lock-constant-face)
   '("\\<\\(CAL\\|MS[FG]\\|PS\\(?:FILE\\)?\\|RUN\\|SPECIAL\\) \\(.*\\)\\>"
     . (2 'font-lock-string-face nil))
   ;; '("\\<\\(a\\(?:bs\\|ccess\\|rgs\\|sc\\)\\|baseyr\\|c\\(?:h\\(?:ar\\|oose\\)\\|oerce\\|urrent\\)\\|d\\(?:a\\(?:te\\(?:part\\|time\\)?\\|wn\\|y\\(?:sinmon\\)?\\)\\|efined\\|osubst\\|usk\\)\\|e\\(?:aster\\(?:date\\)?\\|valtrig\\)\\|file\\(?:d\\(?:ate\\(?:time\\)?\\|ir\\)\\|name\\)\\|getenv\\|h\\(?:eb\\(?:da\\(?:te\\|y\\)\\|mon\\|year\\)\\|our\\)\\|i\\(?:if\\|ndex\\|s\\(?:dst\\|leap\\|omitted\\)\\)\\|l\\(?:anguage\\|ower\\)\\|m\\(?:ax\\|in\\(?:sfromutc\\|ute\\)?\\|o\\(?:n\\(?:num\\)?\\|on\\(?:\\(?:dat\\(?:etim\\)?\\|phas\\|tim\\)e\\)\\)\\|sg\\(?:\\(?:pre\\|suf\\)fix\\)\\)\\|no\\(?:nomitted\\|w\\)\\|o\\(?:rd\\|stype\\)\\|p\\(?:lural\\|s\\(?:moon\\|shade\\)\\)\\|real\\(?:current\\|now\\|today\\)\\|s\\(?:gn\\|hell\\|lide\\|trlen\\|u\\(?:bstr\\|n\\(?:rise\\|set\\)\\)\\)\\|t\\(?:hisyear\\|ime\\(?:part\\)?\\|oday\\|rig\\(?:date\\(?:time\\)?\\|ger\\|time\\|valid\\)\\|ypeof\\|zconvert\\)\\|upper\\|v\\(?:alue\\|ersion\\)\\|w\\(?:eekno\\|kday\\(?:num\\)?\\)\\|year\\)\\>"
   ;;   . (0 font-lock-function-name-face t))
   ;; time
   '("\\<\\(2[0-4]\\|[01]?[0-9][.:][0-5][0-9]\\)\\(\\(A\\|P\\)M\\)?\\>" . font-lock-constant-face)
   ;; date
   '("\\<\\([12][09][0-9][0-9][-/]\\(0[1-9]\\|1[0-2]\\)[-/]\\([12][0-9]\\|0[1-9]\\|3[01]\\)\\)\\(@\\(2[0-4]\\|[01]?[0-9][.:][0-5][0-9]\\)\\)?\\>" . font-lock-constant-face)
   '("\\<\\([0-9]+\\)\\>" . font-lock-constant-face)
   ;; '("\\<\\(SET \\)\\([a-z_][a-z0-9_]*\\)\\>" . (2 font-lock-variable-name-face))
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
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  (set (make-local-variable 'font-lock-defaults) '(remind-font-lock-keywords))
  (set (make-local-variable 'comment-start) ";")
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "\n")
  )


(provide 'remind-mode)
