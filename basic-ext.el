;;-*- lexical-binding: t; -*-

(require 'basic-mode)

(defmacro basic-ext-add-to-list-local (list-var element)
  (cl-check-type list-var symbol)
  (cl-ext-unless (local-variable-if-set-p list-var)
    (error "Invalid symbol %S: Must be a buffer-local variable" list-var))
  (let* ((value (symbol-value list-var)))
    (cl-ext-unless (cl-typep value 'list)
      (error "Invalid form %S: must be a list" value))
    `(let ((newvalue (quote ,value)))
       (cl-pushnew ,element newvalue)
       (setq ,list-var newvalue))))

;; (local-variable-if-set-p 'basic-keywords (get-buffer "code.bas"))

;; (cl-prettyexpand '(basic-ext-add-to-list-local basic-keywords "with"))

;;;###autoload
(defun basic-generic-mode--extra-hook ()
  t)

;;;###autoload
(add-hook 'basic-generic-mode-mode-hook #'basic-generic-mode--extra-hook)

;; ---Major more for Libreoffice dialect of Basic

;;;###autoload
(define-derived-mode basic-libreoffice-mode basic-mode "Basic[Libreoffice]"
  "Programming mode for Libreoffice Basic.
Derived from `basic-mode'."
  (setq basic-functions '("Abs" "Array" "Asc" "Atn" "Blue" "CBool" "CByte"
			  "CCur" "CDateFromUnoDateTime" "CDateToUnoDateTime"
			  "CDateFromUnoDate" "CDateToUnoDate" "CDateToUnoIso"
			  "CDateFromUnoIso" "CDate" "CDbi" "CDec" "CInt"
			  "CLng" "CompatibilityMode" "CSng" "CStr" "Choose"
			  "Chr" "ChrW" "ConvertFromURL" "ConvertToURL" "Cos"
			  "CreateObject" "CreateUnoDialog"
			  "CreateUnoListener" "CreateUnoService"
			  "CreateUnoStruct" "CreateUnoValue" "CurDir" "CVar"
			  "CVErr" "Date" "DateAdd" "DateDiff" "DatePart"
			  "DateSerial" "DateValue" "Day" "DimArray" "Dir"
			  "Environ" "Eof" "EqualUnoObjects" "Erl" "Err"
			  "Error" "Exp" "FileAttr" "FileDateTime"
			  "FileExists" "FileLen" "FindObject"
			  "FindPropertyObject" "Fix" "Format"
			  "FormatDateTime" "Frac" "FreeFile" "FreeLibrary"
			  "GetAttr" "GetDefaultContext" "GetGUIType"
			  "GetProcessServiceManager" "GetPathSeparator"
			  "GetSolarVersion" "GetSystemTicks" "Green"
			  "HasUnoInterfaces" "Hex" "Hour" "IIf" "InStr"
			  "Input" "InputBox" "Int" "IsArray" "IsDate"
			  "IsEmpty" "IsError" "IsMissing" "IsNull"
			  "IsNumeric" "IsObject" "IsUnoStruct" "Join"
			  "LBound" "LCase" "LTrim" "Left" "Len" "Loc" "Lof"
			  "Log" "Mid" "Minute" "Month" "MonthName" "MsgBox"
			  "Now" "Oct" "QBColor" "Red" "Replace" "RGB" "Right"
			  "Rnd" "RTrim" "Second" "Seek" "Sgn" "Shell" "Sin"
			  "Space" "Spc" "Split" "Sqr" "StrComp" "Str"
			  "String" "Switch" "Tan" "TimeSerial" "TimeValue"
			  "Time" "Timer" "Trim" "TwipsPerPixelX"
			  "TwipsPerPixelY" "TypeName" "VarType" "UBound"
			  "UCase" "Val" "WeekDay" "Year"

			  ;; Operators
			  "And" "Eqv" "Imp" "Is" "Mod" "New" "Not" "Or" "Xor"
			  "=" "<" ">" "<=" ">=" "<>")
	basic-builtins '("Beep" "Case" "Call" "ChDir" "ChDrive" "Close"
			 "Declare" "DefBool" "DefCur" "DefDate" "DefDbl"
			 "DefErr" "DefInt" "DefLng" "DefObj" "DefSng" "DefStr"
			 "DefVar" "Dim" "Erase" "Exit" "False" "FileCopy" "Get"
			 "GoSub" "Return" "Input#" "Kill" "LSet" "Let" "Line
			 Input #" "MkDir" "MsgBox" "Name" "On Error" "GoTo"
			 "Open" "Option Base" "Option ClassModule" "Option
			 Compatible" "Option Explicit" "VBASupport" "Print"
			 "Property" "Put" "Randomize" "ReDim" "Rem" "Reset"
			 "Resume" "Reset" "RmDir" "RSet" "Seek" "SetAttr" "Set"
			 "Stop" "Sub" "Type" "True" "Wait" "WaitUntil" "Write")
	basic-keywords '("As" "Case" "Const" "Do" "Else" "ElseIf" "End" "For Each" "For"
			 "Function" "Global" "If" "Loop" "Optional" "Public"
			 "Select" "Static" "Sub" "Then" "Until" "Wend" "While" "With")
	basic-types '("Boolean" "Currency" "Date" "Double" "Integer" "Long" "Object"
		      "Single" "String" "Variant")
	basic-increase-indent-keywords-bol '("Case" "For" "For Each" "Function"
					     "If" "Sub" "While" "With")
	basic-increase-indent-keywords-eol '("Else" "Then")
	basic-decrease-indent-keywords-bol '("Case" "Else" "ElseIf" "End" "Loop"
					     "Next" "Until" "Wend")
	comment-start "REM")

  ;; Treat . and # as part of identifier ("input #" etc)
  (modify-syntax-entry ?. "w   " basic-libreoffice-mode-syntax-table)
  (modify-syntax-entry ?# "w   " basic-libreoffice-mode-syntax-table)
  
  (basic-mode-initialize))

;; ---

(provide 'basic-ext)
;;; basic-ext.el ends here
