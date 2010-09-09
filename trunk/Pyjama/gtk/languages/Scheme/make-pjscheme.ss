(load "scheme-to-csharp.ss")
(delete-file "pjscheme.cs")
(convert-file "pjscheme-rm.ss" "pjscheme.cs")
(exit)
