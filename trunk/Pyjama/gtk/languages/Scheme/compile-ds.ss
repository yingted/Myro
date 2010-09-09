(load "ds-transformer.ss")
(delete-file "pjscheme-ds.ss")
(ds-transform-file "pjscheme-cps.ss" "pjscheme-ds.ss")
(exit)
