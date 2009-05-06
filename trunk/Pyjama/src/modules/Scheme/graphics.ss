;;(load-assembly-from "Graphics" "Graphics.dll")

;; (load-assembly "Graphics")
;; ;;;;(load-assembly "gtk-sharp")
;; ;;;;(load-assembly "gdk-sharp")
;; ;;;;(load-assembly "gnome-sharp")

(load-assembly-from "gtk-sharp" "/opt/mono-1.9.1/lib/mono/gtk-sharp-2.0/gtk-sharp.dll")
(load-assembly-from "gdk-sharp" "/opt/mono-1.9.1/lib/mono/gtk-sharp-2.0/gdk-sharp.dll")
(load-assembly-from "gnome-sharp" "/opt/mono-1.9.1/lib/mono/gtk-sharp-2.0/gnome-sharp.dll")
(import "Graphics" "Graphics.dll")


(using "Gtk")

(call-static 'Gtk.Application 'Init)

;; (define Graphics (new 'Graphics))
;; (define Point (lambda args (call-static-prim Graphics 'makePoint args)))
;; (define Line (lambda args (call-static-prim Graphics 'makeLine args)))
;; (define GraphWin (lambda args (call-static-prim Graphics 'makeGraphWin args)))
;; (define Pixel (lambda args (call-static-prim Graphics 'makePixel args)))
;; (define Pixmap (lambda args (call-static-prim Graphics 'makePixmap args)))
;; (define Image (lambda args (call-static-prim Graphics 'makeImage args)))
;; (call line 'draw win)
;; (call-static Graphics 'show)

(define win (GraphWin))
(define win2 (GraphWin))
(define line (Line (Point 0 0) (Point 100 100)))


