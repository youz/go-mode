
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "go-mode")
  (require "http-client"))

(defpackage :goplay
  (:use :lisp :editor))

(in-package :goplay)

(defparameter *goplay-url* "http://localhost:3999/compile")

(defun popup-result (ok res)
  (let ((buf (get-buffer-create "*goplay*"))
	(km (make-sparse-keymap))
	(wc (current-window-configuration)))
    (setq res (substitute-string res "^<pre>" "")
	  res (substitute-string res "</pre>$" ""))
    (erase-buffer buf)
    (with-output-to-buffer (buf)
      (format t "[~:[error!~;ok~]]~%" ok)
      (format t "~A" res))
    (pop-to-buffer buf t t)
    (if ok
	(set-text-attribute 0 4 nil :foreground 4 :bold t)
      (set-text-attribute 0 8 nil :foreground 1 :bold t))
    (goto-char (point-max))
    (refresh-screen)
    (labels ((closebuf ()
	       (interactive)
	       (delete-buffer buf)
	       (set-window-configuration wc)))
      (define-key km #\q #'closebuf))
    (use-keymap km)))

(defun utf8encode (str)
  (let* ((ch (si:make-string-chunk
	      (convert-encoding-from-internal *encoding-utf8n* str)))
	 (size (si:chunk-size ch))
	 (buf (make-vector (1- size) :element-type 'character)))
    (dotimes (i (1- size))
      (setf (svref buf i) (code-char (si:unpack-uint8 ch i))))
    buf))

(defun run-region (from to)
  (interactive "r")
  (let ((src (buffer-substring from to)))
    (message "goplay: requesting...")
    (http-client:http-post
     *goplay-url* (utf8encode src)
     :oncomplete
     (lambda (res status headers requrl)
       (message "goplay: done.")
       (popup-result (= status 200) res)))))

(defun run-buffer ()
  (interactive)
  (run-region (point-min) (point-max)))

(define-key ed:*go-mode-map* '(#\C-c #\C-r) 'run-region)
(define-key ed:*go-mode-map* '(#\C-c #\C-c) 'run-buffer)

(provide "goplay")
