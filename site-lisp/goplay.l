
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

(defun run-region (from to)
  (interactive "r")
  (let ((src (buffer-substring from to)))
    (message "goplay: requesting...")
    (http-client:http-post *goplay-url*
			   (convert-encoding-from-internal ed:*encoding-utf8n* src)
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
