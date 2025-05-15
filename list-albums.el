;;; list-albums.el --- List music albums by duration in a table -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Kisaragi Hiu
;;
;; Author: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Maintainer: Kisaragi Hiu <mail@kisaragi-hiu.com>
;; Created: 2025-05-15
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/kisaragi-hiu/list-albums
;; Package-Requires: ((emacs "29.3") (f "0.21.0") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  List albums in a table by duration.
;;  Extracted from my config.
;;
;;; Code:

(require 'f)
(require 'seq)
(require 'map)
(require 'dash)
(require 'json)
(require 'tabulated-list)

(defgroup list-albums nil
  "List music albums by duration."
  :group 'multimedia
  :prefix "list-albums-")

(defcustom list-albums-cache-file (expand-file-name "folder-durations.json" user-emacs-directory)
  "Path to albums cache JSON file. This file can also define extra albums."
  :group 'list-albums
  :type 'file)

;; Listing albums by album duration
;; Documented in internal note 2021-08-03T02:02:08+0900 and https://kisaragi-hiu.com/sort-albums-by-duration
(defun list-albums--song-duration (song-file)
  "Return duration of SONG-FILE in seconds."
  (with-temp-buffer
    (call-process
     "ffprobe" nil '(t nil) nil
     "-v" "quiet"
     "-print_format" "json"
     "-show_streams"
     song-file)
    (goto-char (point-min))
    (-some--> (json-parse-buffer
               :object-type 'alist)
      (map-elt it 'streams)
      (seq-find (lambda (elem)
                  (equal (map-elt elem 'codec_type)
                         "audio"))
                it)
      (map-elt it 'duration)
      string-to-number)))

(defun list-albums--folder-duration (folder)
  "Return duration of all songs in FOLDER."
  (let* ((cache
          (with-temp-buffer
            (insert-file-contents list-albums-cache-file)
            (json-parse-buffer)))
         (name (f-filename folder))
         (update-cache nil)
         value)
    (setq value (if-let ((cached (map-elt cache name)))
                    cached
                  (setq update-cache t)
                  (--> (directory-files folder t)
                       (mapcar #'list-albums--song-duration it)
                       -non-nil
                       (apply #'+ it))))
    (when update-cache
      (map-put! cache name value)
      (let ((json-encoding-pretty-print t))
        (with-temp-file list-albums-cache-file
          (insert (json-encode cache)))))
    value))

;;;###autoload
(defun list-albums-add-to-cache (name seconds)
  "Add an entry saying NAME is SECONDS long to the cache."
  (interactive
   (list (read-string "Name: ")
         (read-number "Seconds: ")))
  (unless (and (numberp seconds)
               (>= seconds 0))
    (error "Invalid value for SECONDS, must be a number that is >= 0"))
  (let* ((cache
          (with-temp-buffer
            (insert-file-contents list-albums-cache-file)
            (json-parse-buffer))))
    (map-put! cache name seconds)
    (let ((json-encoding-pretty-print t))
      (with-temp-file list-albums-cache-file
        (insert (json-encode cache))))))

;;;###autoload
(defun list-albums (dir)
  "List music folders in DIR, providing a duration field for sort."
  (interactive (list (xdg-user-dir "MUSIC")))
  (let (folders)
    (dolist-with-progress-reporter (folder (f-directories dir))
        (format "Probing folders in %s..." dir)
      ;; populate cache
      (list-albums--folder-duration folder))
    (setq folders (with-temp-buffer
                    (insert-file-contents list-albums-cache-file)
                    (let ((json-key-type 'string))
                      (json-read))))
    (setq folders (--filter (/= 0 (cdr it)) folders))
    (with-current-buffer (pop-to-buffer
                          (get-buffer-create "*k/albums*"))
      (when (= 0 (buffer-size))
        (tabulated-list-mode)
        (setq-local revert-buffer-function (lambda (&rest _) (k/list-albums dir)))
        (setq tabulated-list-format
              (vector
               '("folder" 70 t)
               (list "duration" 20
                     (lambda (a b)
                       ;; An entry is (ID ["<folder>" "<duration>"]).
                       ;;
                       ;; <duration> looks like (label :key val :key val...)
                       ;; when props are given.
                       (< (-> (cadr a) (elt 1) cdr (plist-get :seconds))
                          (-> (cadr b) (elt 1) cdr (plist-get :seconds)))))))
        (tabulated-list-init-header))
      (setq tabulated-list-entries nil)
      (dolist (folder folders)
        (push (list nil (vector (f-filename (car folder))
                                (list (format-seconds "%.2h:%.2m:%.2s" (cdr folder))
                                      :seconds (cdr folder))))
              tabulated-list-entries))
      (tabulated-list-revert))))

(provide 'list-albums)
;;; list-albums.el ends here
