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
;; Listing albums by album duration.
;; Documented in internal note 2021-08-03T02:02:08+0900 and
;; https://kisaragi-hiu.com/sort-albums-by-duration
;;
;; Since no music player that I've tried supports this, I had to implement this
;; myself. It's been useful from time to time, so I've made it a package.
;;
;;; Code:

(require 'dash)
(require 'f)
(require 'json)
(require 'map)
(require 'seq)
(require 'tabulated-list)
(require 'xdg)
(require 'cl-lib)

(require 'eww)

(defun list-albums--mark-metadata (collection metadata)
  "Mark COLLECTION as having completiong metadata METADATA.
METADATA should be an alist to specify all sorts of completion metadata,
such as `category' or `display-sort-function'. (Unfortunately there
doesn\\='t seem to be an index of them in the manual.)"
  (lambda (str pred action)
    (pcase action
      ('metadata
       `(metadata . ,metadata))
      (_
       (all-completions str collection pred)))))

(defun list-albums--sort-releases-numerically (candidates)
  "Sort release CANDIDATES with their prefix numbers."
  (let* ((cache (make-hash-table :test #'equal))
         (key (lambda (thing)
                (or (gethash thing cache)
                    (with-temp-buffer
                      (insert thing)
                      (goto-char (point-min))
                      ;; skip through the prefix JSON object, which we are using to
                      ;; store the ID right in the string...
                      (json-parse-buffer)
                      (puthash thing
                               (string-to-number
                                (buffer-substring (point) (point-max)))
                               cache))))))
    (sort candidates
          (lambda (a b)
            ;; This is what the numeric sort in `org-sort' does too
            (> (funcall key a)
               (funcall key b))))))

(defun list-albums--face (thing face)
  "Return THING as a string with FACE as its face property."
  (propertize (format "%s" thing) 'face face))

(defgroup list-albums nil
  "List music albums by duration."
  :group 'multimedia
  :prefix "list-albums-")

(defcustom list-albums-cache-file (expand-file-name "folder-durations.json" user-emacs-directory)
  "Path to albums cache JSON file. This file can also define extra albums."
  :group 'list-albums
  :type 'file)

(defcustom list-albums-music-dir (xdg-user-dir "MUSIC")
  "Main directory of music albums."
  :group 'list-albums
  :type 'directory)

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

(defun list-albums--read-cache ()
  "Read the cache JSON file."
  ;; Initialize cache file if it doesn't exist
  (unless (file-exists-p list-albums-cache-file)
    (with-temp-file list-albums-cache-file
      (insert "{}")))
  (with-temp-buffer
    (insert-file-contents list-albums-cache-file)
    (json-parse-buffer)))

(defun list-albums--folder-duration (folder)
  "Return duration of all songs in FOLDER."
  (let* ((cache (list-albums--read-cache))
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

(defvar url-request-extra-headers)
(defun list-albums--fetch-json-sync (url)
  "Fetch from URL using the right headers, then parse the return as JSON."
  (let ((cache-buffer-name (format "k/tmp:%s" url))
        (url-request-extra-headers
         '(("User-Agent" . "KisaragiHiuListAlbumsEl/0.0.1(https://kisaragi-hiu.com)")
           ("Accept" . "application/json"))))
    (with-current-buffer (or (get-buffer cache-buffer-name)
                             (url-retrieve-synchronously url :silent))
      (unless (equal cache-buffer-name (buffer-name))
        (clone-buffer cache-buffer-name))
      (goto-char (point-min))
      (eww-parse-headers)
      (decode-coding-region (point) (point-max) 'utf-8)
      (json-parse-buffer :array-type 'list :object-type 'alist))))

(defun list-albums--read-release (prompt releases)
  "Choose a release from RELEASES and return some of its metadata.
PROMPT is shown to the user.
The returned data includes ID and ARTIST-CREDIT."
  (let ((sort-releases (lambda (candidates)
                         (let* ((cache (make-hash-table :test #'equal))
                                (key (lambda (thing)
                                       (or (gethash thing cache)
                                           (with-temp-buffer
                                             (insert thing)
                                             (goto-char (point-min))
                                             ;; skip through the prefix JSON object, which we are using to
                                             ;; store the ID right in the string...
                                             (json-parse-buffer)
                                             (puthash thing
                                                      (string-to-number
                                                       (buffer-substring (point) (point-max)))
                                                      cache))))))
                           (sort candidates
                                 (lambda (a b)
                                   ;; This is what the numeric sort in `org-sort' does too
                                   (> (funcall key a)
                                      (funcall key b)))))))
        (collection (cl-loop
                     for release in releases
                     collect (let-alist release
                               (format "%s%s\t%s %s(%s tracks%s)"
                                       (propertize (json-encode
                                                    `((id . ,.id)
                                                      (artist-credit . ,.artist-credit)))
                                                   'invisible t)
                                       (-> (format "(score: %s)" .score)
                                           (list-albums--face 'font-lock-comment-face))
                                       (-> .title
                                           (list-albums--face 'font-lock-property-name-face))
                                       (--> (cdr (assq 'name (elt .artist-credit 0)))
                                            (list-albums--face it 'font-lock-string-face)
                                            (format (pcase (length .artist-credit)
                                                      (0 "")
                                                      (1 "by %s ")
                                                      (_ "by %s and others "))
                                                    it))
                                       .track-count
                                       (format (if .country ", %s" "")
                                               (list-albums--face .country 'font-lock-string-face)))))))
    (-some-> (completing-read prompt
                              (list-albums--mark-metadata
                               collection
                               `((display-sort-function . ,sort-releases)))
                              nil t)
      json-read-from-string)))

;;;###autoload
(defun list-albums-lookup-album (title-query)
  "Look up an album with TITLE-QUERY from MusicBrainz."
  (interactive
   (list (read-string "Title query: " nil 'list-albums-lookup-album)))
  (let ((title nil)
        (artist nil)
        (id nil)
        ;; MusicBrainz uses milliseconds
        (duration-ms 0))
    (message "Searching for releases matching %S..." title-query)
    (let ((res (list-albums--fetch-json-sync
                (format "https://musicbrainz.org/ws/2/release?%s"
                        (url-build-query-string
                         `((query ,title-query)
                           (limit 10)
                           (offset 0))))))
          (props nil))
      (unless res (error "Unable to get a response for the query"))
      (setq props (list-albums--read-release "Select release: "
                                             (alist-get 'releases res)))
      (unless props (error "Unable to select a release"))
      (let-alist props
        (setq id .id)
        ;; Don't bother setting artist if there is more than one
        (when (= 1 (length .artist-credit))
          (setq artist
                (->> (elt .artist-credit 0)
                     (alist-get 'name))))))
    (message "Looking up release %S..." id)
    (let ((res (list-albums--fetch-json-sync
                (format "https://musicbrainz.org/ws/2/release/%s?%s"
                        id
                        (url-build-query-string
                         `((inc "recordings")))))))
      (unless res (error "Unable to get a response for the release"))
      (let-alist res
        (setq title .title)
        (dolist (track (->> (elt .media 0)
                            (alist-get 'tracks)))
          (cl-incf duration-ms (let-alist track .length)))))
    (unless (and title duration-ms)
      (error "Title or duration is missing"))
    (list-albums-add-to-cache (if artist
                                  (format "%s - %s" artist title)
                                title)
                              (/ duration-ms 1000.0))))

;;;###autoload
(defun list-albums-add-to-cache (name seconds)
  "Add an entry saying NAME is SECONDS long to the cache."
  (interactive
   (list (read-string "Name: ")
         (read-number "Seconds: ")))
  (unless (and (numberp seconds)
               (>= seconds 0))
    (error "Invalid value for SECONDS, must be a number that is >= 0"))
  (let* ((cache (list-albums--read-cache)))
    (map-put! cache name seconds)
    (let ((json-encoding-pretty-print t))
      (with-temp-file list-albums-cache-file
        (insert (json-encode cache))))))

;;;###autoload
(defun list-albums (dir)
  "List music folders in DIR, providing a duration field for sort."
  (interactive (list list-albums-music-dir))
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
        (setq-local revert-buffer-function (lambda (&rest _) (list-albums dir)))
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
