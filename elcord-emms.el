(require 'elcord)
(require 'emms)

(defvar elcord-emms--musicbrainz-query-url-string
  "http://musicbrainz.org/ws/2/release-group/?query=release:%%22%s%%22%%20AND%%20artist:%%22%s%%22&limit=1")

(defvar elcord-emms--album-art-url-string
  "http://coverartarchive.org/release-group/%s/front-250")

(defvar elcord-emms--player-just-stopped-p nil)
(defvar elcord-emms--just-paused-p nil)
(defvar elcord-emms--last-known-title nil)
(defvar elcord-emms--last-known-artist nil)

(defvar elcord-emms-playing-format-str "🎶 %s")
(defvar elcord-emms-paused-format-str "⏸️ %s")
(defvar elcord-emms-error-format-str "elcord-emms error: %s")
(defvar elcord-emms-musicbrainz-release-group-id-regex "\(MusicBrainz Release Group Id\)\s")

(defun elcord-emms--musicbrainz-query-url (release artist)
  (format elcord-emms--musicbrainz-query-url-string
          (url-hexify-string release)
          (url-hexify-string artist)))

(defun elcord-emms--release-group-id-from-cache (name)
  (with-temp-buffer
    (when (zerop
           (let ((coding-system-for-read 'utf-8))
             (call-process
              "exiftool" nil t
              "-json" "-a" "-UserDefinedText" (expand-file-name name))))
      (goto-char (point-min))
      (when (ignore-errors
              (re-search-forward
               elcord-emms-musicbrainz-release-group-id-regex))
        (sexp-at-point)))))

(defun elcord-emms--album-art-url-from-cache (name)
  (if-let ((release-group-id
            (elcord-emms--release-group-id-from-cache name)))
      (format
       elcord-emms--album-art-url-string release-group-id)))

(defun elcord-emms--musicbrainz-query (release artist)
  (let* ((url (elcord-emms--musicbrainz-query-url release artist))
         (url-mime-accept-string "application/json")
         (musicbrainz-query-buffer
          (with-demoted-errors elcord-emms-error-format-str
            (url-retrieve-synchronously url :silent t))))
    (when musicbrainz-query-buffer
      (let ((musicbrainz-query
             (with-demoted-errors elcord-emms-error-format-str
               (with-current-buffer
                   musicbrainz-query-buffer
                 (goto-char url-http-end-of-headers)
                 (json-parse-buffer)))))
        (kill-buffer musicbrainz-query-buffer)
        musicbrainz-query))))

(defun elcord-emms--album-art-url-from-musicbrainz (release artist)
  (if-let* ((musicbrainz-query
             (elcord-emms--musicbrainz-query release artist))
            (release-groups
             (gethash "release-groups" musicbrainz-query))
            (first-release
             (ignore-errors (aref release-groups 0)))
            (id
             (gethash "id" first-release)))
      (format elcord-emms--album-art-url-from-musicbrainz-string id)))

(defun elcord-emms--title-from-track (track)
  (let ((info-title (alist-get 'info-title track))
        (type (alist-get 'type track))
        (name (alist-get 'name track)))
    (or info-title
        (when (and (eq 'file type) name)
          (file-name-nondirectory name))
        name)))

(defun elcord-emms--release-from-track (track)
  (or (alist-get 'info-album track)
      (elcord-emms--title-from-track track)))

(defun elcord-emms--album-art-url (track)
  (let* ((type (alist-get 'type track))
         (artist (alist-get 'info-artist track))
         (release (elcord-emms--release-from-track track))
         (name (alist-get 'name track)))
    (or (and (eq 'file type)
             name
             (elcord-emms--album-art-url-from-cache name))
        (elcord-emms--album-art-url-from-musicbrainz release artist))))

(defun elcord-emms--mode-icon-and-text (icon-and-text &rest args)
  (if-let* (emms-player-playing-p
            (track
             (emms-playlist-current-selected-track))
            (release
             (elcord-emms--release-from-track track))
            (url
             (elcord-emms--album-art-url track)))
      (list (cons "large_text" release) (cons "large_image" url))
    (apply icon-and-text args)))

(defun elcord-emms--mode-icon-and-text-with-demoted-errors (icon-and-text &rest args)
  (with-demoted-errors elcord-emms-error-format-str
      (apply 'elcord-emms--mode-icon-and-text (cons icon-and-text args))))

(defun elcord-emms--end-time (duration-time playing-time)
  (let* ((duration-plus-now-time (time-add duration-time (current-time)))
        (end-time (time-subtract duration-plus-now-time playing-time)))
    end-time))

(defun elcord-emms--details-and-state (details-and-state &rest args)
  (if-let* (emms-player-playing-p
            (track (emms-playlist-current-selected-track))
            (artist (alist-get 'info-artist track))
            (title (elcord-emms--title-from-track track))
            (duration-seconds (alist-get 'info-playing-time track))
            (duration-time (seconds-to-time duration-seconds))
            (playing-time (seconds-to-time emms-playing-time))
            (end-time (elcord-emms--end-time duration-time playing-time))
            (end-time-str (string-to-number (format-time-string "%s" end-time)))
            (state (format "by %s" artist))
            (details-fstr (if emms-player-paused-p
                              elcord-emms-paused-format-str
                            elcord-emms-playing-format-str))
            (details (format details-fstr title)))
      (let ((timestamps (unless emms-player-paused-p
                          (list (cons "end" end-time-str))))
            (result (list (cons "details" details)
                          (cons "state" state))))
        (if timestamps
            (cons (cons "timestamps" timestamps) result)
          result))
    (apply details-and-state args)))

(defun elcord-emms--details-and-state-with-demoted-errors (details-and-state &rest args)
  (with-demoted-errors elcord-emms-error-format-str
      (apply 'elcord-emms--details-and-state (cons details-and-state args))))

(defun elcord-emms--update-presence (elcord-update &rest args)
  (let* ((track (emms-playlist-current-selected-track))
         (title (elcord-emms--title-from-track track))
         (artist (alist-get 'info-artist track)))
    (if (and emms-player-playing-p
             (or (not (eq elcord-emms--last-known-title title))
                 (not (eq elcord-emms--last-known-artist artist))
                 elcord-emms--just-paused-p))
        (progn
          (setq elcord-emms--last-known-title title
                elcord-emms--last-known-artist artist
                elcord-emms--just-paused-p nil)
          (elcord--try-update-presence "" -1))
      (progn
        (when elcord-emms--player-just-stopped-p
          (setq elcord--last-known-position -1
                elcord--last-known-buffer-name ""
                elcord-emms--player-just-stopped-p nil))
        (apply elcord-update args)))))

(defun elcord-emms--set-player-just-stopped-p ()
  (setq elcord-emms--player-just-stopped-p t))

(defun elcord-emms--set-just-paused-p ()
  (setq elcord-emms--just-paused-p t))

(defun elcord-emms-init ()
  (interactive)
  (advice-add 'elcord--mode-icon-and-text :around #'elcord-emms--mode-icon-and-text-with-demoted-errors)
  (advice-add 'elcord--details-and-state :around #'elcord-emms--details-and-state-with-demoted-errors)
  (advice-add 'elcord--update-presence :around #'elcord-emms--update-presence)
  (add-hook 'emms-player-finished-hook #'elcord--update-presence)
  (add-hook 'emms-playlist-selection-changed-hook #'elcord--update-presence)
  (add-hook 'emms-player-paused-hook #'elcord--update-presence)
  (add-hook 'emms-player-started-hook #'elcord--update-presence)
  (add-hook 'emms-player-stopped-hook #'elcord--update-presence)
  (add-hook 'emms-player-paused-hook #'elcord-emms--set-just-paused-p)
  (add-hook 'emms-player-stopped-hook #'elcord-emms--set-player-just-stopped-p)
  (add-hook 'emms-player-started-hook #'elcord-emms--set-player-just-stopped-p)
  (elcord--update-presence))

(defun elcord-emms-terminate ()
  (interactive)
  (advice-remove 'elcord--mode-icon-and-text #'elcord-emms--mode-icon-and-text-with-demoted-errors)
  (advice-remove 'elcord--details-and-state #'elcord-emms--details-and-state-with-demoted-errors)
  (advice-remove 'elcord--update-presence #'elcord-emms--update-presence)
  (remove-hook 'emms-player-stopped-hook #'elcord-emms--set-player-just-stopped-p)
  (remove-hook 'emms-player-started-hook #'elcord-emms--set-player-just-stopped-p)
  (remove-hook 'emms-player-paused-hook #'elcord-emms--set-just-paused-p)
  (remove-hook 'emms-player-finished-hook #'elcord--update-presence)
  (remove-hook 'emms-playlist-selection-changed-hook #'elcord--update-presence)
  (remove-hook 'emms-player-paused-hook #'elcord--update-presence)
  (remove-hook 'emms-player-started-hook #'elcord--update-presence)
  (remove-hook 'emms-player-stopped-hook #'elcord--update-presence)
  (elcord--update-presence))

(provide 'elcord-emms-init)
(provide 'elcord-emms-terminate)
