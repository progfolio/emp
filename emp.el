;;; emp.el --- Simple MPV interface library  -*- lexical-binding: t; -*-

;;; Commentary:
;;
(eval-when-compile (require 'subr-x))

;;; Code:
(dolist (executable '("socat"))
  (unless (executable-find executable)
    (user-error "Executable %S not found on PATH" executable)))

(defgroup emp nil
  "Simple MPV interface."
  :prefix "emp-"
  :group 'external)

(defvar emp--players (make-hash-table :test 'equal) "Table of mpv processes.")
(defvar emp--player-count 0 "Internal counter to keep track of proccesses.")
(defvar emp--selected-players nil "The current MPV context.")

(defcustom emp-start-poll-timeout 5
  "Freshly strated MPV processes may not recieve commands immediately.
`emp-start' will wait to return by repeatedly sending a test command until MPV
responds. This limit prevents infinite loops if MPV is not responding for
another reason. It is the maximum of seconds we will poll before giving up and
throwing an error."
  :type (or 'interger 'float))

(defcustom emp-video-directory "~/Videos"
  "Default directory to search for videos when using `emp-open-file'."
  :type 'directory)

(defun emp--ensure-list (obj)
  "If OBJ is not a list of players, make it one.
This is to deal with a single player.
Also used to with differences between `completing-read-multiple' and `completing-read' results."
  (cond
   ((stringp obj) (list obj))
   ((keywordp (car obj)) (list obj))
   (t obj)))

(defun emp--start (&optional name socket)
  "Start an IPC enabled MPV process named NAME.
If NAME is nil, autogenerate a numeric one.
If SOCKET is provided, use that socket file instead of creating a new one."
  (let* ((name (or name (format "emp%d" (setq emp--player-count (1+ emp--player-count)))))
         (socket (or socket (make-temp-file (format "%s-" name) nil ".emp")))
         (process (make-process :name name
                                :command (list
                                          "mpv"
                                          "--idle=yes"
                                          "--keep-open=yes"
                                          "--no-terminal"
                                          ;;maybe not
                                          "--force-window"
                                          (format "--input-ipc-server=%s" socket))
                                :noquery t))
         (player (list :socket socket :process process :name name)))
    (with-timeout (emp-start-poll-timeout (error "Unable to start MPV"))
      (while (null (car (emp-send-command (list player) "emp-test")))
        (sleep-for 0.1)))
    (list (puthash name player emp--players))))

(defun emp--parse-time-string (time-string)
  "Convert TIME-STRING into list of form:
\\(milliseconds seconds minutes hours)."
  (let ((components
         (nreverse
          (flatten-tree
           (mapcar (lambda (component) (split-string component "\\."))
                   (split-string time-string ":"))))))
    (setq components
          (if (string-match "\\(?:\\.\\([[:digit:]]+\\)\\)" time-string)
              (push (* (string-to-number (concat "0." (match-string 1 time-string)))
                       1000)
                    (cdr components))
            (push 0 components)))
    (unless (= (seq-reduce (lambda (acc char)
                            (+ acc (if (string= char ":") 1 0)))
                          (split-string time-string "" 'omit-nulls)
                          0)
               2)
      (setq components (append components '(0))))
    (mapcar (lambda (component) (if (stringp component)
                                    (string-to-number component)
                                  component))
            components)))

(defun emp--time-string-to-ms (time)
  "Convert TIME to ms."
  (let ((places '(1 1000 60000 3600000))
        (index 0))
    (truncate
     (apply #'+
            (mapcar
             (lambda (unit)
               (prog1 (* unit (nth index places)) (cl-incf index)))
             (emp--parse-time-string time))))))

(defun emp--compact-time-formatter (h m s ms)
  "Return shortest time string from H M S MS."
  (concat
   (cond ((> h 0) (format "%d:%02d:%02d" h m s))
         ((> m 0) (format "%d:%02d" m s))
         (t (format "%d" s)))
   (when (> ms 0) (format ".%03d" ms))))

(defun emp--format-ms (n)
  "Format N milliseconds with `speedo-time-formatter'.
If FORMATTER is non-nil, use that format function instead.
It is called with hours, minutes, seconds, milliseconds."
  (let* ((milliseconds (mod n 1000))
         (n (/ n 1000))
         (seconds (mod n 60))
         (minutes (mod (/ n 60) 60))
         ;; Don't use mod here because we don't care about
         ;; diving any farther than "hours"
         ;; using mod to check would truncate the hours
         ;; in cases where hours % 60 = 0
         (hours (/ n  (* 60 60))))
    (emp--compact-time-formatter hours minutes seconds milliseconds)))

(defun emp--update-player-metadata (players)
  "Update PLAYERS metadata."
  (dolist (player players)
    (let ((name (plist-get player :name)))
      (puthash name
               (plist-put player
                          :title
                          (plist-get (car (emp-send-command (list player) "get_property" "media-title"))
                                     :data))
               emp--players)
      (puthash name
               (plist-put player
                          :playback-time
                          (ignore-errors
                            (emp--format-ms
                             (* 1000
                                (plist-get (car (emp-send-command (list player)
                                                                  "get_property" "playback-time"))
                                           :data)))))
               emp--players))))

(defun emp--player-selection-info (player)
  "Return a string with metadata info for a PLAYER."
  (string-join (list (plist-get player :name)
                     (plist-get player :title)
                     (plist-get player :playback-time))
               " "))

(defun emp--select-players (&optional prompt)
  "PROMPT for players if more than one, else return player list.
If `emp--selected-players' is non-nil, return that player list instead."
  (emp--update-player-metadata (emp-players 'all))
  (cond
   ((zerop (hash-table-count emp--players)) (user-error "No MPV process running"))
   (emp--selected-players emp--selected-players)
   ((= (hash-table-count emp--players) 1)
    (hash-table-values emp--players))
   (t (emp--ensure-list
       (mapcar (lambda (key) (get-text-property 0 'emp-player key))
               (emp--ensure-list
                (funcall (if current-prefix-arg
                             #'completing-read-multiple
                           #'completing-read)
                         (or prompt "Select Player: ")
                         (mapcar (lambda (player)
                                   (propertize (emp--player-selection-info player)
                                               'emp-player player))
                                 (hash-table-values emp--players))
                         nil 'require-match)))))))

(defun emp-set-context ()
  "Set the MPV process context.
Commands that usually prompt when multiple processes are running
will only use the current context process."
  (interactive)
  (setq emp--selected-players
        (let ((emp--selected-players nil))
          (emp--select-players "MPV context: "))))

(defun emp-kill (players)
  "Kill PLAYERS.
Prompt if PLAYERS is nil and more than one process is running."
  (interactive (list (emp--select-players "Kill MPV process(es): ")))
  (dolist (player (emp--ensure-list players))
    (ignore-errors
      (kill-process (plist-get player :process)))
    (when-let ((socket (plist-get player :socket)))
      (when (file-exists-p socket) (delete-file socket)))
    (when (eq (car emp--selected-players) player)
      (setq emp--selected-players nil))
    (remhash (plist-get player :name) emp--players)))

(defun emp-send-command (players &rest command)
  "Send PLAYERS COMMAND."
  (unless (and (listp players) (not (null players))) (signal 'wrong-type-argument `(listp ,players)))
  (mapcar (lambda (player)
            (when-let ((socket (plist-get player :socket))
                       (message (concat (json-serialize (list :command (apply #'vector command)))
                                        "\n"))
                       (result (with-temp-buffer
                                 (call-process-region message nil "socat" nil t t "-" socket)
                                 (buffer-string))))
              ;; if the process is unresponsive, return nil
              (unless (string-empty-p result)
                (if (string-match-p "Connection Refused" result)
                    (let ((socket (plist-get player :socket)))
                      (delete-file socket)
                      (apply #'emp-send-command (emp--start (plist-get player :name)) command))
                  (json-parse-string result
                                     :object-type 'plist
                                     :array-type 'list)))))
          players))

(defun emp-get-property (property)
  "Return PROPERTY data for PLAYERS."
  (emp-send-command (emp-players) "get_property" property))

(defun emp-set-property (property val)
  "Set PROPERTY to VAL for seledcted players."
  (emp-send-command (emp-players) "set_property" property val))

(defun emp-start (&optional name)
  "Start an MPV process named NAME.
If NAME is nil, automatically generate process name."
  (interactive "MName: ")
  (emp--start name))

(defun emp-players (&optional all)
  "Return the list of currently selected players.
If ALL is non-nil, return all players.
If only one player is started, return a list containing that.
If more than one player is started, but none is selected, prompt the user
For the players.
  Note the results are always contained in a list even if one player is returned."
  (if all
      (hash-table-values emp--players)
    (condition-case _
        (emp--select-players "Select Players: ")
      ((user-error) nil))))

;;;###autoload
(defun emp-open-file (&optional file)
  "Play FILE with currently selected players.
If called interactively, prompt for one relative to `emp-video-directory'.
When called with \\[universal-argument], prompt relative to `default-directory'.
If no players are started, start one and use that."
  (interactive (list (read-file-name "Play media: "
                                     (ignore-errors (file-name-as-directory
                                                     (unless current-prefix-arg emp-video-directory))))))
  (emp-send-command (or (emp-players) (emp--start))
                    "loadfile" (expand-file-name file) "append-play"))
(defun emp-pause ()
  "Cycle pause for currently selected players."
  (interactive)
  (emp-send-command (emp-players) "osd-msg-bar" "cycle" "pause"))

(defun emp-playlist-next ()
  "Got to next entry in the selected players' playlists."
  (interactive)
  (emp-send-command (emp-players) "osd-msg-bar" "playlist-next"))

(defun emp-playlist-prev ()
  "Got to next entry in the selected players' playlists."
  (interactive)
  (emp-send-command (emp-players) "osd-msg-bar" "playlist-prev"))

(defun emp-frame-step ()
  "Play one frame, then pause. Does nothing with audio-only playback."
  (interactive)
  (emp-send-command (emp-players) "osd-msg-bar" "frame-step"))

(defun emp-frame-back-step ()
  "Go back one frame, then pause. Does nothing with audio-only playback."
  (interactive)
  (emp-send-command (emp-players) "osd-msg-bar" "frame-back-step"))

(defun emp--read-playlist ()
  "Select player's playlist entry."
  (let* ((count -1)
         (entries (mapcar (lambda (data) (cons (plist-get data :filename) (setq count (1+ count))))
                          (plist-get (car (emp-get-property "playlist")) :data)))
         (selection (completing-read "Playlist entry: " entries nil 'require-match)))
    (when selection (alist-get selection entries nil nil 'equal))))

(defun emp-playlist-play (&optional index)
  "Start (or restart) playback of given playlist INDEX.
Indices are 0 indexed and may optionally be any of the following:
- `current` : the current playlist entry.
- `none` : Stop playback."
  (interactive (list (emp--read-playlist)))
  (emp-send-command (emp-players) "playlist-play-index" (number-to-string (truncate index))))

(defun emp-seek (time &optional _flags)
  "Seek to TIME.
When called from elisp FLAGS may be:
- absolute
- relative"
  (interactive "p")
  (let ((players (emp-players)))
    (unless players (user-error "No players selected"))
    (emp-send-command players "osd-msg-bar" "seek" (number-to-string (truncate time)) "relative+exact")))

(defun emp-playback-time ()
  "Return current playback time."
  (let ((time (plist-get (car (emp-get-property "playback-time")) :data)))
    (emp--format-ms (truncate (* 1000 time)))))

(defun emp-insert-playback-time ()
  "Insert current playback time."
  (interactive)
  (insert (emp-playback-time)))

(defun emp-speed-set (factor)
  "Set playback speed to FACTOR.
If called interactively with \\[universal-argument] reset speed to 1."
  (interactive (list (if current-prefix-arg 1 (read-number "Factor: "))))
  (emp-set-property "speed" (abs factor)))

(defun emp-seek-absolute (time)
  "Seek to absolute TIME."
  (interactive "MTime: ")
  (emp-send-command (emp-players) "osd-msg-bar" "seek"
                    (/ (emp--time-string-to-ms time) 1000) "absolute"))

(provide 'emp)

;;; emp.el ends here
