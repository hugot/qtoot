;; -*- lexical-binding: t; -*-

(defvar qtoot-toot-width 56 "Character width of a toot")
(defvar qtoot-oauth-host "auth.qtoot.libre-it.nl")
(defvar qtoot-oauth-port 443)

(defvar qtoot-oauth-token-alist '()
  "Alist containing oauth tokens for mastodon instances. The car
  of each element should be the hostname of an instance, and the
  cdr should be the access token for that instance.")

(defcustom qtoot-mastodon-hosts '()
  "List of mastodon host names")

(define-minor-mode qtoot-mode "Minor mode to edit toots" nil "Qtoot"
  `((,(kbd "C-c C-c") . qtoot-toot)
    (,(kbd "C-x 3") . qtoot--split-window-right))
  (if qtoot-mode
      (progn
        (add-hook (make-local-variable 'window-configuration-change-hook)
                  'qtoot--window-configuration-change-hook)
        (dolist (window (get-buffer-window-list (current-buffer)))
          (qtoot-style-window window)))
    (progn
      (dolist (window (get-buffer-window-list (current-buffer)))
        (set-window-margins window 0 0)))))

(defun qtoot--collapse-margins (window)
  (set-window-margins window 0 0))

(defun qtoot--split-window-right (&optional size)
  "Wraps `split-window-right` to still make it work with large
buffer margins"
  (interactive "P")
  (qtoot--collapse-margins (selected-window))
  (split-window-right size))

(defun qtoot--window-configuration-change-hook ()
  "Make sure the window keeps its styling when window sizes etc. change around"
  (if qtoot-mode
      (qtoot-style-window (selected-window))))

(defun qtoot--get-viewport-width (window)
  "Get body width + margin widths"
  (let ((margins (window-margins window)))
    (+ (window-body-width window)
       (or (car margins) 0)
       (or (cdr margins) 0))))

(defun qtoot--adjust-margins (window)
  "Adjust margins so that the window body is only as wide as
`qtoot-toot-width`"
  (let* ((width (qtoot--get-viewport-width window))
         (margin (floor (/ (- width qtoot-toot-width) 2))))
    (set-window-margins window margin margin)))

(defun qtoot-style-window (window)
  (qtoot--adjust-margins window))


(defun qtoot--open-websocket (host port on-message)
  (websocket-open
   (concat "wss://" host ":" (number-to-string port) "/ws")
   :on-message on-message
   :on-close (lambda (_websocket) (message "qtoot-oauth websocket closed"))))

(defun qtoot--oauth-fulfill (mastodon-host token &optional callback)
  "Handle oauth token fulfillment"
  (push mastodon-host qtoot-mastodon-hosts)
  (customize-set-variable 'qtoot-mastodon-hosts qtoot-mastodon-hosts)
  (customize-save-variable 'qtoot-mastodon-hosts qtoot-mastodon-hosts)

  (if (qtoot--save-token? token)
      (qtoot--save-token mastodon-host token))

  (if callback (funcall callback mastodon-host token)))

(defun qtoot--oauth-set-id (request-id)
  "Handle request id change by presenting the user with an
authentication page"
  (let ((url (format "https://%s:%d/auth/%s" qtoot-oauth-host qtoot-oauth-port request-id)))
    (unless (browse-url-xdg-open url)
      (kill-new url)
      (message "Please visit %s to authenticate qtoot, url added to kill-ring" url))))

(defun qtoot--oauth-on-message-closure (mastodon-host &optional callback)
  "Closure to handle websocket messages from the oauth server"
  (let (ID)
    (lambda (_websocket frame)
      (qtoot--json-preset
        (let* ((message (json-read-from-string (websocket-frame-text frame)))
               (message-type (alist-get 'type message)))
          (cond ((string= message-type "invalid-host")
                 (qtoot--oauth-request-token
                  _websocket
                  (read-string "Invalid host, please provide hostname without scheme (so no http://):")))

                ((string= message-type "set-id")
                 (setq ID (alist-get 'id (alist-get 'parameters message)))
                 (qtoot--oauth-set-id ID))

                ((string= message-type "fulfill")
                 (let ((token (alist-get 'token (alist-get 'parameters message))))
                   (qtoot--oauth-fulfill mastodon-host token callback)))
                
                (t (message "qtoot: Got unexpected message: %s" message))))))))

(defun qtoot--save-token (mastodon-host token)
  "Save oauth token with auth-source format in file"
  (secrets-create-item "default" (qtoot--secret-name mastodon-host) token))

(defun qtoot--secret-name (name)
  "Add prefix to make secret name recognizable"
  (concat "qtoot-" name))

(defun qtoot--get-token (mastodon-host)
  "Check qtoot-oauth-token-alist for a matching entry, otherwise
  return the token stored using the secrets API"
  (or (alist-get mastodon-host qtoot-oauth-token-alist nil nil 'string=)
      (secrets-get-secret "default" (qtoot--secret-name mastodon-host))))

(defun qtoot--save-token? (token)
  (yes-or-no-p
   (format (concat "Your access token is \"%s\", would you like to save this to your keyring?\n"
                   "Note: this depends on there being a keyring daemon available on your system.\n"
                   "If you are logged in on a headless server you will likely not have one available.\n"
                   "In that case pick no, the token will then be added to your kill-ring so you can set\n"
                   "`qtoot-oauth-token-alist` with it in your config. Use  C-h v qtoot-oauth-token-alist\n"
                   "for more information about this variable\n")
           token)))

(defun qtoot--oauth-websocket (mastodon-host &optional callback)
  (qtoot--open-websocket
   qtoot-oauth-host
   qtoot-oauth-port
   (qtoot--oauth-on-message-closure mastodon-host callback)))

(defmacro qtoot--json-preset (&rest body)
  "Define JSON preset to use when marshalling/unmarshalling json"

  (append '(let ((json-array-type 'list)
                 (json-object-type 'alist)
                 (json-key-type 'symbol)))
             body))

(defun qtoot--oauth-request-token (websocket mastodon-host)
  (qtoot--json-preset
    (let ((json (json-encode
                 `((type . "auth")
                   (parameters
                    . ((host . ,mastodon-host)))))))
      (websocket-send-text websocket json))))

(defun qtoot--get-oauth-token (&optional callback)
  (require 'websocket)

  (let* ((mastodon-host (read-string "Hostname of your mastodon instance :"))
         (websocket (qtoot--oauth-websocket mastodon-host callback)))

    (qtoot--oauth-request-token websocket mastodon-host)))

(defun qtoot--pick-mastodon-host ()
  "Pick a mastodon host from `qtoot-mastodon-hosts`"
  (cond ((= (length qtoot-mastodon-hosts) 1)
         (car qtoot-mastodon-hosts))
        ((not qtoot-mastodon-hosts) nil)
        (t (completing-read "Pick a mastodon host to post to :"
                            qtoot-mastodon-hosts))))

;;;###autoload
(defun qtoot ()
  (interactive)
  (switch-to-buffer (generate-new-buffer "Toot"))
  (qtoot-mode 1))

(defun qtoot--post-toot (mastodon-host mastodon-token json-data)
  "POST toot over HTTP"
  (let* ((url-request-method "POST")
         (url-request-data (encode-coding-string (qtoot--json-preset (json-encode json-data)) 'utf-8))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " mastodon-token))))
         (response (url-retrieve-synchronously (concat "https://" mastodon-host "/api/v1/statuses"))))

    (with-current-buffer (car (with-current-buffer response (mm-dissect-buffer t)))
      (goto-char (point-min))
      (let* ((response-json (qtoot--json-preset (json-read)))
             (err (alist-get 'error response-json)))
        (list response-json err)))))

(defun qtoot--toot-poster-closure (qtoot-buffer json-data)
  "Make a closure that posts the toot provided at creation to a
mastodon host that is provided at execution"
  (lambda (mastodon-host mastodon-token)
    (when (yes-or-no-p "Do you want to add a content warning?")
      (push `(spoiler_text . ,(read-string "Content Warning")) json-data))

    (pcase-let ((`(,response ,err) (qtoot--post-toot mastodon-host mastodon-token json-data)))
      (if err
          (message "Got error response from server: %s" err)
        (kill-buffer-ask qtoot-buffer)))))


;;;###autoload
(defun qtoot-toot (mastodon-host qtoot-buffer json-data)
  "Toot the current buffer"
  (interactive `(,(qtoot--pick-mastodon-host) ,(current-buffer) ((status . ,(buffer-string)))))
  (let ((toot-poster (qtoot--toot-poster-closure qtoot-buffer json-data))
        (mastodon-token (qtoot--get-token mastodon-host)))
    (cond (mastodon-token
           (funcall toot-poster mastodon-host mastodon-token))
          (t (qtoot--get-oauth-token toot-poster)))))

;;;###autoload
(defun qtoot-add-host ()
  (interactive)
  (qtoot--get-oauth-token))

(provide 'qtoot)
