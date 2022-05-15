;; -*- lexical-binding: t; -*-

(require 'json)

(defvar qtoot-toot-width 56 "Character width of a toot")
(defvar qtoot-oauth-host "auth.qtoot.hugot.nl")
(defvar qtoot-oauth-port 443)

(defvar qtoot-enable-drafts nil
  "Whether or not qtoot should save drafts of your new toots.")

(defvar qtoot-drafts-category "Qtoot Drafts"
  "The category that qtoot should use to store notes under")

(defvar qtoot-drafts-host nil
  "The nextcloud host that qtoot should use to store drafts
  to. Drafts are stored in the nextcloud notes app.")

(defvar qtoot-token-alist '()
  "Alist containing oauth tokens for mastodon instances. The car
  of each element should be the hostname of an instance, and the
  cdr should be the access token for that instance.")

(defcustom qtoot-mastodon-hosts '()
  "List of mastodon host names")

(defvar-local qtoot-draft-idle-timer nil
  "Idle timer that saves drafts after a certain idle time. Buffer local.")

(defvar-local qtoot-draft-deleted nil
  "Whether or not the draft in the current buffer has been
  deleted from the server after being instructed to do so by the
  user.")

(defun qtoot--save-on-idle ()
  (let ((buffer (current-buffer))
        (timer))
    (setq timer
          (run-with-idle-timer
           1
           t
           (lambda ()
             (if (not (buffer-live-p buffer))
                 (cancel-timer timer)
               (with-current-buffer buffer
                 (when (and (not qtoot-draft-deleted) (buffer-modified-p buffer))
                   (qtoot-draft-save)
                   (set-buffer-modified-p nil)))))))
    (setq qtoot-draft-idle-timer timer)))

(define-minor-mode qtoot-mode "Minor mode to edit toots" nil "Qtoot"
  `((,(kbd "C-c C-c") . qtoot-toot)
    (,(kbd "C-x 3") . qtoot--split-window-right)
    (,(kbd "C-x C-s") . qtoot-draft-save))
  (if qtoot-mode
      (progn
        (add-hook (make-local-variable 'window-configuration-change-hook)
                  'qtoot--window-configuration-change-hook)
        (setq-local word-wrap t)
        (dolist (window (get-buffer-window-list (current-buffer)))
          (qtoot-style-window window))
        (if (require 'emojify nil t) (emojify-mode 1))
        (when qtoot-enable-drafts (qtoot--save-on-idle)))
    (progn
      (kill-local-variable 'window-configuration-change-hook)
      (kill-local-variable 'word-wrap)
      (dolist (window (get-buffer-window-list (current-buffer)))
        (set-window-margins window 0 0))
      (when qtoot-draft-idle-timer (cancel-timer qtoot-draft-idle-timer)))))

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

(defun qtoot--save-token (mastodon-host token)
  "Save oauth token with auth-source format in file"
  (secrets-create-item "default" (qtoot--secret-name mastodon-host) token))

(defun qtoot--secret-name (name)
  "Add prefix to make secret name recognizable"
  (concat "qtoot-" name))

(defun qtoot--get-token (mastodon-host)
  "Check qtoot-token-alist for a matching entry, otherwise
  return the token stored using the secrets API"
  (or (alist-get mastodon-host qtoot-token-alist nil nil 'string=)
      (secrets-get-secret "default" (qtoot--secret-name mastodon-host))))

(defun qtoot--save-token? (token)
  (yes-or-no-p
   (format (concat "Your access token is \"%s\", would you like to save this to your keyring?\n"
                   "Note: this depends on there being a keyring daemon available on your system.\n"
                   "If you are logged in on a headless server you will likely not have one available.\n"
                   "In that case pick no, the token will then be added to your kill-ring so you can set\n"
                   "`qtoot-token-alist` with it in your config. Use  C-h v qtoot-token-alist\n"
                   "for more information about this variable\n")
           token)))

(defsubst qtoot--json-serialize-utf8 (json)
  "Serialize a json object and encode the resulting string to UTF-8."
  (encode-coding-string (qtoot--json-preset (json-serialize json))
                        'utf-8 t))

(defmacro qtoot--json-preset (&rest body)
  "Define JSON preset to use when marshalling/unmarshalling json"

  `(let ((json-array-type 'list)
         (json-object-type 'alist)
         (json-key-type 'symbol))
     ,@body))

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
         (url-request-data (qtoot--json-serialize-utf8 json-data))
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

(defun qtoot--request-drafts-password ()
  (let* ((host (qtoot--get-drafts-host))
         (username (read-string (format "Username for nextcloud instance (%s): "
                                        host)))
         (password (read-string (format "Password for nextcloud instance (%s): "
                                       host))))
    (when (and username password)
      (let ((token (base64-encode-string (concat username ":" password))))
        (when (qtoot--save-token? token)
          (qtoot--save-token host token))

        token))))

(defun qtoot--get-drafts-password ()
  (or (alist-get (qtoot--get-drafts-host) qtoot-token-alist nil nil #'string=)
      (qtoot--get-token (qtoot--get-drafts-host))
      (qtoot--request-drafts-password)))

(defun qtoot--request-drafts-host ()
  (let ((drafts-host (read-string
                      (concat "Drafts can be saved to a nextcloud instance, "
                              "but I'd  need to know where your instance is! "
                              "please provide your nextcloud hostname: "))))
    (when drafts-host
      (setq qtoot-drafts-host drafts-host)
      (customize-set-variable 'qtoot-drafts-host drafts-host)
      (customize-save-variable 'qtoot-drafts-host drafts-host))
    drafts-host))

(defun qtoot-set-drafts-host ()
  (interactive)
  (qtoot--request-drafts-host)
  (qtoot--request-drafts-password))

(defun qtoot--get-drafts-host ()
  (if qtoot-drafts-host
      qtoot-drafts-host
    (qtoot--request-drafts-host)))

(defun qtoot--get-drafts-client ()
  (let ((password (qtoot--get-drafts-password))
        (host (qtoot--get-drafts-host)))
    (qtoot--make-draftc :password password
                        :host host)))


(cl-defstruct (qtoot--draftc
               (:constructor qtoot--make-draftc))
  "Qtoot drafts client. Can be used to communicate with a
nextcloud instance that has the notes app installed."
  (password nil
            :type string
            :documentation
            "Password to use to authenticate to nextcloud")
  (host nil
        :type string
        :documentation
        "Hostname that the nextcloud instance can be reached at"))

(cl-defmethod qtoot--draftc-endpoint ((client qtoot--draftc) route)
  (concat "https://" (qtoot--draftc-host client) route))

(cl-defmethod qtoot--draftc-url-headers ((client qtoot--draftc))
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(concat "Basic " (qtoot--draftc-password client)))))

(cl-defmethod qtoot--draftc-get-drafts ((client qtoot--draftc))
  (let* ((url-request-extra-headers (qtoot--draftc-url-headers client))
         (response (url-retrieve-synchronously
                    (qtoot--draftc-endpoint
                     client
                     (concat "/apps/notes/api/v1/notes?"
                             (url-build-query-string `((category ,qtoot-drafts-category))))))))
    (with-current-buffer (car (with-current-buffer response (mm-dissect-buffer t)))
      (goto-char (point-min))
      (condition-case err
          (qtoot--json-preset (json-read))
        (json-error (message "Error parsing json from buffer: %s" (buffer-string)))))))

(defun qtoot--set-draft-name (name)
  (setq qtoot-draft-name name)
  (rename-buffer (qtoot--draft-buffer-name name)))

(defun qtoot--draft-buffer-name (draft-name)
  (concat "[q] *draft* " draft-name))

(defun qtoot--handle-draft-saved (draft-name save-or-update)
  (let ((toot-buffer (current-buffer)))
    (lambda (status &rest args)
      (let ((status-code (url-http-parse-response)))
        (with-current-buffer (car (mm-dissect-buffer t))
          (goto-char (point-min))
          (condition-case err
              (progn
                (when (eq save-or-update 'save)
                  (let ((json (qtoot--json-preset (json-read))))
                    (with-current-buffer toot-buffer
                      (setq qtoot-draft-id (alist-get 'id json)))))

                (if (not (or (= 201 status-code) (= 200 status-code)))
                    (message "Something went wrong while saving draft. Status code: %s" status-code)
                  (progn (with-current-buffer toot-buffer (qtoot--set-draft-name draft-name))
                         (message "[qtoot] Draft saved."))))
            (json-error (message "[qtoot] Error parsing json from buffer: %s" (buffer-string)))))))))

(cl-defmethod qtoot--draftc-save-draft ((client qtoot--draftc) draft-name content)
  (let* ((url-request-extra-headers (qtoot--draftc-url-headers client))
         (url-request-method "POST")
         (url-request-data (qtoot--json-serialize-utf8
                             `((title . ,draft-name)
                               (content . ,content)
                               (category . ,qtoot-drafts-category)))))
    (url-retrieve (qtoot--draftc-endpoint client "/apps/notes/api/v1/notes")
                  (qtoot--handle-draft-saved draft-name 'save)
                  nil t)))

(cl-defmethod qtoot--draftc-update-draft ((client qtoot--draftc) draft-id draft-name content)
  (let* ((url-request-extra-headers (qtoot--draftc-url-headers client))
         (url-request-method "PUT")
         (url-request-data (qtoot--json-serialize-utf8
                             `((id . ,draft-id)
                               (title . ,draft-name)
                               (category . ,qtoot-drafts-category)
                               (content . ,content)))))
    (url-retrieve (qtoot--draftc-endpoint client (format "/apps/notes/api/v1/notes/%d" draft-id))
                  (qtoot--handle-draft-saved draft-name 'update)
                  nil t)))

(cl-defmethod qtoot--draftc-delete-draft ((client qtoot--draftc) draft-id)
  (let* ((url-request-extra-headers (qtoot--draftc-url-headers client))
         (url-request-method "DELETE"))
    (url-retrieve-synchronously
     (qtoot--draftc-endpoint client (format "/apps/notes/api/v1/notes/%d" draft-id))
     t)))


(defvar-local qtoot-draft-id nil
  "The id of the draft in the current buffer")

(defvar-local qtoot-draft-name nil
  "The name of the draft in the current buffer")

(defun qtoot-draft-save ()
  (interactive)
  (let ((client (qtoot--get-drafts-client))
        (content (buffer-string)))
  (if qtoot-draft-id
      (qtoot--draftc-update-draft client
                                  qtoot-draft-id
                                  qtoot-draft-name
                                  content)
    (qtoot--draftc-save-draft client
                              (format-time-string "%Y, %b %d %H:%M")
                              content)
    )
  (setq qtoot-draft-deleted nil)))

(defun qtoot-draft-set-name (name)
  (interactive (list (read-string "Draft name: ")))
  (setq qtoot-draft-name name)
  (qtoot-draft-save))

(defun qtoot--get-draft-alist ()
  (let* ((client (qtoot--get-drafts-client))
         (drafts (qtoot--draftc-get-drafts client))
         (alist))
    (dolist (draft drafts)
      (push `(,(alist-get 'title draft) . ,draft)
            alist))

    alist))

(defun qtoot-draft-open ()
  (interactive)
  (let* ((drafts (qtoot--get-draft-alist))
         (draft-name (completing-read "Open Draft: "
                                      (mapcar #'car drafts)
                                      nil t))
         (draft (alist-get draft-name drafts nil nil #'string=))
         (buffer (get-buffer-create (qtoot--draft-buffer-name draft-name))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (alist-get 'content draft))
      (setq qtoot-draft-id (alist-get 'id draft))
      (qtoot--set-draft-name draft-name))

    (switch-to-buffer buffer nil t)))

(defun qtoot-draft-delete ()
  (interactive)
  (when qtoot-draft-id
    (let ((client (qtoot--get-drafts-client)))
      (qtoot--draftc-delete-draft client qtoot-draft-id)
      (setq qtoot-draft-id nil)
      (setq qtoot-draft-name nil)
      (setq qtoot-draft-deleted t)
      (rename-buffer (generate-new-buffer-name "[q] DELETED")))))

;;;###autoload
(defun qtoot-add-host ()
  (interactive)
  (qtoot--get-oauth-token))

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
    (condition-case _
        (browse-url-xdg-open url)
      (t
       (kill-new url)
       (message "Please visit %s to authenticate qtoot, url added to kill-ring" url)))))

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

(defun qtoot--oauth-websocket (mastodon-host &optional callback)
  (qtoot--open-websocket
   qtoot-oauth-host
   qtoot-oauth-port
   (qtoot--oauth-on-message-closure mastodon-host callback)))

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

(provide 'qtoot)
