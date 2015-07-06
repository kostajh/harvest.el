;;; harvest.el --- Harvest integration with Emacs, via Helm

;; Copyright (C) 2015  Kosta Harlan

;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; Keywords: helm harvest
;;; Code:

(require 'url)
(require 'json)
(require 'helm)

(defun harvest-init ()
  "Initialize harvest integration. Stores basic auth credentials and subdomain"
  (interactive)
  (let ((harvest-subdomain (read-string "Enter the subdomain: "))
        (harvest-username (read-string "Enter your username: "))
        (harvest-password (read-string "Enter your password: "))
        )
    (setq harvest-auth-hash (make-hash-table :test 'equal))
    (puthash "subdomain" harvest-subdomain harvest-auth-hash)
    (puthash "auth" (concat "Basic " (base64-encode-string (concat (symbol-value 'harvest-username) ":" (symbol-value 'harvest-password)))) harvest-auth-hash)
    (unless (file-exists-p "~/.emacs.d/.harvest")
      (mkdir "~/.emacs.d/.harvest"))
    (delete-file "~/.emacs.d/.harvest/auth.el")
    (create-file-buffer "~/.emacs.d/harvest/auth.el")
    (let (print-length print-level)
      (write-region (prin1-to-string harvest-auth-hash) nil "~/.emacs.d/.harvest/auth.el" harvest-auth-hash)
      (message "Credentials stored in '~/.emacs.d/.harvest/auth.el'")
      ))
  ;; Refresh entries.
  (message "Retrieving data from Harvest")
  (harvest-refresh-entries)
  )

(defun harvest-get-credentials()
  "Load credentials from the auth.el file"
  (if (file-exists-p "~/.emacs.d/.harvest/auth.el")
      (progn
        (with-temp-buffer
          (insert-file-contents "~/.emacs.d/.harvest/auth.el")
          (read (buffer-string))))
    (message "No file exists at ~/.emacs.d/harvest/auth.el. Try running harvest-init()")))

;;;###autoload
(defun harvest-clock-in ()
  "Start a timer for an entry in Harvest"
  (interactive)
  (harvest-refresh-entries)
  (helm :sources '(((name . "Day Entries")
                    (candidates . harvest-day-entries-search)
                    (action-transformer . harvest-actions-for-entry))
                   ((name . "Tasks")
                    (candidates . harvest-task-search)
                    (action-transformer . harvest-actions-for-task-entry))
                   )
        :prompt "Project/Task: "
        :buffer "*harvest*" :candidate-number-limit 100))

(defun harvest-refresh-entries()
  "Refresh the local cache of day entries and projects/tasks. N.B. this is called before harvest-clock-in, so you usually don't need to run this yourself."
  (interactive)
  (let (
        (harvest-auth (harvest-get-credentials))
        (url-request-method "GET")
        (url-set-mime-charset-string)
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string "application/json")
        (url-personal-mail-address nil)
        )
    (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) ".harvestapp.com/daily"))
    (setq url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(gethash "auth" harvest-auth))))
    (with-current-buffer (url-retrieve-synchronously request-url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (delete-region (point-min) (point))
      (write-file "~/.emacs.d/.harvest/daily.json")
      (json-read)
      )))

(defun harvest-search-daily-entries (search-term)
  "Get day entries from the daily.json file."
  (mapcar (lambda (entry)
            (cons (harvest-format-entry entry) entry))
          (alist-get '(day_entries) (harvest-get-cached-daily-entries))))

(defun harvest-search-tasks (search-term)
  "Get project entries from the daily.json file"
  (mapcar (lambda (entry)
            (cons (harvest-format-project-entry entry) entry))
          (alist-get '(projects) (harvest-get-cached-daily-entries))))

(defun harvest-search-project-tasks (search-term)
  "Get available tasks for a particular project"
  (mapcar (lambda (entry)
            (cons (harvest-format-project-task-entry entry) entry))
          (alist-get '(tasks) (entry))))

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in LIST."
  (if symbols
      (alist-get (cdr symbols)
                 (assoc (car symbols) alist))
    (cdr alist)))

(defun harvest-format-entry (entry)
  "Format a task as a string. Format is TASK - PROJECT:CLIENT"
  (if (alist-get '(timer_started_at) entry)
      (setq active-indicator "*** ")
    (setq active-indicator ""))
  (concat active-indicator
          (alist-get '(task) entry)
          " - "
          (alist-get '(project) entry)
          ":"
          (alist-get '(client) entry)
          ))

(defun harvest-format-project-entry (entry)
  "Show available projects and clients to clock in for"
  (concat (alist-get '(client) entry) " - " (alist-get '(name) entry))
  )

(defun harvest-day-entries-search ()
  (harvest-search-daily-entries helm-pattern))

(defun harvest-task-search ()
  (harvest-search-tasks helm-pattern))

(defun harvest-project-task-search ()
  (harvest-search-daily-entries helm-pattern))

(defun harvest-get-cached-daily-entries ()
  (json-read-from-string
   (with-temp-buffer
     (insert-file-contents "~/.emacs.d/.harvest/daily.json")
     (buffer-string))))

(defun harvest-actions-for-entry (actions entry)
  "Return a list of helm ACTIONS available for this ENTRY."
  `((,(format "Toggle timer for - %s in %s for %s"
              (alist-get '(task) entry)
              (alist-get '(project) entry)
              (alist-get '(client) entry)) . harvest-toggle-timer-for-entry)
    (,(format "Edit notes - %s in %s for %s"
              (alist-get '(task) entry)
              (alist-get '(project) entry)
              (alist-get '(client) entry)) . harvest-edit-description)
    ("Show entry metadata" . pp)))

(defun harvest-actions-for-task-entry (actions entry)
  "Return a list of helm ACTIONS available for this ENTRY."
  `((,(format "Clock in for - %s for %s"
              (alist-get '(name) entry)
              (alist-get '(client) entry)) . harvest-clock-in-task-entry)
    ("Show entry metadata" . pp)))


(defun harvest-edit-description (entry)
  (setq harvest-updated-description (read-string "Notes: " (alist-get '(notes) entry)))
  (let (
        (harvest-auth (harvest-get-credentials))
        (url-request-method "POST")
        (url-set-mime-charset-string)
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string "application/json")
        (url-personal-mail-address nil)
        )
    (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/daily/update/%s" (alist-get '(id) entry))))
    (print request-url)
    (setq url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(gethash "auth" harvest-auth))))
    (with-current-buffer (url-retrieve-synchronously request-url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (delete-region (point-min) (point))
      (json-read)
      (message (format "Updated notes for task %s in %s for %s" (alist-get '(task) entry) (alist-get '(project) entry) (alist-get '(client) entry)))
      )))

(defun harvest-clock-out ()
  ;; Clocks out of any active timer.
  (mapcar (lambda (entry)
            (if (alist-get '(timer_started_at) entry)
                (let (
                      (harvest-auth (harvest-get-credentials))
                      (url-request-method "GET")
                      (url-set-mime-charset-string)
                      (url-mime-language-string nil)
                      (url-mime-encoding-string nil)
                      (url-mime-accept-string "application/json")
                      (url-personal-mail-address nil)
                      )
                  (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/daily/timer/%s" (alist-get '(id) entry))))
                  (setq url-request-extra-headers
                        `(("Content-Type" . "application/json")
                          ("Authorization" . ,(gethash "auth" harvest-auth))))
                  (url-retrieve-synchronously request-url t)
                  (message (format "Clocked out of %s in %s - %s" (alist-get '(task) entry) (alist-get '(project) entry) (alist-get '(client) entry)))
                  )))
          (alist-get '(day_entries) (harvest-refresh-entries))))

(defun harvest-clock-in-task-entry (entry)
  (helm :sources '((name . "Tasks")
                   (candidates . (lambda ()
                                   (mapcar (lambda (task)
                                             (cons
                                              (alist-get '(name) task)
                                              (format "%d:%d" (alist-get '(id) entry) (alist-get '(id) task))))
                                           (alist-get '(tasks) entry))))
                   (action-transformer . harvest-clock-in-project-task-entry))
        :prompt "Task: "
        :buffer "*harvest*"
        :candidate-number-limit 999))

(defun harvest-clock-in-project-task-entry (entry task)
  ;; Start a new timer for a task on a project. Entry is actually not populated,
  ;; which is why we need to split "task" on the colon to retrieve project and
  ;; task info.
  (setq harvest-payload (make-hash-table :test 'equal))
  (puthash "project_id" (car (s-split ":" task)) harvest-payload)
  (puthash "task_id" (car (cdr (s-split ":" task))) harvest-payload)
  (puthash "notes" (read-string "Notes: ") harvest-payload)
  
  (let (
        (harvest-auth (harvest-get-credentials))
        (url-request-method "POST")
        (url-set-mime-charset-string)
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string "application/json")
        (url-personal-mail-address nil)
        (url-request-data (json-encode harvest-payload))
        )
    (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/daily/add")))
    (setq url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(gethash "auth" harvest-auth))))
    (with-current-buffer (url-retrieve-synchronously request-url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (delete-region (point-min) (point))
      (json-read)
      )))

(defun harvest-toggle-timer-for-entry (entry)
  "Clock in or out of a given entry."
  (setq prompt (format "Are you sure you want to clock in for %s?" (harvest-format-entry entry)))
  (setq clock-message (format "Clocked in for %s" (harvest-format-entry entry)))
  (if (assoc 'timer_started_at entry)
      (progn
        (setq prompt (format "Are you sure you want to clock out of %s?" (harvest-format-entry entry)))
        (setq clock-message (format "Clocked out of %s" (harvest-format-entry entry)))
        )
    )
  (if (yes-or-no-p prompt)
      (let (
            (harvest-auth (harvest-get-credentials))
            (url-request-method "GET")
            (url-set-mime-charset-string)
            (url-mime-language-string nil)
            (url-mime-encoding-string nil)
            (url-mime-accept-string "application/json")
            (url-personal-mail-address nil)
            )
        (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/daily/timer/%s" (alist-get '(id) entry))))
        (setq url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(gethash "auth" harvest-auth))))
        (with-current-buffer (url-retrieve-synchronously request-url)
          (goto-char (point-min))
          (search-forward "\n\n" nil t)
          (delete-region (point-min) (point))
          (json-read)
          (message clock-message)
          ))))

(provide 'harvest)

;;; harvest.el ends here
