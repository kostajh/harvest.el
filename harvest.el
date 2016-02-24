;;; harvest.el --- Harvest integration with Emacs
;;
;; Copyright (C) 2016  Kosta Harlan

;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; Maintainer: Kosta Harlan <kosta@kostaharlan.net>
;; Homepage: https://github.com/kostajh/harvest.el
;; Keywords: harvest
;; Package-Requires: ((swiper))
;;
;; This file is not part of GNU Emacs.
;;; Code:

(require 'url)
(require 'json)
(require 'ivy)

(defvar hydra-harvest nil)
(defvar hydra-harvest-day-entry nil)
(defvar harvest-selected-entry nil)

(defhydra hydra-harvest ()
  "harvest"
  ("v" (harvest-search-daily-entries) "view day entries" :color blue)
  ("n" (harvest-create-new-entry) "new entry")
  ("o" (harvest-clock-out) "clock out" :color pink)
  ("r" (harvest-refresh-entries) "refresh entries")
  ("q" nil "quit"))

(defhydra hydra-harvest-day-entry ()
  "day entry"
  ("e" (harvest-edit-description harvest-selected-entry) "edit description")
  ("t" (harvest-toggle-timer-for-entry harvest-selected-entry) "toggle timer")
  ("h" (message "not yet implemented") "edit hours")
  ("q" hydra-harvest/body "quit" :exit t))

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
(defun harvest ()
  "Start the Harvest hydra."
  (interactive)
  (hydra-harvest/body))

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
      (if (file-exists-p "~/.emacs.d/.harvest/daily.json")
          (delete-file "~/.emacs.d/.harvest/daily.json"))
      (create-file-buffer "~/.emacs.d/harvest/daily.json")
      (write-file "~/.emacs.d/.harvest/daily.json")
      (json-read)
      )))


(defun harvest-search-daily-entries ()
  "Get day entries from the daily.json file."
  ;; TODO: Do this asynchronously, and earlier.
  (harvest-refresh-entries)
  (ivy-read "Day entries: "
            (mapcar (lambda (entry)
                      (cons (harvest-format-entry entry) entry))
                    (alist-get '(day_entries) (harvest-get-cached-daily-entries)))
            :action (lambda (x)
                      (setq harvest-selected-entry x)
                      (hydra-harvest-day-entry/body)))
  )

(defun harvest-create-new-entry ()
  "Create a new entry for a particular project and task."
  (ivy-read "Project: "
            (mapcar (lambda (entry)
                      (cons (harvest-format-project-entry entry) entry))
                    (alist-get '(projects) (harvest-get-cached-daily-entries)))
            :action (lambda (x)
                      (setq harvest-selected-entry x)
                      (ivy-read "Task: "
                                (harvest-get-tasks-for-project harvest-selected-entry)
                                :action (lambda (selection)
                                          (harvest-clock-in-project-task-entry nil selection))))
            ))

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in LIST."
  (if symbols
      (alist-get (cdr symbols)
                 (assoc (car symbols) alist))
    (cdr alist)))

(defun harvest-format-entry (entry)
  "Format a task as a string. Format is PROJECT (CLIENT) \n TASK - NOTES"
  (setq formatted-string (concat
                          (alist-get '(project) entry)
                          " ("
                          (alist-get '(client) entry)
                          ")"
                          ": "
                          (alist-get '(task) entry)
                          " - "
                          (alist-get '(notes) entry)
                          "\t["
                          (number-to-string (alist-get '(hours) entry))
                          "]"
                          ))
  (if (alist-get '(timer_started_at) entry)
      (propertize formatted-string 'face 'bold)
    (propertize formatted-string 'face 'nil)))

(defun harvest-format-project-entry (entry)
  "Show available projects and clients to clock in for"
  (concat (alist-get '(name) entry) " (" (alist-get '(client) entry) ")")
  )

(defun harvest-get-cached-daily-entries ()
  (json-read-from-string
   (with-temp-buffer
     (insert-file-contents "~/.emacs.d/.harvest/daily.json")
     (buffer-string))))

(defun harvest-edit-description (entry)
  "Edit the description for a Harvest day entry."
  ;; TODO Refactor HTTP code.
  (setq harvest-payload (make-hash-table :test 'equal))
  ;; TODO Not ideal to overwrite hours in Harvest, but unless we do it,
  ;; it gets reset to 0.
  (puthash "hours" (alist-get '(hours) entry) harvest-payload)
  (puthash "project_id" (alist-get '(project_id) entry) harvest-payload)
  (puthash "task_id" (alist-get '(task_id) entry) harvest-payload)
  (puthash "notes" (read-string "Notes: " (alist-get '(notes) entry)) harvest-payload)
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
    (setq request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/daily/update/%s" (alist-get '(id) entry))))
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

;;;###autoload
(defun harvest-clock-out ()
  "Clocks out of any active timer."
  ;; TODO Refactor HTTP code.
  (interactive)
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

(defun harvest-get-tasks-for-project (project)
  (mapcar (lambda (task)
            (cons
             (alist-get '(name) task)
             (format "%d:%d" (alist-get '(id) project) (alist-get '(id) task))))
          (alist-get '(tasks) project)))

(defun harvest-clock-in-project-task-entry (entry task)
  "Start a new timer for a task on a project. Entry is actually not populated,
  which is why we need to split "task" on the colon to retrieve project and
  task info."
  ;; TODO: Refactor HTTP code.
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
  ;; TODO Refactor HTTP code.
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

;;;###autoload
(add-hook 'org-clock-in-hook 'harvest)
;;;###autoload
(add-hook 'org-clock-out-hook 'harvest-clock-out)

(provide 'harvest)

;;; harvest.el ends here
