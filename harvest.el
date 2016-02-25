;;; harvest.el --- Harvest integration with Emacs
;;
;; Copyright (C) 2016  Kosta Harlan

;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; Maintainer: Kosta Harlan <kosta@kostaharlan.net>
;; Homepage: https://github.com/kostajh/harvest.el
;; Keywords: harvest
;; Package-Requires: ((swiper "0.7.0") (hydra "0.13.0") (s "1.11.0"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;; harvest.el is an Emacs package for integrating with the Harvest time
;; tracking service (getharvest.com).  The package supports creating and
;; modifying time entries for the current day.  To begin, run
;; M-x harvest-authenticate, and then M-x harvest to get started with
;; logging time.

;;; Code:

(require 'url)
(require 'json)
(require 'ivy)
(require 'hydra)
(require 's)

(defvar hydra-harvest nil)
(defvar harvest-cached-daily-entries nil)
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

(defun harvest-authenticate()
  "Authenticate with Harvest. Stores basic auth credentials and subdomain"
  (interactive)
  (let ((harvest-auth-hash (make-hash-table :test 'equal)))
    (puthash "subdomain" (read-string "Enter the subdomain (e.g.'example' for a site like 'example.harvestapp.com'): ") harvest-auth-hash)
    (puthash "auth" (concat "Basic " (base64-encode-string (concat (read-string "Enter your username: ") ":" (read-passwd "Enter your password: ")))) harvest-auth-hash)
    (unless (file-exists-p "~/.emacs.d/.harvest")
      (mkdir "~/.emacs.d/.harvest"))
    (if (file-exists-p "~/.emacs.d/.harvest/auth.el")
        (delete-file "~/.emacs.d/.harvest/auth.el"))
    (create-file-buffer "~/.emacs.d/harvest/auth.el")
    (let (print-length print-level)
      (write-region (prin1-to-string harvest-auth-hash) nil "~/.emacs.d/.harvest/auth.el" harvest-auth-hash)
      (message "Credentials stored in '~/.emacs.d/.harvest/auth.el'")
      ))
  (message "Retrieving data from Harvest")
  (harvest-refresh-entries)
  (hydra-harvest/body))

(defun harvest-get-credentials()
  "Load credentials from the auth.el file"
  (if (file-exists-p "~/.emacs.d/.harvest/auth.el")
      (progn
        (with-temp-buffer
          (insert-file-contents "~/.emacs.d/.harvest/auth.el")
          (read (buffer-string))))
    (message "No file exists at ~/.emacs.d/harvest/auth.el. Try running harvest-authenticate()")))

;;;###autoload
(defun harvest ()
  "Start the main Harvest hydra."
  (interactive)
  (hydra-harvest/body))

(defun harvest-refresh-entries()
  "Refresh the local cache of day entries and projects/tasks. N.B. this is called before harvest-clock-in, so you usually don't need to run this yourself."
  (interactive)
  (setq harvest-cached-daily-entries (harvest-api "GET" "daily" nil "Refreshed cache of daily entries")))

(defun harvest-search-daily-entries ()
  "Ivy interface to search through day entries."
  ;; TODO: Do this asynchronously, and earlier.
  ;; TODO: Sort by most recent entry.
  (harvest-refresh-entries)
  (ivy-read "Day entries: "
            (mapcar (lambda (entry)
                      (cons (harvest-format-entry entry) entry))
                    ;; TODO: Use cache data here.
                    (alist-get '(day_entries) (harvest-refresh-entries)))
            :action (lambda (x)
                      (setq harvest-selected-entry x)
                      (hydra-harvest-day-entry/body)))
  )

(defun harvest-create-new-entry ()
  "Create a new entry for a particular project and task."
  ;; TODO: Sort by name.
  (ivy-read "Project: "
            (mapcar (lambda (entry)
                      (cons (harvest-format-project-entry entry) entry))
                    ;; TODO: Use cache data here.
                    (alist-get '(projects) (harvest-refresh-entries)))
            :action (lambda (x)
                      (setq harvest-selected-entry x)
                      (ivy-read "Task: "
                                (harvest-get-tasks-for-project harvest-selected-entry)
                                :action (lambda (selection)
                                          (harvest-clock-in-project-task-entry nil selection))))
            ))

(defun alist-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  (if symbols
      (alist-get (cdr symbols)
                 (assoc (car symbols) alist))
    (cdr alist)))

(defun harvest-format-entry (entry)
  "Format an ENTRY as a string.
Format is PROJECT (CLIENT) \n TASK - NOTES" 
  (let ((formatted-string (concat
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
                           )))
    (if (alist-get '(timer_started_at) entry)
        (propertize formatted-string 'face 'bold)
      (propertize formatted-string 'face 'nil))))

(defun harvest-format-project-entry (entry)
  "Show available projects and clients to clock in for ENTRY."
  (concat (alist-get '(name) entry) " (" (alist-get '(client) entry) ")")
  )

(defun harvest-get-cached-daily-entries ()
  "Get daily entries from the variable, or query Harvest if not set."
  ;; '(harvest-cached-daily-entries))
  (if (boundp 'harvest-cached-daily-entries)
      '(harvest-cached-daily-entries)
    (harvest-refresh-entries))
  '(harvest-cached-daily-entries))

(defun harvest-edit-description (entry)
  "Edit the description for a Harvest day ENTRY."
  (let ((harvest-payload (make-hash-table :test 'equal)))
    ;; TODO Not ideal to overwrite hours in Harvest, but unless we do it,
    ;; it gets reset to 0.
    (puthash "hours" (alist-get '(hours) entry) harvest-payload)
    (puthash "project_id" (alist-get '(project_id) entry) harvest-payload)
    (puthash "task_id" (alist-get '(task_id) entry) harvest-payload)
    (puthash "notes" (read-string "Notes: " (alist-get '(notes) entry)) harvest-payload)
    (harvest-api "POST" (format "daily/update/%s" (alist-get '(id) entry)) harvest-payload (format "Updated notes for task %s in %s for %s" (alist-get '(task) entry) (alist-get '(project) entry) (alist-get '(client) entry)))))

;;;###autoload
(defun harvest-clock-out ()
  "Clock out of any active timer."
  (interactive)
  (mapcar (lambda (entry)
            (if (alist-get '(timer_started_at) entry)
                (harvest-api "GET" (format "daily/timer/%s" (alist-get '(id) entry)) nil (message (format "Clocked out of %s in %s - %s" (alist-get '(task) entry) (alist-get '(project) entry) (alist-get '(client) entry))))))
          (alist-get '(day_entries) (harvest-refresh-entries))))

(defun harvest-get-tasks-for-project (project)
  ;; TODO: Add docstring.
  (mapcar (lambda (task)
            (cons
             (alist-get '(name) task)
             (format "%d:%d" (alist-get '(id) project) (alist-get '(id) task))))
          (alist-get '(tasks) project)))

(defun harvest-api (method path payload completion-message)
  "Make an API call to Harvest."
  ;; TODO: Document params.
  (let ((harvest-auth (harvest-get-credentials))
        (url-request-method method)
        (url-set-mime-charset-string)
        (url-mime-language-string nil)
        (url-mime-encoding-string nil)
        (url-mime-accept-string "application/json")
        (url-personal-mail-address nil)
        (url-request-data (json-encode payload)))
    (let ((request-url (concat "https://" (gethash "subdomain" harvest-auth) (format ".harvestapp.com/%s" path))))
      (let ((url-request-extra-headers
             `(("Content-Type" . "application/json")
               ("Authorization" . ,(gethash "auth" harvest-auth))))))
      (with-current-buffer (url-retrieve-synchronously request-url)
        (goto-char (point-min))
        (search-forward "\n\n" nil t)
        (delete-region (point-min) (point))
        (message "%s" completion-message)
        (json-read)
        ))))

(defun harvest-clock-in-project-task-entry (entry task)
  "Start a new timer for an ENTRY on a particular TASK.
Entry is actually not populated, which is why we need to split task on the
colon to retrieve project and task info."
  (let ((harvest-payload (make-hash-table :test 'equal)))
    (puthash "project_id" (car (s-split ":" task)) harvest-payload)
    (puthash "task_id" (car (cdr (s-split ":" task))) harvest-payload)
    (puthash "notes" (read-string "Notes: ") harvest-payload)
    (harvest-api "POST" "daily/add" harvest-payload (format "Started new task: %s" (gethash "notes" harvest-payload)))))

(defun harvest-toggle-timer-for-entry (entry)
  "Clock in or out of a given ENTRY."
  ;; TODO: Fix the logic here. Use let instead of setq.
  (let ((timer_started (assoc 'timer_started_at entry)))
    (if '(timer_started)
        (progn
          (setq prompt (format "Are you sure you want to clock in for %s?" (harvest-format-entry entry)))
          (setq clock-message (format "Clocked in for %s" (harvest-format-entry entry))))
      (setq prompt (format "Are you sure you want to clock out of %s?" (harvest-format-entry entry)))
      (setq clock-message (format "Clocked out of %s" (harvest-format-entry entry)))) 
    (if (yes-or-no-p prompt)
        (harvest-api "GET" (format "daily/timer/%s" (alist-get '(id) entry)) nil clock-message)
      )))

(provide 'harvest)
;;; harvest.el ends here
