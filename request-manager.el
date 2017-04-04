;;; force-mode.el --- A functional force IDE -*- lexical-binding: t -*-

(defvar requests '(0 ""))
(defvar default-timeout-ms 5000)
(defvar default-request-interval-ms 100)
(defvar request-num 0)
(defvar request-timer 0)

(defun construct-request-message (type)
  '(status "PENDING" type type))

(defun force-mode-add-to-request-queue (args)
  (progn (setq request-num (+ 1 request-num))
         (plist-put requests request-num args)
         request-num))

(defun force-mode-start-request-timer ()
  )

(defun force-mode-monitor (req-key)
  "checks for completed requests; removes key when found"
  (let* ((results (plist-get requests req-key))
         (status (plist-get results 'status)))
    (print (concat "key is" status))
    (cond ((string= "PENDING" status) "Pending")
          ((string= "SUCCESS" status) "Success")
          ((string= "FAIL" status) "Fail")
          ("No request"))))

(defun force-cli-helm-complete (data)
  (interactive)
  (helm :sources (helm-build-sync-source "objects"
                   :candidates data
                   :fuzzy-match t)
        :buffer "* Force cli completions *"))

(defun force-cli-complete (params)
  (request
   (concat "localhost:8080/complete")
   :params params

   :parser
   (lambda ()
     (let ((json-object-type 'plist))
       (json-read)))

   :success
   (function* (lambda (&key data &allow-other-keys)
                (progn
                  (setq tester (cl-map 'list #'identity data))
                  (force-cli-helm-complete data))))))


(defun force-mode-send-request (filter)
  (progn (force-cli-complete `(("filter" . ,filter)
                               ("api" . "true")))))

;; (force-mode-send-request "ALMErrorServices.")
;; (force-cli-helm-complete tester)

(defun force-mode-server-request (request)
  "put a request in the queue, and give the requestor a key to monitor"
  (let ((req-key (force-mode-add-to-request-queue (construct-request-message request))))
    (force-mode-monitor req-key)))
