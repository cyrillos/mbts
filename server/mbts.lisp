;;;
;;; For SBCL only
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :yason :silent t)
  (ql:quickload :trivial-utf-8 :silent t))

;;;
;;; Various helpers
;;;
(defun cli-options ()
  (rest sb-ext:*posix-argv*))

(defmacro to-utf8 (b)
  `(trivial-utf-8:utf-8-bytes-to-string ,b))

(defmacro utf8-to (s)
  `(trivial-utf-8:string-to-utf-8-bytes ,s))

(defmacro ubytes (size &rest body)
  `(make-array ,size
               :element-type '(unsigned-byte 8)
               ,@body))

(defun read-utf8 (stream size)
  (ignore-errors
    (let* ((buf (ubytes size))
           (pos (read-sequence buf stream :start 0 :end size)))
      (when pos
        (to-utf8 (subseq buf 0 pos))))))

(defun read-file-utf8 (path)
  (when (probe-file path)
    (with-open-file (stream path :element-type '(unsigned-byte 8))
      (read-utf8 stream (file-length stream)))))

;;;
;;; JSON herlpers
;;;
(defun json-parse (s)
  (ignore-errors
    (case (array-element-type s)
      ((character) (yason:parse s :object-as :alist))
      (t (yason:parse (to-utf8 s) :object-as :alist)))))

(defmacro json-get (key jdata)
  `(cdr (assoc ,key ,jdata :test #'equalp)))

(defmacro json-add (key value jdata)
  `(setf ,jdata (nconc ,jdata (list (cons ,key ,value)))))

(defmacro json-leaf (key value)
  `(list (cons ,key ,value)))

;;;
;;; Printing helpers
;;;
(defparameter *loglevels* '(("debug" . 3) ("info" . 2) ("warn" . 1) ("error" . 0)))

(defun loglevel-decode (name)
  (cdr (assoc name *loglevels* :test #'equalp)))

(defparameter *current-loglevel* (loglevel-decode "debug"))

(defun loglevel-get () (values *current-loglevel*))

(defun loglevel-set (lvl-or-name)
  (let ((lvl (if (stringp lvl-or-name)
                 (cdr (assoc lvl-or-name *loglevels* :test #'equalp))
                 (values lvl-or-name))))
    (when lvl
      (setf *current-loglevel* lvl))))

(defun loglevel-should-print (lvl)
  (when (<= lvl (loglevel-get)) t))

(defmacro print-on-level (lvl prefix fmt &rest body)
  `(when (loglevel-should-print ,lvl)
     (format t (concatenate 'string ,prefix ,fmt "~%") ,@body)))

(defmacro pr-err (fmt &rest body)
  `(print-on-level 1 "error: " ,fmt ,@body))

(defmacro pr-warn (fmt &rest body)
  `(print-on-level 2 "warn : " ,fmt ,@body))

(defmacro pr-info (fmt &rest body)
  `(print-on-level 3 "info : " ,fmt ,@body))

(defmacro pr-debug (fmt &rest body)
  `(print-on-level 4 "debug: " ,fmt ,@body))

(defmacro pr-exit (fmt &rest body)
  `(progn (pr-info ,fmt ,@body)
     #+sbcl (sb-ext:exit :code 0)))

(defmacro pr-fatal (fmt &rest body)
  `(progn (pr-err ,fmt ,@body)
     (sb-ext:exit :code 1)))

;;;
;;; Main code
;;;

(defparameter *conf* nil)
(defparameter *doexit* nil)

(defun run-action (action)
  (let ((cmd (json-get "cmd" action))
        (args (json-get "args" action)))
    (pr-debug "run ~a ~a" cmd args)
    (when cmd
      (sb-ext:run-program cmd args :wait t))))


(defun actions-loop (actions)
  (let* ((idle (json-get "idle" *conf*))
         (idle-secs (if idle (parse-integer idle) 1)))
    (loop do
          (dolist (action actions)
            (run-action action))
          (pr-debug "idle ~d sec" idle-secs)
          (sleep idle-secs)
          (when *doexit*
            (loop-finish)))))

(defun process-args (argv)
  (when argv
    (let ((next (cdr argv)))
      (cond
        ((and (string= (first argv) "--conf")
              (>= (list-length argv) 2))
         (setf *conf* (json-parse (read-file-utf8 (second argv))))
         (setf next (cddr argv))))
      (process-args next))))

(process-args (cli-options))

(when (not *conf*)
    (pr-fatal "No '--conf path-to-conf' option provided"))
(loglevel-set (json-get "loglevel-server" *conf*))

(let ((pid (sb-posix:getpid))
      (path (json-get "pidfile" *conf*)))
  (when path
    (with-open-file (out path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format out "~d" pid) :stream path)))

(actions-loop (json-get "actions" *conf*))
(pr-exit "exiting")
