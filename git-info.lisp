;;;; git-info.lisp

(in-package #:git-info)

(defbinary index ()
  ((file-type s4)
   (version u4)
   (entries u4)))

(defbinary entry ()
  ((metadata-modified u8)
   (file-modified u8)
   (dev u4)
   (inode u4)
   (mode u4)
   (uid u4)
   (gid u4)
   (file-size u4)
   (sha1 u20)
   (flags u2)
   (name tstring)))

(defclass gitlog ()
  ((file-sha1 :initarg :file-sha1 :accessor file-sha1)
   (commit-sha1 :initarg :commit-sha1 :accessor commit-sha1)
   (author :initarg :author :accessor author)
   (email :initarg :email :accessor email)
   (date :initarg :date :accessor date)
   (timezone :initarg :timezone :accessor timezone)
   (log-type :initarg :log-type :accessor log-type)
   (message :initarg :message :accessor message)))

(defun tracked-files (directory)
  (with-open-file (in (merge-paths (force-directory directory) #P".git/index")
                      :element-type '(unsigned-byte 8))
    (let ((index (read-value 'index in)))
      (if (string= "DIRC" (file-type index))
          (loop repeat (entries index)
             for byte = (read-value 'entry in)
             collect byte)
          (error "Invalid Git repository")))))

(defun branches (directory)
  (mapcar #'pathname-name (list-directory (merge-paths directory
                                                       ".git/refs/heads/"))))

(defun tags (directory)
  (mapcar #'pathname-name (list-directory (merge-paths directory
                                                       ".git/refs/tags/"))))

(defun current-branch (directory)
  (with-open-file (in (merge-paths directory ".git/HEAD"))
    (with-open-stream (str (make-string-input-stream (reverse (read-line in))))
      (coerce (reverse (read-until #\/ str)) 'string))))

(defun read-log (stream)
  (when (peek-char nil stream nil)
    (make-instance 'gitlog
                   :file-sha1 (coerce (read-until #\Space stream) 'string)
                   :commit-sha1 (coerce (read-until #\Space stream) 'string)
                   :author (coerce (read-until #\< stream) 'string)
                   :email (coerce (butlast (read-until #\Space stream)) 'string)
                   :date (parse-integer (coerce (read-until #\Space stream) 'string))
                   :timezone (coerce (read-until #\Tab stream) 'string)
                   :log-type (coerce (read-until #\: stream) 'string)
                   :message (coerce (cdr (read-until #\Newline stream)) 'string))))

(defun logs (directory)
  (with-open-file (in (merge-paths directory ".git/logs/HEAD"))
    (loop for log = (read-log in)
       until (null log)
       collect log)))
