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

(defstruct gitlog
  (file-sha1)
  (commit-sha1)
  (author)
  (email)
  (date)
  (timezone)
  (type)
  (commit))

(defun tracked-files (directory)
  (with-open-file (in (merge-paths (force-directory directory) #P".git/index")
                      :element-type '(unsigned-byte 8))
    (let ((index (read-value 'index in)))
      (if (string= "DIRC" (file-type index))
          (loop repeat (entries index)
             for byte = (read-value 'entry in) do
               (read-until-not 0 in :read #'read-byte)
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
      (reverse (read-until #\/ str :type 'string)))))

(defun read-log (stream)
  (when (peek-char nil stream nil)
    (make-gitlog :file-sha1 (read-until #\Space stream :type 'string)
                 :commit-sha1 (read-until #\Space stream :type 'string)
                 :author (read-until #\Space stream :type 'string)
                 :email (read-until #\Space stream :type 'string)
                 :date (parse-integer (read-until #\Space stream :type 'string))
                 :timezone (read-until #\Tab stream :type 'string)
                 :type (read-until #\: stream :type 'string)
                 :commit (subseq (read-until #\Newline stream :type 'string) 1))))

(defun logs (directory)
  (with-open-file (in (merge-paths directory ".git/logs/HEAD"))
    (loop for log = (read-log in)
       until (null log)
       collect log)))
