;;;; git-info.lisp

(in-package #:git-info)

(defun octets->string->integer (octets)
  (parse-integer (octets->string octets) :junk-allowed t))

(defdata space-tstring :terminator (char-code #\Space) :function #'octets->string)
(defdata tab-tstring :terminator (char-code #\Tab) :function #'octets->string)
(defdata colon-tstring :terminator (char-code #\:) :function #'octets->string)
(defdata space-tinteger :terminator (char-code #\Space)
         :function #'octets->string->integer)
(defdata newline-tstring :terminator (char-code #\Newline) :function #'octets->string)

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

(defbinary gitlog ()
  ((file-sha1 space-tstring)
   (commit-sha1 space-tstring)
   (author space-tstring)
   (email space-tstring)
   (date space-tinteger)
   (timezone tab-tstring)
   (log-type colon-tstring)
   (message newline-tstring)))

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
      (reverse (read-until #\/ str :type 'string)))))

(defun logs (directory)
  (with-open-file (in (merge-paths directory ".git/logs/HEAD")
                      :element-type '(unsigned-byte 8))
    (loop for log = (read-value 'gitlog in)
       until (null log)
       collect log)))
