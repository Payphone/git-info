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
   (sha-1 u20)
   (flags u2)
   (name tstring)))

(defun read-repository (directory)
  (with-open-file (in (merge-pathnames #P".git/index" (car (directory directory)))
                      :element-type '(unsigned-byte 8))
    (let ((index (read-value 'index in)))
      (if (string= "DIRC" (file-type index))
          (loop repeat (entries index)
             for byte = (read-value 'entry in) do
               (read-until-not 0 in :read #'read-byte)

             collect byte)
          (error "Invalid Git repository")))))
