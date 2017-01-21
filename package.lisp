;;;; package.lisp

(defpackage #:git-info
  (:use #:cl
        #:binary-utils
        #:peyton-utils)
  (:export #:index
           #:entry
           #:metadata-modified
           #:file-modified
           #:dev
           #:inode
           #:mode
           #:uid
           #:gid
           #:file-size
           #:sha-1
           #:flags
           #:name
           #:read-repository))
