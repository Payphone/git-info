;;;; package.lisp

(defpackage #:git-info
  (:use #:cl
        #:binary-utils
        #:peyton-utils
        #:files-and-folders
        #:split-sequence)
  (:export #:branches
           #:current-branch
           #:index
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
           #:remotes
           #:tags
           #:tracked-files))
