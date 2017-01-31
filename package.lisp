;;;; package.lisp

(defpackage #:git-info
  (:use #:cl
        #:binary-utils
        #:peyton-utils
        #:files-and-folders
        #:dates-and-times)
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
           #:sha1
           #:flags
           #:logs
           #:gitlog
           #:gitlog-file-sha1
           #:gitlog-commit-sha1
           #:gitlog-author
           #:gitlog-email
           #:gitlog-date
           #:gitlog-timezone
           #:gitlog-type
           #:gitlog-message
           #:name
           #:tags
           #:tracked-files))
