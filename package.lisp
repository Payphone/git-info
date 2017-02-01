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
           #:file-sha1
           #:commit-sha1
           #:author
           #:email
           #:date
           #:timezone
           #:type
           #:message
           #:name
           #:tags
           #:tracked-files))
