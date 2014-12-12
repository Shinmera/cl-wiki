#|
  This file is a part of cl-wiki
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.ed-bot.wiki)

(defvar *cookies* (make-instance 'drakma:cookie-jar))
(defvar *edit-token* NIL)
(defvar *wiki-api* NIL)

(defmacro config-tree (config &rest branches)
  "Retrieve a configuration value based on a branch."
  (labels ((ct (branches)
             (if branches
                 `(cdr (assoc ,(car branches) ,(ct (cdr branches))))
                 config)))
    (ct (reverse branches))))

(define-condition wiki-error (error)
  ((json :initarg :json :accessor json)
   (info :initarg :info :accessor info)
   (code :initarg :code :accessor code))
  (:documentation "Condition signalled when a wiki request returns an error."))

(defmethod print-object ((obj wiki-error) stream)
  (format stream "~a (~a)" (slot-value obj 'info) (slot-value obj 'code)))

(defun request (action &optional extraparams)
  "Short wrapper around drakma to make request statements slimmer."
  (restart-case 
      (let ((resp (drakma:http-request *wiki-api*
                                       :method :POST :parameters (acons "format" "json" (acons "action" action extraparams))
                                       :cookie-jar *cookies*
                                       :want-stream T
                                       :external-format-in :utf-8
                                       :external-format-out :utf-8))
            (json NIL))
        (unwind-protect
             (progn
               (setf json (json:decode-json resp))
               (when (config-tree json :error)
                 (error 'wiki-error :json json :info (config-tree json :error :info) :code (config-tree json :error :code)))
               json)
          (close resp)))
    (re-request (&optional new-extraparams)
      (request action (or new-extraparams extraparams)))))

(defun login (user pass)
  "Authenticate on the wiki and retrieve the edit token."
  (let ((reply NIL))
    (setf reply (request "login" `(("lgname" . ,user) ("lgpassword" . ,pass))))    
    (when (string-equal (config-tree reply :login :result) "NeedToken")    
      (setf reply (request "login" `(("lgname" . ,user) ("lgpassword" . ,pass) ("lgtoken" . ,(config-tree reply :login :token))))))
    (if (string-equal (config-tree reply :login :result) "Success")
        (token)
        (error "Login failed!"))))

(defun token ()
  (let ((reply (request "query" `(("prop" . "info") ("titles" . "Main_Page") ("intoken" . "edit")))))
    (setf *edit-token* (config-tree (cdr (first (config-tree reply :query :pages))) :edittoken))  
    *edit-token*))

(defun token-request (action params &key (edit-token *edit-token*))
  "Wrapper for token-required requests."
  (assert (not (eq edit-token NIL)) (edit-token) "Edit token is not available! Are you successfully logged in?")
  (handler-bind ((wiki-error 
                  #'(lambda (err)
                      (let ((code (config-tree (slot-value err 'json) :error :code)))
                        (if (string= code "badtoken")
                            (invoke-restart 're-request (acons "token" (token) params))
                            (error err))))))
    (request action (acons "token" edit-token params))))

(defun page-prepend (title data &key (edit-token *edit-token*))
  "Attempt to edit a wiki page and append the data."
  (token-request "edit" `(("title" . ,title) ("prependtext" . ,data)) :edit-token edit-token))

(defun page-append (title data &key (edit-token *edit-token*))
  "Attempt to edit a wiki page and append the data."
  (token-request "edit" `(("title" . ,title) ("appendtext" . ,data)) :edit-token edit-token))

(defun page-create (title data &key (edit-token *edit-token*))
  "Attempt to create a wiki page and add the data."
  (token-request "edit" `(("title" . ,title) ("createonly") ("appendtext" . ,data)) :edit-token edit-token))

(defun page-edit (title data &key (edit-token *edit-token*))
  "Attempt to edit a wiki page and set the data."
  (token-request "edit" `(("title" . ,title) ("nocreate") ("text" . ,data)) :edit-token edit-token))

(defun page-delete (title &key (edit-token *edit-token*))
  "Attempt to delete a wiki page."
  (token-request "delete" `(("title" . ,title)) :edit-token edit-token))

(defun page-protect (page &key (protections '(("edit" "sysop") ("move" "sysop"))) (expiry "never") (reason "") cascade (edit-token *edit-token*))
  (let* ((protections (format NIL "~{~{~a~^=~}~^|~}" protections))
         (data `(("title" . ,page) ("protections" . ,protections) ("expiry" . ,expiry) ("reason" . ,reason))))
    (if cascade (push (list "cascade") data))
    (token-request "protect" data :edit-token edit-token)))

(defun page-get (title)
  "Get the content of a wiki page."
  (config-tree 
   (car (config-tree 
         (cdr (first (config-tree 
                      (request "query" `(("titles" . ,title) ("rvprop" . "content") ("prop" . "revisions")))
                      :query :pages)))
         :revisions))
   :*))

(defun rollback-token (title)
  "Get the rollback token and user of a wiki page."
  (let ((revisiondata (car (config-tree
                            (cdr (first (config-tree
                                         (request "query" `(("titles" . ,title) ("prop" . "revisions") ("rvtoken" . "rollback")))
                                         :query :pages)))
                            :revisions))))
    (values
     (config-tree revisiondata :rollbacktoken)
     (config-tree revisiondata :user))))

(defun page-rollback (title &key summary markbot)
  "Rollback the latest change on a wiki page."
  (multiple-value-bind (token user) (rollback-token title)
    (let ((data `(("title" . ,title) ("user" . ,user))))
      (if markbot (push (list "markbot") data))
      (if summary (push `("summary" . ,summary) data))
      (token-request "rollback" data :edit-token token))))

(defun user-block (user &key (expiry "never") (reason "") anononly nocreate autoblock noemail (edit-token *edit-token*))
  "Attempt to block a user."
  (let ((data `(("user" . ,user) ("expiry" . ,expiry) ("reason" . ,reason))))
    (if anononly (push (list "anononly") data))
    (if nocreate (push (list "nocreate") data))
    (if autoblock (push (list "autoblock") data))
    (if noemail (push (list "noemail") data))
    (token-request "delete" data :edit-token edit-token)))

(defvar *rc-typelist* '("edit" "external" "new" "log"))
(defvar *rc-dirlist* '("older" "newer"))
(defvar *rc-proplist* '("user" "userid" "comment" "parsedcomment" "timestamp" "title" "ids" "sizes" "redirect" "patrolled" "loginfo" "tags" "flags"))
(defvar *rc-showlist* '("minor" "!minor" "bot" "!bot" "anon" "!anon" "redirect" "!redirect" "patrolled" "!patrolled"))

(defun find-all (list1 list2 &key (test #'equal))
  (dolist (var list1)
    (unless (find var list2 :test test)
      (return-from find-all NIL)))
  T)

(defun recent-changes (&key start end direction namespaces user excludeuser tag type show properties limit toponly)
  "Returns recent-changes log data. The first return value is the log list, the second the start timestamp to continue."
  (let ((proplist `(("list" . "recentchanges"))))
    (when start (push (cons "rcstart" start) proplist))
    (when end (push (cons "rcend" end) proplist))
    (when direction
      (setf direction (string-downcase direction))
      (assert (find direction *rc-dirlist* :test #'equal) () "Direction must be one of ~a." *rc-dirlist*)
      (push (cons "rcdir" direction) proplist))
    (when namespaces
      (unless (listp show) (setf show (list show)))
      (push (cons "rcnamespace" (format NIL "~{~a~^%7C~}" namespaces)) proplist))
    (when user (push (cons "rcuser" user) proplist))
    (when excludeuser (push (cons "rcexcludeuser" excludeuser) proplist))
    (when tag (push (cons "rctag" tag) proplist))
    (when type
      (setf type (string-downcase type))
      (assert (find type *rc-typelist* :test #'equal) () "Type must be one of ~a." *rc-typelist*)
      (push (cons "rctype" type) proplist))
    (when show
      (unless (listp show) (setf show (list show)))
      (setf show (mapcar #'string-downcase show))
      (assert (find-all show *rc-showlist*) () "Show items must be one of ~a." *rc-proplist*)
      (push (cons "rctype" (format NIL "~{~a~^%7C~}" show)) proplist))
    (when properties
      (unless (listp properties) (setf properties (list properties)))
      (setf properties (mapcar #'string-downcase properties))
      (assert (find-all properties *rc-proplist*) () "Properties must be one of ~a." *rc-proplist*)
      (push (cons "rcprop" (format NIL "~{~a~^|~}" properties)) proplist))
    (when limit (push (cons "rclimit" (format NIL "~a" limit)) proplist))
    (when toponly (push (cons "toponly" "true") proplist))
    (let ((query (request "query" proplist)))
      (values (config-tree query :query :recentchanges)
              (config-tree query :query-continue :recentchanges :rcstart)))))

(defvar *search-proplist* '("size" "wordcount" "timestamp" "score" "snippet" "titlesnippet" "redirectsnippet" "redirecttitle" "sectionsnippet" "sectiontitle" "hasrelated"))
(defvar *search-whatlist* '("title" "text" "nearmatch"))
(defun wiki-search (query &key (properties '("timestamp")) (offset 0) (limit 10) redirects (what "title"))
  "Search for matching pages."
  (assert (find-all properties *search-proplist*) () "PROPERTIES must be one of ~a." *search-proplist*)
  (assert (find what *search-whatlist* :test #'string-equal) () "WHAT must be one of ~a." *search-whatlist*)
  (let ((query (request "query" `(("list" . "search") 
                                  ("srprop" . ,(format NIL "~{~a~^|~}" properties))
                                  ("sroffset" . ,(format NIL "~a" offset)) 
                                  ("srlimit" . ,(format NIL "~a" limit))
                                  ("srredirects" . ,(format NIL "~:[false~;true~]" redirects)) 
                                  ("srwhat" . ,what)
                                  ("srsearch" . ,query)))))
    (values (config-tree query :query :search))))

(defvar *parse-proplist* '("text" "langlinks" "categories" "categorieshtml" "languageshtml" "links" "templates" "images" "externallinks" "sections" "revid" "displaytitle" "headitems" "headhtml" "iwlinks"))
(defvar *parse-modellist* '("wikitext" "javascript" "css" "text" "JsonZeroConfig" "Scribunto" "JsonSchema"))
(defun wiki-parse (&key (disablepp T) old-id page page-id only-pst (properties '("text")) pst redirects section sections language summary text title contentmodel generatexml)
  "Get the parsed content of a wiki page."
  (assert (or page page-id summary text title) () "One of PAGE PAGE-ID SUMMARY TEXT or TITLE required.")
  (let ((proplist))
    (when disablepp (push (cons "disablepp" "true") proplist))
    (when old-id (push (cons "old-id" (format NIL "~a" old-id)) proplist))
    (when page (push (cons "page" page) proplist))
    (when page-id (push (cons "pageid" (format NIL "~a" page-id)) proplist))
    (when only-pst (push (cons "onlypst" "true") proplist))
    (when properties 
      (assert (find-all properties *parse-proplist*) () "PROPERTIES must be one or more of ~a." *parse-proplist*)
      (push (cons "prop" (format NIL "~{~a~^|~}" properties)) proplist))
    (when pst (push (cons "pst" "true") proplist))
    (when redirects (push (cons "redirects" "true") proplist))
    (when section (push (cons "section" (format NIL "~a" section)) proplist))
    (when sections (push (cons "sections" sections) proplist))
    (when language (push (cons "uselang" language) proplist))
    (when summary (push (cons "summary" summary) proplist))
    (when text (push (cons "text" text) proplist))
    (when title (push (cons "title" title) proplist))
    (when contentmodel 
      (assert (find contentmodel *parse-modellist*) () "CONETNTMODEL must be one of ~a." *parse-modellist*)
      (push (cons "contentmodel" contentmodel) proplist))
    (when generatexml (push (cons "generatexml" "true") proplist))
    (let ((query (request "parse" proplist)))
      (values (config-tree query :parse :text :*)
              query))))
