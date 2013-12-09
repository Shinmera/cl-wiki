#|
  This file is a part of cl-wiki
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :cl)
(defpackage org.tymoonnext.ed-bot.wiki
  (:use :cl)
  (:shadowing-import-from :cl :restart)
  (:nicknames :wiki)
  (:export :*wiki-api*
           :wiki-error :json :info :code
           :request
           :login
           :token
           :token-request
           :page-get
           :page-prepend
           :page-append
           :page-create
           :page-edit
           :page-delete
           :page-protect
           :page-rollback
           :user-block
           :recent-changes))
