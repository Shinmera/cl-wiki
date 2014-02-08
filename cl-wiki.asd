#|
  This file is a part of cl-wiki
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.ed-bot.wiki.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.ed-bot.wiki.asdf)

(defsystem cl-wiki
  :name "CL Mediawiki"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Mediawiki interaction library."
  :components ((:file "package")
               (:file "wiki" :depends-on ("package")))
  :depends-on (:drakma
               :cl-json))
