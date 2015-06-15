#|
  This file is a part of roswell-scripts project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

#|
  Useful Roswell Scripts.
  Author: Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage roswell-scripts-asd
  (:use :cl :asdf))
(in-package :roswell-scripts-asd)

(defsystem roswell-scripts
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/roswell-scripts"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "roswell-scripts"))))
  :description "Useful Roswell Scripts."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op roswell-scripts-test))))
