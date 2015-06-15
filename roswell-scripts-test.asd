#|
  This file is a part of roswell-scripts project.
  Copyright (c) 2015 Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage roswell-scripts-test-asd
  (:use :cl :asdf))
(in-package :roswell-scripts-test-asd)

(defsystem roswell-scripts-test
  :author "Rudolph Miller"
  :license "MIT"
  :homepage "https://github.com/Rudolph-Miller/roswell-scripts"
  :depends-on (:roswell-scripts
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "roswell-scripts"))))
  :description "Test system for roswell-scripts"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
