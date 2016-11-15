;;; packages.el --- mikezackles Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq mikezackles-packages
    '(
      ;; This package does not exist. We need it to wrap the call to
      ;; spacemacs/declare-prefix.
      (mikezackles-helper :location built-in)
      ))

(defun mikezackles/init-mikezackles-helper ()
  (spacemacs/declare-prefix "o" "clipboard"))
