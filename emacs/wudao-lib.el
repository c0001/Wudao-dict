(defun wudao/lib-message (message &rest args)
  (if (fboundp 'entropy/emacs-message-do-message)
      (entropy/emacs-message-do-message
       message args)
    (apply 'message message args)))

(provide 'wudao-lib)
