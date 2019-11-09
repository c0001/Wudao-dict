(require 'wudao-cache)

(defvar wudao/query--hash-plist nil)

(defun wudao/query--ansi-color-apply (string)
  "The same as `ansi-color-apply', but using `face' instead of
`font-lock-faace' for make `message' colorized."
  (let ((codes (car ansi-color-context))
	(start 0) end result)
    ;; If context was saved and is a string, prepend it.
    (if (cadr ansi-color-context)
        (setq string (concat (cadr ansi-color-context) string)
              ansi-color-context nil))
    ;; Find the next escape sequence.
    (while (setq end (string-match ansi-color-control-seq-regexp string start))
      (let ((esc-end (match-end 0)))
        ;; Colorize the old block from start to end using old face.
        (when codes
          (put-text-property start end 'face
                             (ansi-color--find-face codes) string))
        (push (substring string start end) result)
        (setq start (match-end 0))
        ;; If this is a color escape sequence,
        (when (eq (aref string (1- esc-end)) ?m)
          ;; create a new face from it.
          (setq codes (ansi-color-apply-sequence
                       (substring string end esc-end) codes)))))
    ;; if the rest of the string should have a face, put it there
    (when codes
      (put-text-property start (length string)
                         'face (ansi-color--find-face codes) string))
    ;; save context, add the remainder of the string to the result
    (let (fragment)
      (if (string-match "\033" string start)
	  (let ((pos (match-beginning 0)))
	    (setq fragment (substring string pos))
	    (push (substring string start pos) result))
	(push (substring string start) result))
      (setq ansi-color-context (if (or codes fragment) (list codes fragment))))
    (apply 'concat (nreverse result))))

(defun wudao/query--get-hash ()
  (if (null wudao/query--hash-plist)
      (setq wudao/query--hash-plist
            (wudao/cache-read-hash))
    wudao/query--hash-plist))

;;;###autoload
(defun wudao/query-word-by-hash (query &optional full)
  (let* ((hsp (wudao/query--get-hash))
         (cbk (ignore-errors
                (wudao/query--ansi-color-apply
                 (if full (gethash query (plist-get hsp :full))
                   (gethash query (plist-get hsp :short)))))))
    cbk))

(defun wudao/query-word-by-command (query &optional full)
  (let ((sh-cmd-short "wd -s \"%s\"")
        (sh-cmd-full "wd \"%s\"")
        cbk)
    (setq cbk
          (wudao/query--ansi-color-apply
           (shell-command-to-string
            (format (if full sh-cmd-full
                      sh-cmd-short)
                    query))))
    (when (string-match-p "\\(^No such word:\\|^Error\\)" cbk)
      (setq cbk nil))
    cbk))

(provide 'wudao-query)
