(defun lpaste-region ()
  (interactive)
  (format "curl http://lpaste.net/new?%s"
           (mapconcat 'identity
                      (mapcar (lambda (cons)
                                (concat (url-hexify-string (car cons))
                                        "="
                                        (url-hexify-string (cdr cons))))
                              `(("title" . "Elis1p")
                                ("author" . "chrisdone")
                                ("language" . "haskell")
                                ("channel" . "")
                                ("paste" . "Code here'%@!")
                                ("private" . "private")
                                ("email" . "")))
                      "&")))
