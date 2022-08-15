(define (file->char_list path)
  (call-with-input-file path
    (lambda (file-port)
      (let loop ((x (read-char file-port)))
        (if (eof-object? x)
            '()
            (begin (cons x (loop (read-char file-port)))))))))

(define char-list-of-file (file->char_list "/Users/sukrit/Programs/fun/scheme-zip/compression.txt"))
