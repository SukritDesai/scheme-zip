(define (file->char_list path)
  (call-with-input-file path
    (lambda (file-port)
      (let loop ((x (read-char file-port)))
        (if (eof-object? x)
            '()
            (begin (cons x (loop (read-char file-port)))))))))

(define char-list-of-file (file->char_list "/Users/sukrit/Programs/fun/scheme-zip/compression.txt"))

 (define (element-of-set? x set)
   (cond ((null? set) #f)
         ((equal? x (car set)) #t)
         (else (element-of-set? x (cdr set)))))

(define (symbol-in-tree? symbol tree)
  (cond ((null? tree) #f)
        ((list? (car tree))
         (if (element-of-set? symbol (car tree))
             #t
             (symbol-in-tree? symbol (cdr tree))))
        ((equal? symbol (car tree)) #t)
        (else (symbol-in-tree? symbol (cdr tree)))))

(define (char->symbol ch)
  (string->symbol (string ch)))

(define (symbol->char symbol)
  (if (> (length (string->list
                  (symbol->string symbol))) 1)
      (error "Expected a one character symbol, got:" symbol)
      (car (string->list (symbol->string symbol)))))

 (define (char-list->symbol-list char-list)
   (if (null? char-list)
       #nil
       (cons (char->symbol (car char-list))
             (char-list->symbol-list (cdr char-list)))))

(define (symbol-list->char-list symbol-list)
  (if (null? symbol-list)
      #nil
      (cons (symbol->char (car symbol-list))
            (symbol-list->char-list (cdr symbol-list)))))
