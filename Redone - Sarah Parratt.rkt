#lang racket

;; Project 1 - Prefix Calculator
;; Name: Sarah Parratt
;; Course: CS 4337 - Programming Language Paradigms
;; Purpose: Evaluates prefix expressions and stores previous results for $n references.

(define interactive?
  (not (member "-b" (vector->list (current-command-line-arguments)))))

(define (skip-ws chars)
  (cond
    [(null? chars) chars]
    [(char-whitespace? (car chars)) (skip-ws (cdr chars))]
    [else chars]))

(define (error-result) 'error)

;; Parses unsigned integers/decimals. A leading - is handled in eval-expr.
(define (parse-number chars)
  (define (collect cs acc)
    (cond
      [(and (pair? cs)
            (or (char-numeric? (car cs)) (char=? (car cs) #\.)))
       (collect (cdr cs) (cons (car cs) acc))]
      [else
       (let ([num (string->number (list->string (reverse acc)))])
         (if num
             (list num cs)
             (error-result)))]))
  (collect chars '()))

(define (parse-history chars history)
  (define-values (digits rest)
    (let loop ([cs chars] [acc '()])
      (cond
        [(and (pair? cs) (char-numeric? (car cs)))
         (loop (cdr cs) (cons (car cs) acc))]
        [else (values acc cs)])))
  (if (null? digits)
      (error-result)
      (let* ([n (string->number (list->string (reverse digits)))]
             [hist (reverse history)])
        (if (or (<= n 0) (> n (length hist)))
            (error-result)
            (list (list-ref hist (- n 1)) rest)))))

;; Evaluates one prefix expression and returns (list value remaining-chars), or 'error.
(define (eval-expr chars history)
  (let ([chars (skip-ws chars)])
    (cond
      [(null? chars) (error-result)]

      ;; Handles negative numeric literals such as -5 or -3.2.
      [(and (char=? (car chars) #\-)
            (pair? (cdr chars))
            (or (char-numeric? (cadr chars)) (char=? (cadr chars) #\.)))
       (let ([res (parse-number (cdr chars))])
         (if (eq? res 'error)
             'error
             (list (- (car res)) (cadr res))))]

      ;; Binary operators: +, -, *, /
      [(member (car chars) '(#\+ #\- #\* #\/))
       (let* ([op (car chars)]
              [left (eval-expr (cdr chars) history)])
         (if (eq? left 'error)
             'error
             (let ([right (eval-expr (cadr left) history)])
               (if (eq? right 'error)
                   'error
                   (let ([a (car left)]
                         [b (car right)]
                         [rest (cadr right)])
                     (cond
                       [(and (char=? op #\/) (= b 0)) 'error]
                       [else
                        (list
                         (cond
                           [(char=? op #\+) (+ a b)]
                           [(char=? op #\-) (- a b)]
                           [(char=? op #\*) (* a b)]
                           [(char=? op #\/) (/ a b)])
                         rest)]))))))]

      ;; History reference: $1, $2, etc.
      [(char=? (car chars) #\$)
       (parse-history (cdr chars) history)]

      ;; Number
      [(or (char-numeric? (car chars)) (char=? (car chars) #\.))
       (parse-number chars)]

      [else 'error])))

(define (evaluate line history)
  (let ([res (eval-expr (string->list line) history)])
    (if (eq? res 'error)
        'error
        (let ([remaining (skip-ws (cadr res))])
          (if (null? remaining)
              (car res)
              'error)))))

(define (print-result n result)
  (displayln
   (string-append
    (number->string n)
    ": "
    (number->string result))))

(define (repl history)
  (when interactive?
    (display "> ")
    (flush-output))
  (let ([line (read-line)])
    (unless (eof-object? line)
      (if (or (string=? line "quit") (string=? line "exit"))
          (void)
          (let ([result (evaluate line history)])
            (cond
              [(eq? result 'error)
               (displayln "Error: Invalid Expression")
               (repl history)]
              [else
               (define new-history (cons result history))
               (print-result (length new-history) result)
               (repl new-history)]))))))

(repl '())