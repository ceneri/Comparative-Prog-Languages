#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (enter-labels program)
    (process-lines program))




;; HASH TABLES (Function)
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))

;; Initialize standard Methods inside *function-table*
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

        (mod     ,(lambda (x y) (- x (* (div x y) y))))
        (quot    ,(lambda (x y) (truncate (/ x y))))
        (rem     ,(lambda (x y) (- x (* (quot x y) y))))
        (+       ,+)
        (*       ,*)
        (-       ,-)
        (/       ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%       ,(lambda (x y) (- x (* (div x y) y))))
        (^       ,expt)
        (abs     ,abs)
        (abs     ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (cos     ,cos)
        (exp     ,exp)
        (floor   ,floor)
        (log     , (lambda(x)(log (if (equal? x 0) 0.0 x))))
        (log10   ,(lambda (x) (/ (log x) (log 10.0))))
        (log2   ,(lambda (x) (/ (log x) (log 2.0))))
        (round   ,round)
        (sin     ,sin)
        (sqrt    ,sqrt)
        (tan     ,tan)
        (trunc   ,truncate)

     ))

;; HASH TABLES (Label)
(define *label-table* (make-hash))
(define (label-get key)
        (hash-ref *label-table* key #f))
(define (label-put! key value)
        (hash-set! *label-table* key value))

;; HASH TABLES (Variable)
(define *variable-table* (make-hash))
(define (variable-get key)
        (hash-ref *variable-table* key #f))
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;; Initialize standard constants inside *variable-table*
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (pi      3.141592653589793238462643383279502884197169399)
        (e       2.718281828459045235360287471352662497757247093)
        
     ))

;;--------------------------- STATEMENTS -------------------------------

;; 'dim'        (dim (a size))
(define (process-dim stmt)
    ;(printf "Dim size: ~s~n" (cadadr stmt))
    (variable-put! (caadr stmt) (make-vector (evaluate-expr (cadadr stmt) ) ) ) 
)

;; 'let'        (let size 100)
(define (process-let stmt)
    (variable-put! (cadr stmt) (evaluate-expr (caddr stmt)) ) ;Need to handle array assignment as well and expression
)

;; 'goto'       (goto stop)
(define (process-goto stmt)
    (printf "Label: ~s~n" (cadr stmt))
    (if (is-label? (cadr stmt))
        (printf "Jump: ~n")
        (void)
      )
    ;(display (label-get (car stmt) ) ) ;;this is the jumping needs work
)

;; 'print'       (print "")   (print "2-2      = " (- 2 2))
(define (process-print stmt)
    ;(printf "Inside process print~n")
    
    ;Inner helper function
    (define (print-helper printable)
        (cond ((pair? printable)
            (printf "~a" (evaluate-expr (car printable)))
            (print-helper (cdr printable)))
    ))

    (print-helper (cdr stmt))
    (printf "~n")
)

;; 'input'       (input x)
(define (process-input stmt)
   (printf "Inside process input~n")
    (let ((x (read)))
      (unless (eof-object? x)
        (variable-put! (cadr stmt) x) ))

    (increment-inputcount)

)


(define (increment-inputcount)
   (printf "Inside increment inputcount~n")
    (if (variable-get 'inputcount)
        (variable-put! 'inputcount (+ (variable-get 'inputcount) 1))
        (variable-put! 'inputcount 1)
    )
)


;; 'if'       (if (< max size) read))
(define (process-if stmt)
    (define operator (caadr stmt))
    (define operand1 (evaluate-expr (cadadr stmt)))                 ;need to evaluate expressions
    (define operand2 (evaluate-expr (car (cddadr stmt)) ))
    (printf "Operand 1: ~s~n" (evaluate-expr operand1))
    (printf "Operand 2: ~s~n" (evaluate-expr operand2))
    (cond ((eqv? operator '=)
              (if (eq? operand1 operand2)
                  (printf "Jump: ~n")
                  (void)
              )
          )
          ((eqv? operator '<)
              (if (< operand1 operand2)
                  (printf "Jump: ~n")
                  (void)
              )
          )
          ((eqv? operator '>)
              (if (> operand1 operand2)
                  (printf "Jump: ~n")
                  (void)
              )
          )
          ((eqv? operator '<>)
              (if (not (eq? operand1 operand2) )
                  (printf "Jump: ~n")
                  (void)
              )
          )
          ((eqv? operator '>=)
              (if (>= operand1 operand2) 
                  (printf "Jump: ~n")
                  (void)
              )
          )
          ((eqv? operator '<=)
              (if (<= operand1 operand2) 
                  (printf "Jump: ~n")
                  (void)
              )
          )
    )
)
              

;;----------------Boolean methods for statements
(define (is-dim? stmt)
        ;(printf "IsDim ~s~n" (caar stmt))
        (eqv? (car stmt) 'dim)
)

(define (is-let? stmt)
        (eqv? (car stmt) 'let)
)

(define (is-goto? stmt)
        (eqv? (car stmt) 'goto)
)

(define (is-print? stmt)
        ;(printf "Inside is print~n")
        (eqv? (car stmt) 'print)
)

(define (is-if? stmt)
        ;(printf "Inside is if~n")
        (eqv? (car stmt) 'if)
)

(define (is-input? stmt)
        (printf "Inside is input~n")
        (eqv? (car stmt) 'input)
)

(define (is-variable? var)
        ;(printf "lol==================================================~n")
        (variable-get var)
)

(define (is-label? lab)
        ;(printf "lol==================================================~n")
        (label-get lab)
)

(define (is-function? func)
        ;(printf "lol==================================================~n")
        (function-get func)
)

;;-------------------------- EXPRESSIONS -------------------------------

(define (evaluate-expr expr) ;!!eval until something is a number
    (cond ((number? expr) expr ) ;return constant
          ((is-variable? expr) 
              (evaluate-expr (variable-get expr)) )    ;return function from memory, false otherwise
          ((and (pair? expr) (is-variable? (car expr)))                
              (vector-ref (variable-get (car expr)) (evaluate-expr (cadr expr) ) ) )    ;return array element
          ((and (pair? expr) (is-function? (car expr)))
              (apply (function-get (car expr)) (map evaluate-expr (cdr expr)) ))
          ;((string? expr) 
           ;   (string->number expr) ) ;return constant  
          ;((and (pair? expr) (function? (car expr)) (function-get (car expr)) (not (label-get (car expr))))  
              ;(apply (function-get (car expr)) (map evaluate-expr (cdr expr))))
          ;((and (pair? expr) (function? (car expr)) (variable-get (car expr))) 
              ;(+ (vector-ref (variable-get (car expr)) (inexact->exact (evaluate-expr(cadr expr)))) 0.0 ) )
          (else expr))
)


;; FUNCTIONS

;; Enter all labels to table (recursive)
(define (enter-labels program)
    (define line (car program))
    (cond ((null? (cdr line))
              (void))
          ((pair? (cadr line))
              (void))
          (else 
              (label-put! (cadr line) line)
              (printf "Label Created ~s~n" (cadr line)) ))

    (if (not (null? (cdr program) ) ) 
        (enter-labels (cdr program) )
        (void)
    )
)


;;Process a statement
(define (process-stmt stmt)
    ;(printf "Statement Received: ~s~n" stmt)
    (cond ((is-dim? stmt)(process-dim stmt)) 
          ((is-let? stmt)(process-let stmt)) 
          ((is-goto? stmt)(process-goto stmt))
          ((is-print? stmt)(process-print stmt))
          ((is-if? stmt)(process-if stmt))
          ((is-input? stmt)(process-input stmt))
          (else (do-nothing))
    )
)

;;Proccess every line (recursive)
(define (process-lines program)
    (define line (car program))
    (cond ((null? (cdr line))
              (void))
          ((and (not (pair? (cadr line)) ) (null? (cddr line) ) )
              ;(printf "Skip this one: ~s~n" (cadr line))
              (void))
          ((and (not (pair? (cadr line)) ) (pair? (caddr line) ) )
              ;(printf "Line with label being processesed: ~s~n" (caddr line))
              (process-stmt (caddr line)))
          ((pair? (cadr line))      ;No label statement
              ;(printf "Line being processes: ~s~n" (cadr line))
              (process-stmt (cadr line)))
          (else                     ;Statement with a label
              (printf "Dont know this one: ~s~n" (line))
              (void)) )

    (if (not (null? (cdr program) ) ) 
        (process-lines (cdr program) )
        (void)
    )
)


(define (do-nothing)
  (printf "NOTHING xD~n")
)


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (write-program-by-line sbprogfile program))))
              ;(enter-labels program))))
              ;(process-lines program))))

(main (vector->list (current-command-line-arguments)))


(error "Goodbye, World!")