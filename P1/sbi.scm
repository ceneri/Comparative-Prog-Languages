#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; Author: Cesar Neri       ID: 1513805      email:ceneri@ucsc.edu
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

;;----------------LEGACY/PROVIDED CODE----------------------------

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

;;---------------------- HASH TABLES --------------------------------

;; (Functions)
(define *function-table* (make-hash))
(define (function-get key)
        (hash-ref *function-table* key #f))
(define (function-put! key value)
        (hash-set! *function-table* key value))

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
(define (variable-remove! key )
        (hash-remove! *variable-table* key))

;; HASH TABLES (Variable)
(define *line-table* (make-hash))
(define (line-get key)
        (hash-ref *variable-table* key #f))
(define (line-put! key value)
        (hash-set! *variable-table* key value))


;;---------------------- Initialize TABLES ----------------------------

;; Initialize standard Methods inside *function-table*
(for-each
    (lambda (pair)
            (function-put! (car pair) (cadr pair)))
    `(

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

;; Initialize standard constants inside *variable-table*
(for-each
    (lambda (pair)
            (variable-put! (car pair) (cadr pair)))
    `(

        (pi      3.141592653589793238462643383279502884197169399)
        (e       2.718281828459045235360287471352662497757247093)
        
     ))

;;--------------------------- HELPER METHODS --------------------------

;; Fixes index whjen working with arrays
(define (index i)
    (- i 1)
)

;; For debugging purposes
(define (do-nothing)
  (printf "NOTHING xD~n")
)

;; Error printing
(define (throw-error msg stmt-num)
  (error msg)
  (error "Line: ~a" stmt-num)
  (exit 1)
)

;;---------------- BOOLEAN METHODS ---------------------------
(define (is-dim? stmt)
    (eqv? (car stmt) 'dim)
)

(define (is-let? stmt)
    (eqv? (car stmt) 'let)
)

(define (is-goto? stmt)
    (eqv? (car stmt) 'goto)
)

(define (is-print? stmt)
    (eqv? (car stmt) 'print)
)

(define (is-if? stmt)
    (eqv? (car stmt) 'if)
)

(define (is-input? stmt)
    (eqv? (car stmt) 'input)
)

(define (is-variable? var)
    (variable-get var)
)

(define (is-label? lab)
    (label-get lab)
)

(define (is-function? func)
    (function-get func)
)

(define (is-line? line)
    (line-get line)
)

;;---------------- STATEMENT PROCESSING ---------------------------

;; 'dim'   ex:     (dim (a size))
(define (process-dim stmt current-line)
    ;(printf "Dim size: ~s~n" (evaluate-expr (cadadr stmt) ))
    (variable-put! (caadr stmt) 
                    (make-vector (evaluate-expr (cadadr stmt) )))
    (+ current-line 1) 
)

;; 'let'   ex:        (let size 100)  |  (let (a max) x))
(define (process-let stmt current-line)
    ;(printf "Inside let~n")
    (cond ((pair? (cadr stmt))      ;array    (vector-set! vec pos v)
              (vector-set! (variable-get (caadr stmt)) 
                           (index (evaluate-expr (cadadr stmt))) 
                           (evaluate-expr (caddr stmt)) ))
          (else
              (variable-put! (cadr stmt) 
                             (evaluate-expr (caddr stmt)) ) ))

    (+ current-line 1)
)

;; 'goto'     ex:  (goto stop)
(define (process-goto stmt current-line)
    ;(printf "Label: ~s~n" (cadr stmt))
    (if (is-label? (cadr stmt))
        (label-get (cadr stmt) )
        (throw-error "Invalid label." current-line) ) 
)

;; 'if'      ex: (if (< max size) read))
(define (process-if stmt current-line)
    (define operator (caadr stmt))
    (define operand1 (evaluate-expr (cadadr stmt)))                
    (define operand2 (evaluate-expr (car (cddadr stmt)) ))
    (define label (caddr stmt)) 
    ;(printf "Operand 1: ~s~n" operand1)
    ;(printf "Operand 2: ~s~n" operand2)
    (cond ((eqv? operator '=)
              (if (= operand1 operand2)
                  (label-get label)
                  (+ current-line 1) ))
          ((eqv? operator '<)
              (if (< operand1 operand2)
                  (label-get label)
                  (+ current-line 1) ))
          ((eqv? operator '>)
              (if (> operand1 operand2)
                  (label-get label)
                  (+ current-line 1) ))
          ((eqv? operator '<>)
              (if (not (= operand1 operand2) )
                  (label-get label)
                  (+ current-line 1) ))
          ((eqv? operator '>=)
              (if (>= operand1 operand2) 
                  (label-get label)
                  (+ current-line 1) ))
          ((eqv? operator '<=)
              (if (<= operand1 operand2) 
                  (label-get label)
                  (+ current-line 1) )) )
)

;; 'print'    ex:   (print "") |  (print "2-2   = " (- 2 2))
(define (process-print stmt current-line)
    ;(printf "Inside process print~n")
    
    ;Inner helper function (Recursive printer)
    (define (print-helper printable)
        (cond ((pair? printable)
                  (printf "~a" (evaluate-expr (car printable)))
                  (print-helper (cdr printable)) ))
    )

    (print-helper (cdr stmt))
    (printf "~n")
    (+ current-line 1)
)

;; 'input'      ex: (input x) | (input a b c)
;; Accepts an input(s) (by calling helper method) and returns next 
;; line (Always line++). Recurses to chewck for next input
(define (process-input stmt current-line)
    ;(printf "Inside process input~n")
    (cond ((null? (cdr stmt)) 
              (void) )
          (else
              (input-helper stmt)
              (process-input (cdr stmt) current-line) ))

    (+ current-line 1)
)

;; Reads until end of file is entered
(define (input-helper stmt)
    ;(printf "Inside input helper~n")
    (let ((x (read)))
      (cond ((eof-object? x)
                (variable-put! 'inputcount -1) )
            (else
                (variable-put! (cadr stmt) x)
                (increment-inputcount) )) )
)

;; Handles 'inputcount variable
(define (increment-inputcount)
   ;(printf "Inside increment inputcount~n")
    (if (variable-get 'inputcount)
        (variable-put! 'inputcount (+ (variable-get 'inputcount) 1))
        (variable-put! 'inputcount 1)
    )
)

;;-------------------------- EXPRESSIONS -----------------------------

;; Evaluates a expresion
(define (evaluate-expr expr) 
    (cond ((number? expr)         ;return constant
              expr)                    
          ((is-variable? expr)      ;return from memory
              (evaluate-expr (variable-get expr)) )   
          ((and (pair? expr) (is-variable? (car expr)))  ;array element
              (vector-ref (variable-get (car expr)) 
                          (index (evaluate-expr (cadr expr) )) ))    
          ((and (pair? expr) (is-function? (car expr)))  ;funct map
              (apply (function-get (car expr)) 
                    (map evaluate-expr (cdr expr)) ))
          (else expr))
)

;;-----------------------------MAIN METHODS---------------------------

;; Calls the 2 other major functions to proces program
(define (silly-basic-int filename program)
    ;(write-program-by-line filename program)
    (enter-labels program)
    (interpret-program 1)
)

;; Writes all the lines in the sbir language that are to be read
;; For debugging only
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
)

;; Enters all labels to table (Recursive)
;; and lines to line hash
(define (enter-labels program)
    (define line (car program))
    (line-put! (car line) line ) ;Enter line to hash

    ;;If line has label save it
    (cond ((null? (cdr line))
              (void))
          ((pair? (cadr line))
              (void))
          (else 
              (label-put! (cadr line) (car line)) ))

    ;Check the next line for labels
    (if (not (null? (cdr program) ) ) 
        (enter-labels (cdr program) )
        (void)
    )
)

;; Process/interpret a statement (returns next line to be processed)
(define (process-stmt stmt current-line)

    (cond ((is-dim? stmt)(process-dim stmt current-line)) 
          ((is-let? stmt)(process-let stmt current-line)) 
          ((is-goto? stmt)(process-goto stmt current-line))
          ((is-print? stmt)(process-print stmt current-line))
          ((is-if? stmt)(process-if stmt current-line))
          ((is-input? stmt)
                (if (variable-get 'inputcount)
                    (variable-remove! 'inputcount)
                    (void))
                (process-input stmt current-line))
          (else (+ current-line 1))
    )
)

;; Process lines (Recursively)
(define (interpret-program line-num)
    (define line (line-get line-num))        
    
    ;Process current
    (cond ((not line)
              (exit 0))
          ((null? (cdr line))
              (set! line-num (+ line-num 1) ))
          ((and (not (pair? (cadr line)) ) (null? (cddr line) ) )
              ;(printf "Skip this one: ~s~n" (cadr line))
               (set! line-num (+ line-num 1) ) )
          ((and (not (pair? (cadr line)) ) (pair? (caddr line) ) )
              ;(printf "Line w/ label processesed: ~s~n" (caddr line))
              (set! line-num (process-stmt (caddr line) line-num)) )
          ((pair? (cadr line))      ;No label statement
              ;(printf "Line being processes: ~s~n" (cadr line))
              (set! line-num (process-stmt (cadr line) line-num)) )
          (else                     ;Statement with a label
              (printf "Dont know this one: ~s~n" (line))
              (void)) )

    ;Process next
    (interpret-program line-num)
)

;;-----------------------------MAIN FUNCTION---------------------------

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
              (silly-basic-int sbprogfile program))))

(main (vector->list (current-command-line-arguments)))


