
(define-library (tl1 c-gen)
  (export make-declares make-defines ast->c)
  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (tl1 parser)
          (tl1 exception)
          (tl1 pattern-match))

  (begin
    (define arity (make-parameter #f))

    (define (print . x)
      (for-each (lambda(x)(display x)) x))

    (define (declare-parameter params)
      (print "(")
      (if (null? params)
          (print "void")
          (begin
            (print "unsigned char v_" (car params))
            (for-each (lambda(x)(print ",unsigned char v_" x)) (cdr params))))
      (print ")"))

    (define (declare-procedure name params)
      (print "void p_" name)
      (declare-parameter params))

    (define (declare-function name params)
      (print "unsigned char f_" name)
      (declare-parameter params))

    (define (make-declares toplist)
      (let ((arity '()))
        (for-each
         (lambda(lst)
           (pattern-match lst (vars arrays define-procedure define-function)
             ((vars x . xs)
              (print "unsigned char v_" x)
              (for-each (lambda(x) (print  ",v_" x)) xs)
              (print ";\n"))
             ((arrays (n s) . xs)
              (print "unsigned char a_" n "[" s "+1]")
              (for-each (lambda(x)(print ",a_" (car x) "[" (cadr x) "+1]")) xs)
              (print ";\n"))
             ((define-procedure (name . params) . body)
              (declare-procedure name params)
              (print ";\n")
              (set! arity (cons (cons name (length params)) arity)))
             ((define-function (name . params) . body)
              (declare-function name params)
              (print ";\n")
              (set! arity (cons (cons name (length params)) arity)))))
         toplist)
        arity))

    (define (expression expr)
      (pattern-match expr (adc sbc and or eor gt lt > < = + - * / |#|
                               array-ref mem-ref funcall)
                               ((adc x y)
                               (print "tl1_adc(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((sbc x y)
                               (print "tl1_sbc(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((eor x y)
                               (print "tl1_eor(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((or x y)
                               (print "tl1_or(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((and x y)
                               (print "tl1_and(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((gt x y)
                               (print "tl1_gt(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((lt x y)
                               (print "tl1_lt(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((> x y)
                               (print "tl1_ugt(")
                               (expression x) (print ",") (expression y)
                               (print ")"))
                               ((< x y)
                               (print "tl1_ult(")
                               (expression x) (print  ",") (expression y)
                               (print ")"))
                               ((|#| x y)
                     (print "tl1_diff(")
                     (expression x) (print ",") (expression y)
                     (print ")"))
      ((= x y)
       (print "tl1_eq(")
       (expression x) (print ",") (expression y)
       (print ")"))
      ((+ x y)
       (print "tl1_plus(")
       (expression x) (print ",") (expression y)
       (print ")"))
      ((- x y)
       (print "tl1_minus(")
       (expression x) (print ",") (expression y)
       (print ")"))
      ((* x y)
       (print "tl1_mul(")
       (expression x) (print ",") (expression y)
       (print ")"))
      ((/ x y)
       (print "tl1_div(")
       (expression x) (print ",") (expression y)
       (print ")"))
      ((array-ref name e)
       (print "a_" name "[")
       (expression e)
       (print "]"))
      ((funcall name . args)
       (let ((t (assv name (arity))))
         (if t (if (not (= (cdr t) (length args)))
                   (raise-tl1-error "Call function with incorrect arity."
                                    name))
             (raise-tl1-error "Call undefined function."
                              name)))
       (funcall name args))
      ((mem-ref e1 e2)
       (print "tl1_mem[256*") (expression e1) (print "+") (expression e2)
       (print "]"))
      (factor
       (cond
        ((symbol? factor) (print "v_" factor))
        ((number? factor) (print factor))))
      ))

  (define (procall name args)
    (print "p_" name "(")
    (unless (null? args)
      (expression (car args))
      (for-each (lambda(x) (print ",") (expression x)) (cdr args)))
    (print ");\n"))

  (define (funcall name args)
    (print "f_" name "(")
    (unless (null? args)
      (expression (car args))
      (for-each (lambda(x) (print ",") (expression x)) (cdr args)))
    (print ")"))

  (define (case-statement val opts)
    (print "{unsigned char temp=")
    (expression val)
    (print ";")
    (print "if(")
    (expression (caar opts))
    (print "==temp)")
    (statement (cadar opts))
    (for-each
     (lambda(opt)
       (pattern-match opt (else)
         ((else body)
          (print "else ") (statement body))
         ((c body)
          (print "else if(")
          (expression c)
          (print "==temp)")
          (statement body))))
     (cdr opts))
    (print "}"))

  (define (write-statement dev args)
    (for-each
     (lambda(s)
       (pattern-match s (right-align
                         ascii
                         space
                         crlf
                         hex)
        ((right-align c e)
         (print "tl1_putright(")
         (expression c)
         (print ",")
         (expression e)
         (print ");\n"))
        ((ascii e)
         (print "tl1_putch(") (expression e) (print ");\n"))
        ((space e)
         (print "for(int i=0; i<")
         (expression e)
         (print ";i++){tl1_putch(' ');}"))
        ((crlf e)
         (print "for(int i=0; i<")
         (expression e)
         (print ";i++){tl1_putch('\\n');}"))
        ((hex e)
         (print "tl1_puthex(")
         (expression e)
         (print ");\n"))
        (e
         (if (string? e)
             (begin (print "tl1_putstr(\"") (print e) (print "\");\n"))
             (begin (print "tl1_putnum(")
                    (expression e)
                    (print ");\n"))))))
     args))
  
  (define (statement s)
    (pattern-match s (return for to downto repeat until while if
                             case write assign funcall procall stop
                             block vars arrays)
      ((procall name . args)
       (let ((t (assv name (arity))))
         (if t (if (not (= (cdr t) (length args)))
                   (raise-tl1-error "Call function with incorrect arity."
                                    name))
             (raise-tl1-error "Call undefined function." name)))
       (procall name args))
      ((block . body) (block body))
      ((return) (print "return;\n"))
      ((return e) (print "return ") (expression e) (print ";\n"))
      ((if condition t e)
       (print "if(") (expression condition) (print ")")
       (statement t)
       (print "else ") (statement e))
      ((if condition t)
       (print "if(") (expression condition) (print ")")
       (statement t))
      ((repeat condition . body)
       (print "do{\n") (block body) (print "} while(!")
       (expression condition) (print ");\n"))
      ((for (v s to e) body)
       (print "{v_" v "=") (expression s)
       (print ";unsigned char temp=") (expression e)
       (print ";while(1){")
       (statement body)
       (print "if(v_" v "++==temp) break;}}"))
      ((for (v s downto e) body)
       (print "for(v_" v "=") (expression s) (print ";v_" v ">=")
       (expression e) (print ";v_" v "--)") (statement body))
      ((assign lvalues rvalue)
       (for-each (lambda(v)(expression v)(print "=")) lvalues)
       (expression rvalue) (print ";\n"))
      ((write dev . a)
       (write-statement dev a))
      ((stop)
       (print "tl1_stop();\n"))
      ((while e body)
       (print "while(") (expression e) (print ")")
       (statement body))
      ((case val . opts)
       (case-statement val opts))
      ((vars . s)
       (for-each (lambda(v) (print "unsigned char v_" v ";\n")) s))
      ((arrays . s)
       (for-each (lambda(v)
                   (print "unsigned char a_" (car v) "[" (cadr v) "+1];"))
                 s))
      ))

  (define (block body)
    (print "{\n")
    (for-each statement body)
    (print "}\n"))

  (define (define-procedure name params body)
    (declare-procedure name params)
    (block body))

  (define (define-function name params body)
    (declare-function name params)
    (block body))

  (define (make-defines toplist)
    (for-each
     (lambda(lst)
       (pattern-match lst (define-procedure define-function)
         ((define-procedure (name . params) . body)
          (define-procedure name params body))
         ((define-function (name . params) . body)
          (define-function name params body))))
     toplist))

  (define header
    "
extern unsigned char tl1_mem[256*256];
void tl1_putch(unsigned char ch);
void tl1_putstr(char* s);
void tl1_putnum(unsigned char x);
void tl1_puthex(unsigned char x);
void tl1_putright(unsigned char c, unsigned char x);
unsigned char tl1_adc(unsigned char x, unsigned char y);
unsigned char tl1_sbc(unsigned char x, unsigned char y);
unsigned char tl1_eor(unsigned char x, unsigned char y);
unsigned char tl1_or(unsigned char x, unsigned char y);
unsigned char tl1_and(unsigned char x, unsigned char y);
unsigned char tl1_gt(unsigned char x, unsigned char y);
unsigned char tl1_lt(unsigned char x, unsigned char y);
unsigned char tl1_ugt(unsigned char x, unsigned char y);
unsigned char tl1_ult(unsigned char x, unsigned char y);
unsigned char tl1_diff(unsigned char x, unsigned char y);
unsigned char tl1_eq(unsigned char x, unsigned char y);
unsigned char tl1_plus(unsigned char x, unsigned char y);
unsigned char tl1_minus(unsigned char x, unsigned char y);
unsigned char tl1_mul(unsigned char x, unsigned char y);
unsigned char tl1_div(unsigned char x, unsigned char y);
void tl1_stop(void);
unsigned char f_mod_sys(void);
unsigned char f_get_sys(unsigned char x);
unsigned char f_mhigh_sys(void);
unsigned char f_mod_sys(void);
unsigned char f_mhigh_sys(void);
unsigned char f_rnd_sys(unsigned char x);
unsigned char f_not_sys(unsigned char x);
unsigned char f_neg_sys(unsigned char x);
unsigned char f_com_sys(unsigned char x);
unsigned char f_lsr_sys(unsigned char x);
unsigned char f_asr_sys(unsigned char x);
unsigned char f_asl_sys(unsigned char x);
unsigned char f_ror_sys(unsigned char x);
unsigned char f_rol_sys(unsigned char x);
")

  (define arity-system-subprogram
    '((mhigh_sys . 0)
      (mod_sys . 0)
      (rnd_sys . 1)
      (get_sys . 1)
      (read_sys . 1)
      (not_sys . 1)
      (neg_sys . 1)
      (com_sys . 1)
      (lsr_sys . 1)
      (asr_sys . 1)
      (asl_sys . 1)
      (ror_sys . 1)
      (rol_sys . 1)
      (rdhex_sys . 1)
      (rrc_sys . 1)
      (rlc_sys . 1)))

  (define (ast->c ast)
    (display header)
    (parameterize((arity (append (make-declares ast) arity-system-subprogram)))
      (make-defines ast))
    )
  ))
