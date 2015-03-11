
(define-library (tl1 parser)
  (export tl1-parse)
  (import (scheme base)
          (scheme write)
          (tl1 lexer)
          (tl1 peg))
  (begin

    (define (fold kons knil e)
      (let lp ((lis e) (ans knil))
        (if (null? lis) ans
            (lp (cdr lis) (kons (car lis) ans)))))

    (define procs (make-parameter '()))
    (define funcs (make-parameter '()))
    (define gvars (make-parameter '()))
    (define garrs (make-parameter '()))
    (define lvars (make-parameter '()))
    (define larrs (make-parameter '()))
    (define situation (make-parameter 'global))

    (define %ident ($one-of symbol?))

    (define ($reserved sym)
      ($let* s ((ident %ident))
        (if (and (eqv? sym ident)
                 (not (or (assq ident (larrs))
                          (memq ident (lvars))
                          (assq ident (garrs))
                          (memq ident (gvars))
                          (memq ident (funcs))
                          (memq ident (procs)))))
            ($return sym)
            ($fail sym))))

    (define %array
      ($let* s ((ident %ident))
        (if (or (assq ident (larrs))
                (and (not (memq ident (lvars)))
                     (assq ident (garrs))))
            ($return ident)
            ($fail ident))))

    (define %var
      ($let* s ((ident %ident))
        (let ((laf (assq ident (larrs)))
              (lvf (memq ident (lvars)))
              (gaf (assq ident (garrs)))
              (gvf (memq ident (gvars))))
          (if (or (and (not laf) lvf)
                  (and (not laf) (not lvf) (not gaf) gvf))
              ($return ident)
              ($fail ident)))))

    (define %func
      ($let* s ((ident %ident))
        (if (and (not (assq ident (larrs)))
                 (not (memq ident (lvars)))
                 (not (assq ident (garrs)))
                 (not (memq ident (gvars)))
                 (memq ident (funcs)))
            ($return ident)
            ($fail ident))))

    (define %proc
      ($let* s ((ident %ident))
        (if (and (not (assq ident (larrs)))
                 (not (memq ident (lvars)))
                 (not (assq ident (garrs)))
                 (not (memq ident (gvars)))
                 (not (memq ident (funcs)))
                 (memq ident (procs)))
            ($return ident)
            ($fail ident))))

    (define (duplicate-check lst)
      (if (null? lst)
          #f
          (or (memq (car lst) (cdr lst))
              (duplicate-check (cdr lst)))))

    (define %ident-list
      ($let* ((ids ($split-by ($expect %ident "identifier") ($eqv #\,))))
        ($return ids)))

    (define %declare-procedures
      ($let* ((proc ($eqv 'proc))
              (ids %ident-list))
        ($return ids)))

    (define %declare-functions
      ($let* ((proc ($eqv 'func))
              (ids %ident-list))
        ($return ids)))

    (define %declare-variables
      ($let* ((proc ($eqv 'var))
              (ids %ident-list))
        ($return ids)))

    (define %declare-arrays
      ($let* ((proc ($eqv 'array))
              (ars ($split-by
                    ($let* ((id %ident)
                            (_ ($expect ($eqv #\[) "["))
                            (ix ($expect ($one-of number?) "number"))
                            (_ ($expect ($eqv #\]) "]")))
                      ($return (list id ix)))
                    ($eqv #\,))))
        ($return ars)))

    (define %expr
      ($lazy %additive-with-curry))

    (define %return
      ($let* ((_ ($reserved 'return)))
        (if (eqv? (situation) 'function)
            ($let* ((expr ($expect %expr "expression")))
              ($return `(return ,expr)))
            ($return '(return)))))

    (define %statement
      ($lazy
       ($select
        %procedure-call
        ($reserved 'stop)
        %return
        %if-statement
        %block
        %for-statement
        %repeat-statement
        %while-statement
        %case-statement
        %write-statement
        %assignment
        )))

    (define %constant
      ($select ($one-of number?)
               ($let* ((n ($reserved 'true))) ($return 255))
               ($let* ((n ($reserved 'false))) ($return 0))))

    (define %system-func
      ($let* s ((ident %ident))
        (if (and (not (assq ident (larrs)))
                 (not (memq ident (lvars)))
                 (not (assq ident (garrs)))
                 (not (memq ident (gvars)))
                 (not (memq ident (funcs)))
                 (let-syntax
                     ((ex (syntax-rules ()
                            ((_ n ...)
                             (or (eqv? ident 'n) ...)))))
                   (ex mhigh mod rnd get read not neg com
                       lst asr asl ror rol rdhex rrc rlc)))
            ($return
             (string->symbol (string-append (symbol->string ident) "_sys")))
            ($fail ident))))

    (define %function-call
      ($let* ((f ($select %func %system-func))
              (args ($optional
                     ($between ($eqv #\()
                               ($split-by %expr ($eqv #\,))
                               ($eqv #\)))
                     '())))
        ($return `(funcall ,f ,@args))))

    (define %array-ref
      ($let* ((a %array)
              (_ ($expect ($eqv #\[) "["))
              (e ($expect %expr "expression"))
              (_ ($expect ($eqv #\]) "]")))
        ($return `(array-ref ,a ,e))))

    (define %mem-ref
      ($let* ((_ ($reserved 'mem))
              (_ ($expect ($eqv #\() "( after mem"))
              (e1 ($expect %expr "expression"))
              (_ ($expect ($eqv #\,) ","))
              (e2 ($expect %expr "expression"))
              (_ ($expect ($eqv #\)) ")")))
        ($return `(mem-ref ,e1 ,e2))))

    (define %lvalue
      ($let* ((h ($select %var %array-ref %mem-ref))
              (r ($while
                  ($order ($eqv #\,)
                          ($expect
                           ($select %var %array-ref %mem-ref)
                           "left value")))))
        ($return (cons h r))))

    (define %factor
      ($select
       ($between ($eqv #\() %expr ($eqv #\)))
       %constant
       %var
       %function-call
       %array-ref
       %mem-ref))

    (define ($tosym x)
      ($let* s ((c ($one-of char?)))
        (if (eqv? x c)
            ($return (if (char? c) (string->symbol (string c)) c))
            ($fail c))))

    (define-syntax define-binary-operator
      (syntax-rules ()
        ((_ name parser op ...)
         (define name
           ($binary-operator-left parser ($select op ...))))))

    (define-binary-operator %multitive
      %factor ($tosym #\*) ($tosym #\/))

    (define-binary-operator %additive
      %multitive ($tosym #\+) ($tosym #\-))

    (define-binary-operator %relation
      %additive
      ($tosym #\>) ($tosym #\<) ($tosym #\#) ($tosym #\=)
      ($reserved 'gt) ($reserved 'lt))

    (define-binary-operator %logical
      %relation ($reserved 'and) ($reserved 'or) ($reserved 'eor))

    (define-binary-operator %additive-with-curry
      %logical ($reserved 'adc) ($reserved 'sbc))

    (define %for-statement
      ($let* ((_ ($reserved 'for))
              (var ($expect %var "variable"))
              (_ ($expect ($eqv ':=) ":="))
              (expr ($expect %expr "expression"))
              (direction
               ($expect ($select ($reserved 'to) ($reserved 'downto))
                        "`to' or `downto'"))
              (dest ($expect %expr "expression"))
              (_ ($expect ($reserved 'do) "`do'"))
              (st %statement))
        ($return `(for (,var ,expr ,direction ,dest) ,st))))

    (define ($statement-list end)
      ($while %statement ($select end ($error "Unknown statement"))))

    (define %block
      ($let* ((lst
               ($select
                ($order ($reserved 'begin) ($statement-list ($reserved 'end)))
                ($order ($eqv #\{) ($statement-list ($eqv #\})))
                ($order ($eqv #\[) ($statement-list ($eqv #\])))
                ($order ($eqv #\() ($statement-list ($eqv #\)))))))
        ($return `(block ,@lst))))

    (define %repeat-statement
      ($let* ((_ ($reserved 'repeat))
              (statements ($statement-list ($reserved 'until)))
              (expr ($expect %expr "expression")))
        ($return `(repeat ,expr ,@statements))))

    (define %while-statement
      ($let* ((_ ($reserved 'while))
              (expr ($expect %expr "expression"))
              (_ ($expect ($reserved 'do) "`do'"))
              (statement %statement))
        ($return `(while ,expr ,@statement))))

    (define %if-statement
      ($let* ((_ ($reserved 'if))
              (expr ($expect %expr "expression"))
              (_ ($expect ($reserved 'then) "`then'"))
              (statement1 %statement)
              (statement2 ($optional
                           ($order ($reserved 'else) %statement)
                           #f)))
        (if statement2
            ($return `(if ,expr ,statement1 ,statement2))
            ($return `(if ,expr ,statement1)))))

    (define %case-statement
      ($let* ((_ ($reserved 'case))
              (expr ($expect %expr "expression"))
              (_ ($expect ($reserved 'of) "`of'"))
              (opt ($while ($let* ((e %expr)
                                   (s %statement))
                             ($return `(,e ,s)))))
              (ec ($optional ($order ($reserved 'else) %statement) #f)))
        ($return `(case ,expr ,@opt ,@(if ec `((else ,ec)) '())))))

    (define %assignment
      ($let* ((lv %lvalue)
              (_ ($expect ($eqv ':=) ":="))
              (expr ($expect %expr "expression")))
        ($return `(assign ,lv ,expr))))

    (define %right-align
      ($let* ((_ ($order ($eqv #\#) ($expect ($eqv #\() "(")))
              (expr1 ($expect %expr "expression"))
              (_ ($expect ($eqv #\,) ","))
              (expr2 ($expect %expr "expression"))
              (_ ($expect ($eqv #\)) ")")))
        ($return `(right-align ,expr1 ,expr2))))

    (define %ascii
      ($let* ((_ ($order ($reserved 'ascii) ($expect ($eqv #\() "(")))
              (expr ($expect %expr "expression"))
              (_ ($expect ($eqv #\)) ")")))
        ($return `(ascii ,expr))))

    (define %space
      ($let* ((_ ($order ($reserved 'space) ($expect ($eqv #\() "(")))
              (expr ($expect %expr "expression"))
              (_ ($expect ($eqv #\)) ")")))
        ($return `(space ,expr))))

    (define %crlf
      ($let* ((_ ($order ($reserved 'crlf)))
              (expr ($optional
                     ($between ($eqv #\()
                               %expr
                               ($eqv #\)))
                     1)))
        ($return `(crlf ,expr))))

    (define %hex
      ($let* ((_ ($order ($reserved 'hex) ($expect ($eqv #\() "(")))
              (expr ($expect %expr "expression"))
              (_ ($expect ($eqv #\)) ")")))
        ($return `(hex ,expr))))

    (define %output-list
      ($split-by
       ($expect
        ($select
         %expr
         %right-align
         ($one-of string?)
         %ascii
         %space
         %crlf
         %hex)
        "output element")
       ($eqv #\,)))

    (define %write-statement
      ($let* ((_ ($reserved 'write))
              (_ ($expect ($eqv #\() "("))
              (dn ($expect %expr "expression"))
              (_ ($expect ($eqv #\:) ":"))
              (olist %output-list)
              (_ ($expect ($eqv #\)) ")")))
        ($return `(write ,dn ,@olist))))

    (define %procedure-call
      ($let* ((proc %proc)
              (args ($optional
                     ($between ($eqv #\()
                               ($split-by %expr ($eqv #\,))
                               ($expect ($eqv #\)) ")"))
                     '())))
        ($return `(procall ,proc ,@args))))

    (define %main-program
      ($let* ((_ ($expect ($eqv 'begin) "begin"))
              (sts ($statement-list ($reserved 'end))))
        ($return sts)))

    (define %parameter-list
      ($between ($eqv #\()
                ($split-by %ident ($eqv #\,))
                ($expect ($eqv #\)) ")")))

    (define %define-function
      ($let* ((n ($select %func))
              (args ($optional %parameter-list '()))
              (lv ($optional %declare-variables '()))
              (la ($optional %declare-arrays '()))
              (_ ($reserved 'begin)))
        ($parameterize ((lvars (append args lv))
                        (larrs la)
                        (situation 'function))
          ($let* ((sts ($statement-list ($reserved 'end))))
            ($return `(define-function (,n ,@args)
                        (vars ,@lv) (arrays ,@la)
                        ,@sts))))))

    (define %define-procedure
      ($let* ((n ($select %proc))
              (args ($optional %parameter-list '()))
              (lv ($optional %declare-variables '()))
              (la ($optional %declare-arrays '()))
              (_ ($reserved 'begin)))
        ($parameterize ((lvars (append args lv))
                        (larrs la)
                        (situation 'procedure))
          ($let* ((sts ($statement-list ($reserved 'end))))
            ($return `(define-procedure (,n ,@args)
                        (vars ,@lv) (arrays ,@la)
                        ,@sts))))))

    (define %sub-program
      ($select %define-function %define-procedure))

    (define %sub-program-list
      ($while %sub-program))

    (define %program
      ($let* ((p ($optional %declare-procedures '()))
              (f ($optional %declare-functions '()))
              (gv ($optional %declare-variables '()))
              (ga ($optional %declare-arrays '())))
        (if (or (duplicate-check p)
                (duplicate-check f)
                (duplicate-check gv)
                (duplicate-check ga))
            ($error "There are duplicate names in declare list")
            ($parameterize ((procs p) (funcs f) (gvars gv) (garrs ga))
              ($let* ((mp %main-program)
                      (sps ($null %sub-program-list)))
                ($return
                 `((proc ,@p) (func ,@f) (vars ,@gv) (arrays ,@ga)
                   (define-procedure (_main) ,@mp)
                   ,@sps)))))))

    (define (tl1-parse port)
      (let* ((tokens (tl1-tokenize port)))
        (let-values (((status result rest)(%program tokens)))
          (case status
            ((success) result)
            ((fail) (error "parse error" result rest))
            ((error) (error result (if (null? rest) #f (car rest))))
            ((expect)
             (error (string-append "expected " result " but ")
                    (car rest)))))))
    ))
