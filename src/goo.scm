#! /home/pierre/local/bin/gsi -:s
;;; goo.scm

(define (if? exp) (and (pair? exp) (eq? (car exp) 'if)))
(define (def? exp) (and (pair? exp) (eq? (car exp) 'def)))
(define (let? exp) (and (pair? exp) (eq? (car exp) 'let)))
(define (loc? exp) (and (pair? exp) (eq? (car exp) 'loc)))
(define (seq? exp) (and (pair? exp) (eq? (car exp) 'seq)))
(define (fun? exp) (and (pair? exp) (eq? (car exp) 'fun)))
(define (defmacro? exp) (and (pair? exp) (eq? (car exp) 'defmacro)))
(define (defclass? exp) (and (pair? exp) (eq? (car exp) 'defclass)))
(define (set!? exp) (and (pair? exp) (eq? (car exp) 'set)))
(define (use? exp) (and (pair? exp) (eq? (car exp) 'use)))
(define (export? exp) (and (pair? exp) (eq? (car exp) 'export)))

(define (ev-goo exp env)
  ;(display ";;eval ") (display exp) (newline)
  (cond ((literal? exp) (ev-goo-literal exp env))
        ((symbol? exp) (ev-goo-variable exp env))
        ((if? exp) (ev-goo-if exp env))
        ((def? exp) (ev-goo-def exp env))
        ((let? exp) (ev-goo-let exp env))
	((loc? exp) (ev-goo-loc exp env))
        ((seq? exp) (ev-goo-seq exp env))
        ((fun? exp) (ev-goo-fun exp env))
        ((defmacro? exp) (ev-goo-macro exp env))
        ((defclass? exp) (ev-goo-class exp env))
	((set!? exp) (ev-goo-set! exp env))
        ((use? exp) (ev-goo-use exp env))
        ((export? exp) (ev-goo-export exp env))
        ((pair? exp) (ev-goo-application exp env))
        (else (error "unknown expression ~a to evaluate" exp))))

(define (name->filename name) (string-append name ".goo"))

(define *module-path* '("./"))

(define (locate-file fn)
  (let lp ((paths *module-path*))
;    (display paths) (newline)
    (if (null? paths)
	(error "Couldn't find module ~a" fn)
	(let* ((path (car paths))
	       (filename (string-append path "/" fn)))
	  ;(display ";; testing") (display filename) (newline)
	  (if (file-exists? filename)
	      filename
	      (lp (cdr paths)))))))

(define (ev-goo-use-module name env)
  (let* ((module (find-module name)))
    (if module
        (set-module-import! (env-module env)
                            (cons name (module-import (env-module env))))
        (let ((module (make-module name '() '() '() '())))
          (display ";; loading module ") (display name) (newline)
          (load-goo-file (locate-file (name->filename (symbol->string name))) module)
          (set-module-import! (env-module env)
                              (cons name (module-import (env-module env))))
          (bind-module! module)))))
  
(define (ev-goo-use exp env)
  (for-each (lambda (e) (ev-goo-use-module e env)) (cdr exp)))

(define (ev-goo-export exp env)
  (let ((module (env-module env)))
    (set-module-export! module (append (cdr exp) (module-export module)))))

;;; set!

(define (setter-name basename)
  (string->symbol (string-append (symbol->string basename) "-setter")))

(define (ev-goo-set! exp env)
  (let ((dest (cadr exp))
	(source (caddr exp)))
    (if (pair? dest)
	(let ((setter (setter-name (car dest))))
	  (ev-goo-application `(,setter ,source ,@(cdr dest)) env))
	(set-var! dest (ev-goo source env) env))))

(define (set-var! name value env)
  (let ((binding (assq name (env-bindings env))))
    (if binding
	(set-cdr! binding value)
	(error "unknown variable ~a" name))))

;;; class

(define (ev-goo-class-prop name prop env)
  (let* ((prop-name (car prop))
	 (prop-type (cadr prop))
	 (prop-init (caddr prop))
	 (prop-getter-name prop-name)
	 (prop-setter-name (setter-name prop-getter-name)))
    (bind-global! (env-module env) prop-getter-name
		  (make-method prop-getter-name
			       '(instance)
			       (list (ev-goo name env))
			       #f #f
			       env
			       (lambda (i) (instance-ref i prop-getter-name))))
    (bind-global! (env-module env) prop-setter-name
		  (make-method prop-setter-name
			       '(value instance)
			       (list (ev-goo prop-type env)
				     (ev-goo name env))
			       #f #f
			       env
			       (lambda (v i) (instance-set! i prop-getter-name v))))))

(define (ev-goo-class-props name props env)
  (for-each (lambda (p) (ev-goo-class-prop name p env)) props))

(define (ev-goo-class-init name props env)
  (if (not (null? props))  
      (let ((vars (map car props))
	    (init (map caddr props)))
	(ev-goo `(def init ((i ,name) &key ,@(map list vars init))
		      ,@(map (lambda (v) `(set (,v i) ,v)) vars))
		env))))

(define (ev-goo-class-make name supers props env)
  (let ((props* (map (lambda (p) (make-prop (car p)
					    (ev-goo (cadr p) env)
					    (ev-goo (caddr p) env)
					    #f #f))
		     props)))
    (bind-global! (env-module env) name
		  (create-class name
				(if (eq? supers '()) (list :any) supers)
				props*))))
  
(define (ev-goo-class exp env)
  (let ((name (cadr exp))
        (supers (map (lambda (e) (ev-goo e env)) (caddr exp)))
        (props (if (pair? (cdddr exp)) (cadddr exp) '())))
    (ev-goo-class-make name supers props env)
    (ev-goo-class-props name props env)
    (ev-goo-class-init name props env)))

;;; macro

(define (ev-goo-macro exp env)
  (let ((name (cadr exp))
        (args (caddr exp))
        (code (cdddr exp)))
    (bind-global! (env-module env) name (make-macro name (make-method name args (list :list) #f #f env code)))))

;;; literal

(define (keyword? obj)
  (and (symbol? obj)
       (let ((str (symbol->string obj)))
	 (and (> (string-length str) 0)
	      (char=? #\: (string-ref str 0))))))
	
(define (literal? exp)
  (or (string? exp)
      (char? exp)
      (number? exp)
      (eq? exp #t)
      (eq? exp #f)
      (keyword? exp)
      (eq? exp '())
      (and (pair? exp) (eq? (car exp) 'quote))))

(define (ev-goo-literal exp env)
  (if (and (pair? exp) (eq? (car exp) 'quote))
      (cadr exp)
      exp))

;;; variable evaluation

(define (make-env module bindings) (cons module bindings))
(define (env-module env) (car env))
(define (env-bindings env) (cdr env))

(define (lookup-defined var module)
  (let ((entry (assq var (module-defined module))))
    (if entry
	(begin
	  ;; (display ";; var ") (display var) (display " found in ")
	  ;; (display (module-name module)) (newline)
	  entry)
	#f)))

(define (lookup var env)
  (let ((entry (assq var (env-bindings env))))
    (if entry
        (cdr entry)
        (let ((entry (lookup-defined var (env-module env))))
          (if entry
              (cdr entry)
	      (let ((entry (lookup-exported var (env-module env))))
		(if entry
		    (cdr entry)
		    (error "unbound variable ~a in" var env))))))))

(define (ev-goo-variable exp env)
  (lookup exp env))

;;; if form evaluation

(define (ev-goo-if exp env)
  (let ((value (ev-goo (cadr exp) env)))
    (if value
        (ev-goo (caddr exp) env)
        (ev-goo (cadddr exp) env))))

;;; def form

(define *goo-modules* '())

(define (make-module name export import defined referenced)
  (vector 'module name export import defined referenced))

(define (module? obj) (and (vector? obj) (eq? 'module (vector-ref obj 0))))
(define (module-name m) (vector-ref m 1))
(define (module-export m) (vector-ref m 2))
(define (set-module-export! m e) (vector-set! m 2 e))
(define (module-import m) (vector-ref m 3))
(define (set-module-import! m mod) (vector-set! m 3 mod))
(define (module-defined m) (vector-ref m 4))
(define (set-module-defined! m v) (vector-set! m 4 v))
(define (module-referenced m) (vector-ref m 5))

(define (find-module name)
  ;(display ";; finding module ") (display name) (newline)
  (let lp ((mods *goo-modules*))
    (cond ((null? mods) #f)
          ((eq? name (module-name (car mods)))
           (car mods))
          (else (lp (cdr mods))))))

(define (bind-module! module)
  (set! *goo-modules* (cons module *goo-modules*)))

(define (lookup-exported name module)
  (let see-in ((mods (map find-module (module-import module))))
    ;(display ";; lookup exported ") (display name) (display (map module-name mods))
    ;(newline)
    (if (null? mods)
	#f
	(or (let ((e (lookup-exported-name name (car mods))))
	      ;; (if e
	      ;; 	  (begin (display ";; found exported ") (display name)
	      ;; 		 (display " in mod ") (display (module-name (car mods))) (newline)))
	      e)
	    (see-in (cdr mods))))))

(define (lookup-exported-name name module)
  ;(display ";; lookup exported name ") (display name) (display (module-name module))
  ;(newline)
  (and (memq name (module-export module))
       (or (assq name (module-defined module))
	   (lookup-exported name module))))

(define *runtime-sig*
  '(<any> <type> <class> <union> <singleton> <subclass>
          <mag> <num> <int> <float> <log> <chr>
          <str> <sym> <col> <seq> <lst> <vector> <pair> <nil> <fun>
          <met> <gen> <input-port> <output-port>
          %isa? %subtype? %alloc-instance make %t= %t< %type-class %t+ union-elts %t?
          %class-of %class-name %class-parents %class-direct-props %class-props

	  %eq? %int->char %char->int %+ %- %* %/ %> %< %>= %<=
	  %round %floor %ceil %trunc %mod %div %rem
 
          %fun-name %fun-specs %fun-nary? %fun-arity %apply %apply2
	  %fun-methods
	  %generic-add-method! %sorted-application-methods %method-applicable?
	  %sup

          %pair %head %tail %null?
          %make-vector %vector-ref %vector-set! %vector-length
	  %make-string %string-ref %string-set! %string-length

          %read %write
          %open-input-file %open-output-file

	  %dbg
          ))

(define *goo-runtime* (make-module 'primitives *runtime-sig* '() '() '()))

(bind-module! *goo-runtime*)

(define (bind-global! module name value)
  (let ((binding (assq name (module-defined module))))
    (if binding
        (set-cdr! binding value)
	(let ((defs (module-defined module)))
	  (set-module-defined! module (cons (cons name value) defs))))))
  
(define (bind-def! module name value)
  (let ((entry (or (lookup-defined name module)
		   (lookup-exported name module))))
    (if entry
        (let ((old (cdr entry)))
          (cond ((method? old)
		 (set-cdr! entry (make-generic name (method-args old)
					       (list old value))))
                ((generic? old)
                 (generic-add-method! old value))
                (else (display ";; redefinition of ") (display name) (newline)
		      (set-cdr! entry value))))
	(bind-global! module name value))))

(define (ev-goo-def exp env)
  (let ((name (cadr exp))
        (formals/exp (caddr exp)))
    (if (null? (cdddr exp))
        (bind-def! (env-module env) name (ev-goo formals/exp (bind-variable name #f env)))
        (let ((method (ev-goo-fun `(fun ,formals/exp
					,@(cdddr exp))
				  env)))
	  (set-method-name! method name)
	  (bind-def! (env-module env) name method)
	  method))))

;;; binding form

(define (bind-variable var val env)
  (make-env (env-module env) (cons (cons var val) (env-bindings env))))

(define (bind-variables vars vals env)
  (if (null? vars)
      env
      (bind-variable (car vars) (car vals)
                     (bind-variables (cdr vars) (cdr vals) env))))

(define (ev-goo-let exp env)
  (let* ((binding (cadr exp))
         (vars (map car binding))
         (vals (map (lambda (b) (ev-goo (cadr b) env)) binding)))
    (ev-goo-seq* (cddr exp)
                (bind-variables vars vals env))))

(define (ev-goo-loc exp env)
  (let* ((binding (cadr exp))
         (vars (map car binding))
	 (env* (bind-variables vars (map (lambda (v) #f) vars) env))
         (vals (map (lambda (b) (set-var! (car b) (ev-goo (cadr b) env*) env*)) binding)))
    (ev-goo-seq* (cddr exp) env*)))

;;; seq form

(define (ev-goo-seq* exp env)
;  (display ";; evseq ") (display exp) (newline)
  (cond ((null? exp) #f)
        ((null? (cdr exp)) (ev-goo (car exp) env))
        (else (ev-goo (car exp) env)
              (ev-goo-seq* (cdr exp) env))))

(define (ev-goo-seq exp env) (ev-goo-seq* (cdr exp) env))

;;; fun form evaluation

(define (make-macro name expander) (vector 'macro name expander))
(define (macro? m) (and (vector? m) (eq? 'macro (vector-ref m 0))))
(define (macro-name m) (vector-ref m 1))
(define (macro-expander m) (vector-ref m 2))

(define (make-method name args specs keys rest env code)
  (vector 'method name args specs keys rest env code))
(define (method? m) (and (vector? m) (eq? 'method (vector-ref m 0))))
(define (method-name m) (vector-ref m 1))
(define (set-method-name! m n) (vector-set! m 1 n))
(define (method-args m) (vector-ref m 2))
(define (method-specs m) (vector-ref m 3))
(define (method-keys m) (vector-ref m 4))
(define (method-rest m) (vector-ref m 5))
(define (method-env m) (vector-ref m 6))
(define (method-code m) (vector-ref m 7))

(define (make-generic name args methods)
  (vector 'generic name args methods))

(define (generic? g) (and (vector? g) (eq? 'generic (vector-ref g 0))))
(define (generic-name g) (vector-ref g 1))
(define (generic-args g) (vector-ref g 2))
(define (generic-methods g) (vector-ref g 3))

(define (generic-add-method! g m)
  (vector-set! g 3 (cons m (generic-methods g))))

(define (method-arg-names args)
  (let lp ((args args))
    (cond ((null? args) args)
	  ((eq? (car args) '&rest) '())
	  ((eq? (car args) '&key) '())
	  (else (let ((a (car args)))
		  (cons (if (pair? a) (car a) a) (lp (cdr args))))))))

(define (method-arg-specs args)
  (let lp ((args args))
    (cond ((null? args) args)
	  ((eq? (car args) '&rest) '())
	  ((eq? (car args) '&key) '())
	  (else (let ((a (car args)))
		  (cons (if (pair? a) (cadr a) '<any>) (lp (cdr args))))))))

(define (symbol->keyword sym)
  (string->symbol (string-append ":" (symbol->string sym))))

(define (method-arg-keys args)
  (let ((keys (memq '&key args)))
    (if keys
	(let lp ((args (cdr keys))
		 (keys '()))
	  (cond ((null? args) keys)
		((eq? '&rest (car args))
		 keys)
		((symbol? (car args))
		 (lp (cdr args) (cons (cons (symbol->keyword (car args)) #f) keys)))
		((and (pair? (car args)) (symbol? (caar args)))
		 (lp (cdr args) (cons (cons (symbol->keyword (caar args)) (cadar args)) keys)))
		(else (error "bad key definition ~a in argument list"))))
	#f)))

(define (method-arg-rest args)
  (let ((rest (memq '&rest args)))
    (cond ((and rest (pair? (cdr rest)))
	   (cadr rest))
	  (rest (error "bad &rest argument definition ~a" args))
	  (else #f))))

(define (ev-goo-fun exp env)
  (let* ((args (cadr exp))
         (names (method-arg-names args))
         (specs (method-arg-specs args))
	 (keys (method-arg-keys args))
	 (rest (method-arg-rest args)))
    ;(display (list 'args names specs keys rest)) (newline)
    (make-method 'anonymous names
               (map (lambda (e) (ev-goo e env)) specs)
	       keys
	       rest
               env (cddr exp))))

(define (key-ref key args default)
  (cond ((null? args) default)
	((and (pair? args) (pair? (cdr args)) (eq? key (car args)))
	 (cadr args))
	(else (key-ref key (cddr args) default))))

(define (keyword->symbol keyword)
  (let ((str (symbol->string keyword)))
    (string->symbol (substring str 1 (string-length str)))))

(define (bind-arguments-rest-and-keys args env keys rest)
  (let ((env* (if rest
		  (bind-variable rest (list->vector (cons 'vector args)) env)
		  env)))
    (if keys
	(let lp ((env env*)
		 (keys keys))
	  (if (null? keys)
	      env
	      (let ((key (car keys)))
		(lp (bind-variable (keyword->symbol (car key))
				   (key-ref (car key) args (cdr key))
				   env)
		    (cdr keys)))))
	env*)))

(define (bind-arguments args env names keys rest)
  (cond ((and (null? args) (null? names))
	 (if (or rest keys)
	     (bind-arguments-rest-and-keys args env keys rest)
	     env))
	((null? args) (error "too few arguments"))
	((null? names)
	 (if (or rest keys)
	     (bind-arguments-rest-and-keys args env keys rest)
	     (error "too many arguments")))
	(else (bind-arguments (cdr args)
			      (bind-variable (car names)
					     (car args)
					     env)
			      (cdr names)
			      keys rest))))

(define (apply-method method args)
  (let ((names (method-args method))
        (code (method-code method))
	(keys (method-keys method))
	(rest (method-rest method))
        (env (method-env method)))
    (if (method-applicable? method args)
	(if (procedure? code)
	    (apply code args)
	    (ev-goo-seq* code (bind-arguments args env names keys rest)))
	(error "method ~a not applicable to ~a" method args))))

(define (ev-goo-app-method fun args types)
  (apply-method fun args))

(define (ev-goo-app-methods meth next-methods args)
  ;; (display ";; app-meth ")
  ;; (for-each (lambda (a) (display a) (display "|") (display (class-name (class-of a)))) args) (newline)
  (apply-method meth args))

(define (applicable-methods meths args)
  (if (null? meths)
      meths
      (let ((meth (car meths)))
        (if (method-applicable? meth args)
            (cons meth (applicable-methods (cdr meths) args))
            (applicable-methods (cdr meths) args)))))
            
(define (method-applicable? meth args)
  (let applicable? ((types (method-specs meth))
		    (args args))
    (if (null? types)
	#t
	(and (is? (car args) (car types))
	     (applicable? (cdr types) (cdr args))))))

(define (method-more-specific? m1 m2)
  (let specific? ((s1 (method-specs m1))
                  (s2 (method-specs m2)))
    (if (null? s1)
        s1
        (and (subtype? (car s1) (car s2))
             (specific? (cdr s1) (cdr s2))))))

(define (sorted-applicable-methods meths)
  (if (null? meths)
      '()
      (let sort ((meths (cdr meths))
                 (smallest (car meths))
                 (unordered '()))
        (if (null? meths)
            (cons smallest (sorted-applicable-methods unordered))
            (if (method-more-specific? smallest (car meths))
                (sort (cdr meths)
                      smallest
                      (cons (car meths) unordered))
                (sort (cdr meths)
                      (car meths)
                      (cons smallest unordered)))))))

(define (ev-goo-app-generic fun args types)
  (let* ((methods (generic-methods fun))
         (m1 (applicable-methods methods args))
         (m2 (sorted-applicable-methods m1)))
    ;; (display ";; gen ") (display (length methods)) (display (length m2)) (newline)
    ;; (for-each (lambda (m)
    ;; 		(display ";; sap ")
    ;; 		(for-each (lambda (s) (display s)) (method-specs m))
    ;; 		(newline))
    ;; 	      m2)
    ;; (newline)
    (if (eq? m2 '())
	(error "no applicable methods in generic ~a" fun)
	(ev-goo-app-methods (car m2) (cdr m2) args))))

(define (ev-goo-app-macro macro exp env)
  (ev-goo (apply-method (macro-expander macro) (list exp)) env))

(define (ev-goo-apply fun args)
;  (display ";; evga ")  (write (class-of fun)) (write (length args)) (newline)
  (let ((types (map class-of args)))
    (cond ((method? fun) (ev-goo-app-method fun args types))
	  ((generic? fun) (ev-goo-app-generic fun args types))
	  (else (error "cannot call ~a" fun)))))
    
(define (ev-goo-application exp env)
  (let ((fun (ev-goo (car exp) env)))
    (if (macro? fun)
        (ev-goo-app-macro fun exp env)
        (let ((args (map (lambda (e) (ev-goo e env)) (cdr exp))))
	  (ev-goo-apply fun args)))))

;;; class

(define (all? pred? lst)
  (or (null? lst)
      (and (pred? (car lst)) (all? pred? (cdr lst)))))

(define (any? pred? lst)
  (if (null? lst)
      #f
      (or (pred? (car lst)) (any? pred? (cdr lst)))))

(define (make-singleton value) (vector 'singleton value))
(define (singleton? obj) (and (vector? obj) (eq? 'singleton (vector-ref obj 0))))
(define (singleton-value singleton) (vector-ref singleton 1))

(define (make-union . types) (vector 'union types))
(define (union? obj) (and (vector? obj) (eq? 'union (vector-ref obj 0))))
(define (union-types union) (vector-ref union 1))

(define (make-subclass class) (vector 'subclass class))
(define (subclass? class) (and (vector? class) (eq? 'subclass (vector-ref class 0))))
(define (subclass-class sub) (vector-ref sub 1))

(define (make-class name supers direct-props props)
  (vector 'class name supers direct-props props))

(define (class? c) (and (vector? c) (eq? 'class (vector-ref c 0))))
(define (class-name c) (vector-ref c 1))
(define (class-supers c) (vector-ref c 2))
(define (class-direct-props c) (vector-ref c 3))
(define (class-props c) (vector-ref c 4))
(define (set-class-props! c v) (vector-set! c 4 v))

(define (compute-class-props class)
  (let collect ((classes (list class)))
    (if (null? classes)
        classes
        (let ((class (car classes)))
          (append (class-direct-props class)
                  (collect (append (cdr classes) (class-supers class))))))))

(define (create-class name supers props)
  (let ((class (make-class name supers props #f)))
    (set-class-props! class (compute-class-props class))
    class))

(define (is? obj class)
  (cond ((class? class) (subtype? (class-of obj) class))
	((singleton? class) (eq? obj (singleton-value class)))
	((union? class) (any? (lambda (t) (is? obj t)) (union-types union)))
	((subclass? class) (and (is? obj :class) (subtype? obj (subclass-class obj))))
	(else #f)))

;;; TODO fixe bugs
(define (subtype-class? t1 t2)
  (cond ((class? t1)
	 (or (eq? t1 t2)
	     (any? (lambda (t) (subtype? t t2)) (class-supers t1))))
	((singleton? t1)
	 (is? (singleton-value t1) t2))
	(else #f)))

(define (subtype-singleton? t1 t2)
  (and (singleton? t1)
       (eq? (singleton-value t1)
	    (singleton-value t2))))

(define (subtype? t1 t2)
  ;;  (display ";; subtype ") (display t1) (display t2) (newline)
  (cond ((class? t2) (subtype-class? t1 t2))
	((singleton? t2) (subtype-singleton? t1 t2))
	((union? t2) (any? (lambda (t) (subtype? t1 t2)) (union-types t2)))
	((subclass? t2) (subtype? t1 (subclass-class t2)))
	(else (error "unknown type relation" t1 t2))))

(define (class-of obj)
  (cond ((integer? obj) :int)
        ((real? obj) :float)
        ((boolean? obj) :bool)
        ((char? obj) :char)
        ((string? obj) :string)
        ((symbol? obj) :symbol)

        ((class? obj) :class)
	((union? obj) :union)
	((subclass? obj) :subclass)
	((singleton? obj) :singleton)

        ((pair? obj) :pair)
        ((null? obj) :nil)

        ((method? obj) :method)
        ((generic? obj) :generic)

	((input-port? obj) :input-port)
	((output-port? obj) :output-port)

	((instance? obj) (instance-class obj))
	((vector? obj) :vector)

        (else (error "unknown class of object ~a" obj))))

(define (alloc-instance class)
  (let* ((size (length (class-props class))))
    (make-instance class (make-vector size #f))))

(define (make-instance class props) (vector 'instance class props))
(define (instance? i) (and (vector? i) (eq? 'instance (vector-ref i 0))))
(define (instance-class i) (vector-ref i 1))
(define (instance-props i) (vector-ref i 2))
(define (instance-ref i n)
  (let ((offset (prop-offset (instance-class i) n)))
;    (display ";; ir ") (display i) (display offset) (newline)
    (vector-ref (instance-props i) offset)))
(define (instance-set! i n v)
  (let ((offset (prop-offset (instance-class i) n)))
;    (display ";; is! ") (display i) (display offset) (newline)
    (vector-set! (instance-props i) offset v)))

(define (make-prop name type init getter setter)
  (vector 'prop name type init getter setter))
(define (prop? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'prop)))
(define (prop-name p) (vector-ref p 1))
(define (prop-type p) (vector-ref p 2))
(define (prop-init p) (vector-ref p 3))
(define (prop-getter p) (vector-ref p 4))
(define (prop-setter p) (vector-ref p 5))

(define (prop-offset class name)
  (let lp ((i 0)
	   (props (class-props class)))
    (cond ((null? props) (error "property ~a not found in ~a" name class))
	  ((eq? name (prop-name (car props))) i)
	  (else (lp (+ i 1) (cdr props))))))

;;; runtime

(define :any (create-class '<any> (list) (list)))
(define :type (create-class '<type> (list :any) (list)))
(define :class (create-class '<class> (list :type) (list)))
(define :union (create-class '<union> (list :type) (list)))
(define :singleton (create-class '<singleton> (list :type) (list)))
(define :subclass (create-class '<subclass> (list :type) (list)))

(define :mag (create-class '<mag> (list :any) (list)))
(define :num (create-class '<num> (list :mag) (list)))
(define :int (create-class '<int> (list :num) (list)))
(define :float (create-class '<float> (list :num) (list)))
(define :bool (create-class '<log> (list :any) (list)))
(define :char (create-class '<chr> (list :mag) (list)))
(define :symbol (create-class '<sym> (list :any) (list)))

(define :col (create-class '<col> (list :any) (list)))
(define :col! (create-class '<col!> (list :any) (list)))
(define :seq (create-class '<seq> (list :col) (list)))
(define :string (create-class '<str> (list :seq) (list)))

(define :list (create-class '<lst> (list :seq) (list)))
(define :pair (create-class '<pair> (list :list) (list)))
(define :nil (create-class '<nil> (list :list) (list)))

(define :vector (create-class '<vector> (list :seq) (list)))

(define :fun (create-class '<fun> (list :any) (list)))
(define :method (create-class '<met> (list :fun) (list)))
(define :generic (create-class '<gen> (list :fun) (list)))

(define :port (create-class '<port> (list :type) (list)))
(define :input-port (create-class '<input-port> (list :port) (list)))
(define :output-port (create-class '<output-port> (list :port) (list)))

(define-syntax def
  (syntax-rules ()
    ((def ?name ((?arg ?type) ...) . ?body)
     (bind-def! *goo-runtime* '?name (make-method '?name
						  '(?arg ...)
						  (list (ev-goo '?type (make-env *goo-runtime* '())) ...)
						  #f
						  #f
						  #f (lambda (?arg ...) . ?body))))))

(define-syntax def!
  (syntax-rules ()
    ((def! ?name ?val) (bind-global! *goo-runtime* '?name ?val))))

(def! <any> :any)
(def! <type> :type)
(def! <class> :class)
(def! <union> :union)
(def! <singleton> :singleton)
(def! <subclass> :subclass)

(def! <mag> :mag)
(def! <num> :num)
(def! <int> :int)
(def! <float> :float)
(def! <log> :bool)
(def! <chr> :char)
(def! <str> :string)
(def! <sym> :symbol)

(def! <col> :col)
(def! <seq> :seq)

(def! <lst> :list)
(def! <pair> :pair)
(def! <nil> :nil)

(def! <vector> :vector)
(def! <fun> :fun)
(def! <met> :method)
(def! <gen> :generic)

(def! <input-port> :input-port)
(def! <output-port> :output-port)

(def %load ((s <str>)) (eval-goo-file s))

;;; primitive

;; type -- primitives
(def %isa? ((x <any>) (y <type>)) (is? x y))
(def %subtype? ((x <type>) (y <type>)) (subtype? x y))
(def %class-of ((x <any>)) (class-of x))

(def %t= ((x <any>)) (make-singleton x))

;; (def <subclass>

(def %t< ((class <class>)) (make-subclass class))

(def %type-class ((x <subclass>)) (subclass-class x))

;; (def <union>

(def %t+ ((x <type>) (y <type>)) (make-union (list x y)))

;(def union-elts ((x <union>)) (union-types x))

(def %t? ((type <type>)) (make-union (make-singleton #f) type))

;; (def <product>

;; (def t*

;; (def product-elts ((x <product>))

;; (def <class>

(def %alloc-instance ((x <class>)) (alloc-instance x))
(def %class-name ((x <class>)) (class-name x))

(def %class-parents ((x <class>)) (class-supers x))

;; (def class-ancestors ((x <class>)

(def %class-direct-props ((x <class>)) (class-direct-props x))

(def %class-props ((x <class>)) (class-props x))

;; scalar -- primitives
(def %eq? ((x <any>) (y <any>)) (eq? x y))
(def %int->char ((x <int>)) (integer->char x))
(def %char->int ((x <chr>)) (char->integer x))
(def %str->int ((x <str>)) (string->number x))
(def %int->str ((x <int>)) (number->string x))
(def %+ ((x <num>) (y <num>)) (+ x y))
(def %- ((x <num>) (y <num>)) (- x y))
(def %* ((x <num>) (y <num>)) (* x y))
(def %/ ((x <num>) (y <num>)) (/ x y))

(def %> ((x <num>) (y <num>)) (> x y))
(def %< ((x <num>) (y <num>)) (< x y))
(def %>= ((x <num>) (y <num>)) (>= x y))
(def %<= ((x <num>) (y <num>)) (<= x y))

(def %round ((x <num>)) (round x))

(def %floor ((x <num>)) (floor x))
(def %ceil ((x <num>)) (ceil x))
(def %trunc ((x <num>)) (trunc x))
(def %mod ((x <num>) (y <num>)) (modulo x y))
(def %div ((x <num>) (y <num>)) (quotient x y))
(def %rem ((x <num>) (y <num>)) (remainder x y))

;; fun -- primitives
(def %fun-name ((x <fun>)) (fun-name x))
(def %fun-specs ((x <fun>)) (fun-specs x))
(def %fun-nary? ((x <fun>)) (fun-nary? x))
(def %fun-arity ((x <fun>)) (fun-arity x))
(def %apply ((f <fun>) (args <any>)) (ev-goo-apply f args))
(def %apply2 ((f <fun>) (arg <any>) (args <any>))
     (ev-goo-apply f (cons arg (cdr (vector->list args)))))
(def %fun-methods ((x <gen>)) (fun-methods x))

(def %generic-add-method! ((x <gen>) (y <met>)) (generic-add-method! x y))
(def %sorted-applicable-methods ((x <gen>) (args <any>)) (sorted-applicable-methods x args))
(def %method-applicable? ((x <met>) (args <any>)) (method-applicable? x args))

(def %sup () (error "SUP not implemented"))

;; io -- primitives
(def %dbg ((o <any>)) (write o))
(def %read ((p <input-port>)) (read-goo p))
(def %write ((p <output-port>) (o <any>)) (write o p))
(def %open-input-file ((fn <str>)) (open-output-file fn))
(def %open-output-file ((fn <str>)) (open-input-file fn))

;; collection -- primitives

(def %pair ((h <any>) (t <any>)) (cons h t))
(def %head ((p <pair>)) (car p))
(def %tail ((p <pair>)) (cdr p))
(def %null? ((p <any>)) (null? p))

(def %make-vector ((size <num>) (fill <any>))
  (let ((v (make-vector (+ 1 size) fill)))
    (vector-set! v 0 'vector)
    v))

(def %vector-ref ((v <vector>) (i <int>)) (vector-ref v (+ 1 i)))
(def %vector-set! ((v <vector>) (i <int>) (o <any>)) (vector-set! v (+ 1 i) o))
(def %vector-length ((v <vector>)) (- (vector-length v) 1))

(def %make-string ((size <num>) (fill <chr>)) (make-string size fill))
(def %string-ref ((s <str>) (i <num>)) (string-ref s i))
(def %string-set! ((s <str>) (i <num>) (ch <chr>)) (string-set! s i ch))
(def %string-length ((s <str>)) (string-length s))

;; READER

(define (read-goo port)
  (let ((first (read-token port)))
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) first)
            ((char=? ch #\|)
             (read-char port) (list first (read-token port)))
            (else first)))))

(define (char-symbol-initial? ch)
  (or (char-alphabetic? ch)
      (memq ch '(#\! #\| #\$ #\/ #\~ #\& #\: #\+ #\= #\* #\% #\< #\> #\? #\^))))

(define (char-symbol-consequent? ch)
  (or (and (not (char=? ch #\|))
	   (char-symbol-initial? ch))
      (char-numeric? ch)
      (char=? ch #\-)))

(define (read-token port)
  (let ((ch (read-noise port)))
    (cond ((eof-object? ch) ch)
          ((char-numeric? ch) (read-number port))
          ((char-symbol-initial? ch) (read-symbol port))
          ((char=? ch #\") (read-string port))
          ((char=? ch #\#) (read-sharp port))
          ((char=? ch #\() (read-list port))
          ((char=? ch #\[) (read-bracket port))
          ((char=? ch #\,) (read-comma port))
          ((char=? ch #\`) (read-quasiquote port))
          ((char=? ch #\{) (read-hash port))
          ((char=? ch #\') (read-quote port))
	  ((char=? ch #\-) (read-minus port))
          (else (error "don't know initial character ~a" ch)))))

(define (read-noise port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) ch)
          ((or (char=? ch #\space) (char=? ch #\newline)
               (char=? ch #\return) (char=? ch #\tab))
           (read-char port)
           (read-noise port))
          ((char=? ch #\;) (read-comment port) (read-noise port))
          (else ch))))

(define (read-minus port)
  (read-char port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? port) '-)
	  ((or (char=? ch #\space) (char=? ch #\newline)
	       (char=? ch #\return))
	   '-)
	  (else (- (read-number port))))))

(define (read-comment port)
  (let ((ch (read-char port)))
    (if (char=? ch #\newline)
        ch
        (read-comment port))))

(define (read-number port)
  (string->number
   (list->string
    (let lp ((ch (peek-char port)))
      (cond ((eof-object? ch) '())
            ((char-numeric? ch)
             (read-char port)
             (cons ch (lp (peek-char port))))
            (else '()))))))

(define (read-symbol port)
  (string->symbol
   (list->string
    (cons (read-char port)
	  (let lp ((ch (peek-char port)))
	    (cond ((eof-object? ch) '())
		  ((char-symbol-consequent? ch)
		   (read-char port)
		   (cons ch (lp (peek-char port))))
		  (else '())))))))
    
(define (read-string port)
  (read-char port)
  (list->string
   (let lp ((ch (read-char port)))
     (cond ((eof-object? ch) (error "end of file inside string literal"))
           ((char=? ch #\\) 
            (let ((next (read-string-special port)))
              (cons next (lp (read-char port)))))
           ((char=? ch #\") '())
           (else (cons ch (lp (read-char port))))))))

(define (read-string-special port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) (error "end of file in \\ string syntax"))
          ((char=? ch #\n) #\newline)
          ((char=? ch #\t) (integer->char 9))
          (else (error "unknown \\ string syntax ~a" ch)))))

(define (read-sharp port)
  (read-char port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) (error "end of file inside a # literal"))
          ((char=? ch #\\) (read-character port))
          ((char=? ch #\t) #t)
          ((char=? ch #\f) #f)
          ((char=? ch #\() (read-vector port))
          (else (error "undefined # syntax ~a" ch)))))

(define (read-character port)
  (let ((ch (read-char port)))
    (cond ((eof-object? ch) (error "end of file inside character literal"))
          (else ch))))

(define (read-vector port)
  (read-char port)
  (let lp ((lst '()))
    (read-noise port)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) (error "end of file inside list"))
            ((char=? ch #\)) (read-char port) (list->vector (reverse lst)))
            (else (let ((obj (read-goo port)))
                    (lp (cons obj lst))))))))

(define (read-list port)
  (read-char port)
  (let lp ((lst '()))
    (read-noise port)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) (error "end of file inside list"))
            ((char=? ch #\)) (read-char port) (reverse lst))
            (else (let ((obj (read-goo port)))
                    (lp (cons obj lst))))))))

(define (read-bracket port)
  (read-char port)
  (let lp ((lst '()))
    (read-noise port)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) (error "end of file inside [] form"))
            ((char=? ch #\]) (read-char port) (cons 'elt (reverse lst)))
            (else (let ((obj (read-goo port)))
                    (lp (cons obj lst))))))))

(define (read-comma port)
  (read-char port)
  (let ((ch (peek-char port)))
    (cond ((eof-object? ch) (error "end of file in unquote form"))
          ((char=? ch #\@) (read-unquote-splicing port))
          (else
           (let ((exp (read-goo port)))
             (cons 'unquote exp))))))

(define (read-unquote-splicing port)
  (read-char port)
  (cons 'unquote-splicing (read-goo port)))

(define (read-quasiquote port)
  (read-char port)
  (cons 'quasiquote (read-goo port)))

(define (read-hash port)
  (read-char port)
  (let lp ((lst '()))
    (read-noise port)
    (let ((ch (peek-char port)))
      (cond ((eof-object? ch) (error "end of file inside [] form"))
            ((char=? ch #\}) (read-char port) (reverse lst))
            (else (let* ((key (read-goo port))
                         (value (read-goo port)))
                    (lp (cons (cons key value) lst))))))))

(define (read-quote port)
  (read-char port)
  (list 'quote (read-goo port)))

;;; TOP LEVEL

(define (load-goo-file file-name module)
  (call-with-input-file file-name
    (lambda (ip)
      (let lp ((form (read-goo ip)))
        (if (eof-object? form)
            '()
            (begin
              (ev-goo form (make-env module '()))
              (lp (read-goo ip))))))))

(define (eval-goo-file file-name)
  (load-goo-file file-name *goo-runtime*))

(define (print-goo obj)
  (cond ((number? obj) (display obj))
        ((boolean? obj) (display obj))
        ((char? obj) (write obj))
        ((string? obj) (write obj))
        ((symbol? obj) (write obj))

        ((class? obj) (display "{class ") (display (class-name obj)) (display "}"))
	((subclass? obj) (display "{subclass ") (display (class-name (subclass-class obj))) (display "}"))
	((union? obj) (display "{union}"))
	((singleton? obj) (display "{singleton ") (display (singleton-value obj)) (display "}"))
        ((pair? obj) (write obj))
        ((null? obj) (write obj))

        ((method? obj) (display "{method ") (display (method-name obj)) (display "}"))
        ((generic? obj) (display "{generic ") (display (generic-name obj)) (display "}"))

        ((instance? obj) (display "{instance ") (display (class-name (instance-class obj))) (display "}"))
        (else (write obj))))

(define *goo-user* (make-module 'goo-user '() '() '() '()))

(bind-module! *goo-user*)

(define (repl-goo)
  (display "Welcome !") (newline) (newline)
  (let repl ()
    (display "goo> ")
    (let ((form (read-goo (current-input-port))))
      (if (eof-object? form)
          (display "Thank you!")
	  (begin
	    (with-exception-catcher
	     (lambda (e)
	       (if (error-exception? e)
		   (write (cons (error-exception-message e)
				(error-exception-parameters e)))
		   (write "Internal error !"))
	       (newline))
	     (lambda () 
	       (let ((value (ev-goo form (make-env *goo-user* '()))))
		 (display ";; ") (print-goo value) (newline))))
            (repl))))))

(define (main . args)
  (if (null? args)
      (repl-goo)
      (let lp ((args args))
	(if (null? args)
	    'ok
	    (let ((arg (car args)))
	      (cond ((string=? arg "--path")
		     (if (pair? (cdr args))
			 (begin
			   (set! *module-path* (cons (cadr args) *module-path*))
			   (lp (cddr args)))
			 (error "missing argument to --path option")))
		    ((string=? arg "--repl")
		     (repl-goo))
		    (else
		     (display ";; evaluating ") (display arg) (newline)
		     (eval-goo-file arg)
			  (lp (cdr args)))))))))

