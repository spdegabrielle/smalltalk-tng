(load "tinyscheme+cvs20090722/init.scm")

(define (check-arg pred val caller) #t)
(macro (:optional form)
  `(if (null? ,(cadr form)) ,(caddr form) (car ,(cadr form))))
(load "srfi-1.scm")

(define (symbol-append . syms)
  (if (null? syms)
      (error "No symbols supplied to symbol-append")
      (string->symbol
       (fold-right (lambda (sym acc)
		     (if (zero? (string-length acc))
			 (symbol->string sym)
			 (string-append (symbol->string sym) acc)))
		   ""
		   syms))))

(macro (cheap-struct form)
  (let ((record-name (cadr form))
	(field-names (caddr form)))
    (let ((maker-name (symbol-append 'make- record-name))
	  (predicate-name (symbol-append record-name '?)))
      `(begin
	 (define (,maker-name ,@field-names)
	   (vector ',record-name ,@field-names))
	 (define (,predicate-name x)
	   (and (vector? x)
		(eq? (vector-ref x 0) ',record-name)))
	 ,@(let loop ((field-names field-names)
		      (counter 1))
	     (if (null? field-names)
		 '()
		 (cons `(define (,(symbol-append record-name '- (car field-names)) x)
			  (vector-ref x ,counter))
		       (loop (cdr field-names) (+ counter 1)))))))))

(define (filter-map-alist predicate transformer l)
  (filter-map (lambda (entry) (and (predicate (car entry))
				   (if transformer
				       (transformer (car entry) (cdr entry))
				       entry))) l))

(cheap-struct relocation (target))
(cheap-struct label-reference (name is-8bit))
(cheap-struct label-anchor (name))

(define (flatten-and-pre-relocate instrs k)
  (define (walk instrs acc pos marks k)
    (if (null? instrs)
	(k acc pos marks)
	(let ((instr (car instrs))
	      (rest (cdr instrs)))
	  (cond
	   ((or (relocation? instr)
		(label-anchor? instr)
		(label-reference? instr))
	    (walk rest acc pos (cons (cons instr pos) marks) k))
	   ((list? instr) (walk instr acc pos marks
				(lambda (acc pos marks)
				  (walk rest acc pos marks k))))
	   ((number? instr) (walk rest (cons instr acc) (+ pos 1) marks k))
	   ((string? instr) (walk rest
				  (append (reverse (map char->integer (string->list instr))) acc)
				  (+ pos (string-length instr))
				  marks
				  k))
	   (else (error "Invalid instruction in stream" instr))))))
  (walk instrs '() 0 '()
	(lambda (reversed-code final-length reversed-marks)
	  (let* ((code (list->vector (reverse reversed-code)))
		 (marks (reverse reversed-marks))
		 (relocations (filter-map-alist relocation?
						(lambda (r p) (cons (relocation-target r) p))
						marks))
		 (anchors (filter-map-alist label-anchor?
					    (lambda (a p) (cons (label-anchor-name a) p))
					    marks))
		 (refs (filter-map-alist label-reference? #f marks)))
	    (define (anchor-pos name)
	      (cond
	       ((assq name anchors) => cdr)
	       (else (error "Undefined label-anchor" name))))
	    (write `(code ,code))(newline)
	    (write `(marks ,marks))(newline)
	    (for-each (lambda (entry)
			(let ((name (label-reference-name (car entry)))
			      (is-8bit (label-reference-is-8bit (car entry)))
			      (pos (cdr entry)))
			  (if is-8bit
			      (let ((delta (- (anchor-pos name) (+ pos 1))))
				(if (onebyte-immediate? delta)
				    (vector-set! code pos delta)
				    (error "Short jump out of range" entry)))
			      (let ((v (list->vector (imm32* (- (anchor-pos name) (+ pos 4))))))
				(vector-set! code (+ pos 0) (vector-ref v 0))
				(vector-set! code (+ pos 1) (vector-ref v 1))
				(vector-set! code (+ pos 2) (vector-ref v 2))
				(vector-set! code (+ pos 3) (vector-ref v 3))))))
		      refs)
	    (k code
	       relocations)))))

(define (*ret) #xC3)

(define regs '((eax 0)
	       (ecx 1)
	       (edx 2)
	       (ebx 3)
	       (esp 4)
	       (ebp 5)
	       (esi 6)
	       (edi 7)))

(define (reg-num reg)
  (cond
   ((assq reg regs) => cadr)
   (else (error "Invalid register" reg))))

(define %eax 'eax)
(define %ecx 'ecx)
(define %edx 'edx)
(define %ebx 'ebx)
(define %esp 'esp)
(define %ebp 'ebp)
(define %esi 'esi)
(define %edi 'edi)

(define condition-codes '#((o)
			   (no)
			   (b nae)
			   (nb ae)
			   (e z)
			   (ne nz)
			   (be na)
			   (nbe a)
			   (s)
			   (ns)
			   (p pe)
			   (np po)
			   (l nge)
			   (nl ge)
			   (le ng)
			   (nle g)))

(define (condition-code-num code-sym)
  (let loop ((i 0))
    (cond
     ((>= i 16) (error "Invalid condition-code" code-sym))
     ((member code-sym (vector-ref condition-codes i)) i)
     (else (loop (+ i 1))))))

(define specials '((undefined 0)
		   (true 1)
		   (false 2)
		   (nil 3)))

(define (special-oop special-name)
  (cond
   ((assq special-name specials) =>
    (lambda (entry)
      (let ((special-num (cadr entry)))
	(+ 3 (* special-num 8)))))
   (else (error "Invalid special name" special-name))))

(define (register=? x y)
  (eq? x y))

(define (register? x)
  (symbol? x))

(define (immediate? x)
  (or (number? x)
      (relocation? x)
      (label-reference? x)))

(define (memory? x)
  (and (pair? x)
       (eq? (car x) '@)
       (pair? (cdr x))))

(define (@ base-reg . maybe-offset)
  (cond
   ((and (number? base-reg) (null? maybe-offset))
    (list '@ base-reg))
   ((and (register? base-reg) (pair? maybe-offset) (number? (car maybe-offset)))
    (list '@ base-reg (car maybe-offset)))
   (else
    (error "Invalid/unsupported memory reference" `(@ ,base-reg ,@maybe-offset)))))

(define (memory-base-reg-or-absolute x)
  (cadr x))

(define (absolute-memory? x)
  (and (memory? x)
       (number? (memory-base-reg-or-absolute x))))

(define (bitfield . args)
  (define (loop acc args)
    ;;(write `(bitfield-loop ,acc ,args))(newline)
    (if (null? args)
	acc
	(let* ((width-parameter (car args))
	       (signed? (negative? width-parameter))
	       (width-in-bits (abs width-parameter))
	       (limit (inexact->exact (expt 2 width-in-bits))))
	  (let ((value (cadr args)))
	    (if (if signed?
		    (let ((half-limit (quotient limit 2)))
		      (or (>= value half-limit)
			  (< value (- half-limit))))
		    (or (>= value limit)
			(< value 0)))
		(error "Value exceeds bitfield width" (list width-parameter value))
		(loop (+ (* acc limit) (modulo value limit))
		      (cddr args)))))))
  ;;(write `(bitfield ,@args))(newline)
  (loop 0 args))

;; In 32-bit mode, #x66 is the 16-bit-operand override prefix

(define (mod-r-m* mod reg rm)
  (bitfield 2 mod 3 reg 3 rm))

(define (onebyte-immediate? n)
  (and (number? n) (< n 128) (>= n -128)))

(define (imm8 i)
  (modulo i 256))

(define (imm32* i)
  (list (modulo i 256)
	(modulo (shr i 8) 256)
	(modulo (shr i 16) 256)
	(modulo (shr i 24) 256)))

(define (imm32 i)
  (if (or (relocation? i) (label-reference? i))
      (list i 0 0 0 0)
      (imm32* i)))

(define (imm32-if test-result i)
  (if test-result (imm32 i) (imm8 i)))

;; Mod values:
;;  00 - no displacement, [reg]
;;  01 - 8bit displacement, [reg + n]
;;  10 - 32bit displacement, [reg + n]
;;  11 - direct, reg
(define (mod-r-m reg modrm)
  (let ((reg (cond
	      ((number? reg) reg)
	      ((register? reg) (reg-num reg))
	      (else (error "mod-r-m needs a number or a register for reg" reg)))))
    (cond
     ((register? modrm)
      (mod-r-m* 3 reg (reg-num modrm)))
     ((memory? modrm)
      (let ((base-reg (memory-base-reg-or-absolute modrm))
	    (offset (if (null? (cddr modrm)) 0 (caddr modrm))))
	(if (absolute-memory? modrm)
	    ;; raw absolute address, always 32 bits
	    (list (mod-r-m* 0 reg 5) (imm32 base-reg))
	    (let ((mod (cond
			((zero? offset) 0)
			((onebyte-immediate? offset) 1)
			(else 2)))
		  (offset-bytes (cond
				 ((zero? offset) '())
				 ((onebyte-immediate? offset) (imm8 offset))
				 (else (imm32 offset)))))
	      (if (register=? base-reg %esp)
		  ;; can't directly use base reg, must use scaled indexing
		  (list (mod-r-m* mod reg 4) #x24 offset-bytes)
		  ;; normal
		  (list (mod-r-m* mod reg (reg-num base-reg)) offset-bytes))))))
     (else (error "mod-r-m needs a register or memory for modrm" modrm)))))

(define (arithmetic-opcode opcode)
  (cond
   ((assq opcode '((add 0) (or 1) (adc 2) (sbb 3) (and 4) (sub 5) (xor 6) (cmp 7))) => cadr)
   (else (error "arithmetic-opcode: Invalid opcode" opcode))))

(define (*op opcode source target . maybe-8bit)
  (let ((opcode (arithmetic-opcode opcode))
	(w-bit (if (null? maybe-8bit) 1 (if (car maybe-8bit) 0 1))))
    (cond
     ((immediate? source)
      (let ((s-bit (if (and (= w-bit 1) (onebyte-immediate? source)) 1 0)))
	(if (register=? target %eax)
	    (list (bitfield 2 0 3 opcode 2 2 1 w-bit)
		  (imm32-if (= w-bit 1) source))
	    (list (bitfield 2 2 3 0 1 0 1 s-bit 1 w-bit)
		  (mod-r-m opcode target)
		  (imm32-if (and (= w-bit 1) (not (onebyte-immediate? source))) source)))))
     ((memory? source)
      (cond
       ((not (register? target))
	(error "*op: Cannot have memory source and non-register target"
	       (list opcode source target)))
       (else
	(list (bitfield 2 0 3 opcode 2 1 1 w-bit) (mod-r-m target source)))))
     ((register? source)
      (cond
       ((or (memory? target) (register? target))
	(list (bitfield 2 0 3 opcode 2 0 1 w-bit) (mod-r-m source target)))
       (else
	(error "*op: Cannot have register source and non-mem, non-reg target"
	       (list opcode source target)))))
     (else
      (error "*op: Invalid source"
	     (list opcode source target))))))

(define (*mov source target . maybe-8bit)
  (let ((w-bit (if (null? maybe-8bit) 1 (if (car maybe-8bit) 0 1))))
    (cond
     ((immediate? source)
      (if (register? target)
	  ;; special alternate encoding
	  (list (bitfield 4 #b1011 1 w-bit 3 (reg-num target))
		(imm32-if (= w-bit 1) source))
	  (list (bitfield 2 3 3 0 2 3 1 w-bit)
		(mod-r-m 0 target)
		(imm32-if (= w-bit 1) source))))
     ((memory? source)
      (cond
       ((and (absolute-memory? source) (register=? target %eax))
	;; special alternate encoding
	(list (bitfield 7 #b1010000 1 w-bit) (imm32 (memory-base-reg-or-absolute source))))
       ((not (register? target))
	(error "*mov: Cannot have memory source and non-register target" (list source target)))
       (else
	(list (bitfield 2 2 3 1 2 1 1 w-bit) (mod-r-m target source)))))
     ((register? source)
      (cond
       ((and (absolute-memory? target) (register=? source %eax))
	;; special alternate encoding
	(list (bitfield 7 #b1010001 1 w-bit) (imm32 (memory-base-reg-or-absolute target))))
       ((or (memory? target) (register? target))
	(list (bitfield 2 2 3 1 2 0 1 w-bit) (mod-r-m source target)))
       (else
	(error "*mov: Cannot have register source and non-mem, non-reg target"
	       (list source target)))))
     (else
      (error "*mov: Invalid source" (list source target))))))

(define (*call-or-jmp-like immediate-opcode indirect-mod loc)
  (cond
   ((immediate? loc)
    (list immediate-opcode (imm32 loc)))
   ((or (register? loc) (memory? loc))
    (list #xFF (mod-r-m indirect-mod loc)))
   (else
    (error "*call/*jmp: Invalid location" loc))))

(define (*call loc)
  (*call-or-jmp-like #xE8 2 loc))

(define (is-short-jump? loc)
  (and (label-reference? loc)
       (label-reference-is-8bit loc)))

(define (*jmp loc)
  (if (is-short-jump? loc)
      (list #xEB loc 0)
      (*call-or-jmp-like #xE9 4 loc)))

(define (*jmp-cc code loc)
  (write `(*jmp-cc ,code ,loc))(newline)
  (let ((tttn (condition-code-num code)))
    (if (is-short-jump? loc)
	(list (bitfield 4 7 4 tttn) loc 0)
	(list #x0F (bitfield 4 8 4 tttn) (imm32 loc)))))

(define (push32 reg)
  (mod-r-m* 1 2 (reg-num reg)))

(define (pop32 reg)
  (mod-r-m* 1 3 (reg-num reg)))

(define (_CAR) (*mov (@ %eax 4) %eax))
(define (_CDR) (*mov (@ %eax 8) %eax))

(define (*getip reg)
  (list (*call 0)
	(pop32 reg)))

(define (code->binary codevec)
  (list->string (map integer->char (vector->list codevec))))

(define (simple-function . instrs)
  (flatten-and-pre-relocate
   instrs
   (lambda (code relocs)
     (write `((code ,code) (relocs ,relocs))) (newline)
     (let ((bin (code->binary code)))
       (disassemble bin)
       (build-native-function bin relocs)))))

(define (round-up-to-nearest n val)
  (let ((temp (+ val n -1)))
    (- temp (remainder temp n))))

(define (prelude-function locals-frame-size . instrs)
  (let* ((existing-unaccounted-for-padding 8) ;; eip and ebp, just before stack adjustment
	 (total-required-space (+ existing-unaccounted-for-padding locals-frame-size))
	 (total-adjustment (- (round-up-to-nearest 16 total-required-space)
			      existing-unaccounted-for-padding)))
    (simple-function (push32 %ebp)
		     (*mov %esp %ebp)
		     (*op 'sub total-adjustment %esp)
		     instrs
		     (*mov %ebp %esp)
		     (pop32 %ebp)
		     (*ret))))

(define x (simple-function
	   (*mov (@ %esp 8) %eax)
	   (_CAR)
	   (*ret)))

(define real-code (list #x55 #x89 #xe5 #x83 #xec #x08 #x8b #x45 #x0c #xc9 #xc3))

(define y (prelude-function 0
	   (*mov (@ %ebp 12) %eax)
	   (_CAR)))

(define mk_integer-addr (lookup-native-symbol "mk_integer"))
(define get-native-function-addr
  (prelude-function 8
   (*mov (@ %ebp 8) %ecx)
   (*mov (@ %ebp 12) %eax)
   (_CAR)
   (_CAR) ;; function pointer is in car slot
   (*mov %ecx (@ %esp 0))
   (*mov %eax (@ %esp 4))
   (*call (make-relocation mk_integer-addr))))

(define puts-addr (lookup-native-symbol "puts"))
(define puts (prelude-function 4
	      (*mov (@ %ebp 12) %eax)
	      (_CAR)
	      (*mov (@ %eax 4) %eax)
	      (*mov %eax (@ %esp 0))

	      ;(*mov puts-addr %eax)
	      ;(*call %eax)
	      (*call (make-relocation puts-addr))

	      (*mov (@ %ebp 12) %eax)))

(puts "Hello world")

(load "evaluator.scm")

(define (make-parameter v)
  (lambda args
    (if (null? args)
	v
	(begin
	  (set! v (car args))
	  v))))

(macro (parameterize form)
  (let ((bindings0 (cadr form))
	(body (cddr form)))
    (let ((bindings (map (lambda (entry) (cons (gensym "p") entry)) bindings0))
	  (retval (gensym "prv")))
      `(let ,(map (lambda (entry)
		    `(,(car entry) (,(cadr entry))))
		  bindings)
	 ,@(map (lambda (entry)
		  `(,(cadr entry) ,(caddr entry)))
		bindings)
	 (let ((,retval (begin ,@body)))
	   ,@(map (lambda (entry)
		    `(,(cadr entry) ,(car entry)))
		  bindings)
	   ,retval)))))

(define-global! 'jit-compile
  (lambda (exp)
    (let ((env-accumulator #f)

	  (continuation-depth (make-parameter 0))
	  (instruction-rev-acc (make-parameter '()))
	  (frame-depth (make-parameter 0))
	  (frame-stack (make-parameter '())))

      (define (dp term)
	(display (make-string (* 2 (length (frame-stack))) #\ ))
	(write term)
	(newline))

      ;; -- Data types used to represent partial values

      (cheap-struct argument (number))
      (cheap-struct literal (number))
      (cheap-struct recursive-binding (number frame-depth))

      ;; -- Interface to assembler

      (define (emit! . instrs)
	(dp `(emit! ,@instrs))
	(instruction-rev-acc (cons instrs (instruction-rev-acc))))

      (define (*mov-to-eax source)
	(if (and (register? source)
		 (register=? source %eax))
	    '()
	    (*mov source %eax)))

      (define (location-for-value v)
	(cond
	 ((argument? v) (@ %esp (+ (frame-depth) (* (+ (argument-number v) 1) 4))))
	 ((literal? v) (literal-number v))
	 ((recursive-binding? v) (@ %esp (+ (- (frame-depth) (recursive-binding-frame-depth v))
					    (* (+ (recursive-binding-number v) 1) -4))))
	 ((or (register? v) (memory? v) (immediate? v)) v)
	 (else (error "Invalid PE value" v))))

      (define (move-esp-by! slot-count)
	(when (not (zero? slot-count))
	  (let ((nbytes (abs (* slot-count 4))))
	    (emit! (*op (if (positive? slot-count) 'add 'sub) nbytes %esp))
	    (frame-depth ((if (positive? slot-count) - +) (frame-depth) nbytes)))))

      (define (allocate! nwords)
	(let ((ok-label (gensym "allocok")))
	  (emit! (*mov %esi %eax)
		 (*op 'add (* nwords 4) %esi)
		 (*op 'cmp %esi %edi)
		 (*jmp-cc 'ge (make-label-reference ok-label #t))
		 (*mov %eax %esi)
		 (*mov (* nwords 4) %eax))
	  (trap! 0)
	  (emit! (make-label-anchor ok-label))))

      (define (push-frame* count)
	(dp `(push-frame* ,count (frame-stack ,(frame-stack))))
	(when (positive? count)
	  (move-esp-by! (- count)))
	(frame-stack (cons count (frame-stack))))

      (define (pop-frame*)
	(dp `(pop-frame* (frame-stack ,(frame-stack))))
	(let ((count (car (frame-stack))))
	  (when (positive? count)
	    (move-esp-by! (car (frame-stack))))
	  (frame-stack (cdr (frame-stack)))))

      ;; -- Interpreter-core API

      ;;(define (error key val)
      ;;...?)

      (define (undefined)
	(special-oop 'undefined))

      (define (begin-env is-recursive env)
	(dp `(begin-env ,is-recursive))
	(set! env-accumulator (if is-recursive 0 #f))
	env)

      (define (allocate-env name v)
	(dp `(allocate-env ,name ,v))
	(if env-accumulator
	    (let ((a (make-recursive-binding env-accumulator (frame-depth))))
	      (set! env-accumulator (+ env-accumulator 1))
	      a)
	    (or v (undefined))))

      (define (end-env is-recursive env)
	(dp `(end-env ,is-recursive (env-accumulator ,env-accumulator)))
	(when is-recursive
	  (push-frame* env-accumulator)
	  (set! env-accumulator #f))
	env)

      (define (leave-env is-recursive v k)
	(dp `(leave-env ,is-recursive ,v))
	(when is-recursive
	  (pop-frame*))
	(k v))

      (define (update-env name old-annotation v)
	(dp `(update-env ,name ,old-annotation ,v))
	v)

      (define (load-env name annotation v)
	(dp `(load-env ,name ,annotation ,v))
	(let ((loc (location-for-value annotation)))
	  (if (immediate? loc)
	      loc
	      (begin
		(emit! (*mov-to-eax loc))
		%eax))))

      (define (unbound-variable-read name)
	(dp `(load-implicit-global ,name))
	(let ((symloc (load-literal name)))
	  (emit! (*mov (@ %eax 8) %eax)) ;; symbol's value -- FIXME, guessing about future
	  %eax))

      (define (load-literal x)
	(dp `(load-literal ,x))
	(let ((value (cond
		      ((number? x) (+ 1 (* 4 x)))
		      ((symbol? x) #x42424242)
		      ((eq? x #t) (special-oop 'true))
		      ((eq? x #f) (special-oop 'false))
		      (else (error "Unsupported literal type" x)))))
	  ;;(emit! (*mov-to-eax value))
	  ;;%eax
	  value))

      (define (load-closure formals f)
	(dp `(load-closure ,formals))
	(parameterize ((continuation-depth 0)
		       (instruction-rev-acc '())
		       (frame-depth 0)
		       (frame-stack '()))
	  (dp `=============================================)
	  (f (do ((number 0 (+ number 0))
		  (acc '() (cons (make-argument number) acc))
		  (formals formals (cdr formals)))
		 ((null? formals) (reverse acc)))
	     (lambda (v)
	       (dp `---------------------------------------------)
	       (emit! (*mov-to-eax v)
		      (*ret))
	       (get-native-function-addr
		(simple-function (reverse (instruction-rev-acc))))))))

      (define (do-if v tg fg k)
	(let ((false-label (gensym "testfalse"))
	      (done-label (gensym "testdone")))
	  (dp `(do-if ,v))
	  (emit! (*op 'cmp (special-oop 'false) v)
		 (*jmp-cc 'e (make-label-reference false-label #f)))
	  (dp `tg)
	  (tg (lambda (true-v)
		(emit! (*mov-to-eax true-v)
		       (*jmp (make-label-reference done-label #f))
		       (make-label-anchor false-label))
		(dp `fg)
		(fg (lambda (false-v)
		      (emit! (*mov-to-eax false-v)
			     (make-label-anchor done-label))
		      (dp `done-if)
		      (k %eax)))))))

      (define (push-frame count k)
	(dp `(push-frame ,count))
	(push-frame* count)
	k)

      (define (update-frame index v)
	(dp `(update-frame ,index ,v))
	(let ((loc (@ %esp (* index 4))))
	  (emit! (*mov v loc))
	  loc))

      (define (do-primitive names vals expressions k)
	(dp `(%assemble ,names ,vals ,expressions))
	(k 'primitive-result))

      (define (do-call operator operands k)
	(dp `(do-call ,(if (= (continuation-depth) 0)
			      'tailcall
			      'normalcall) ,operator ,operands))
	(let ((loc (location-for-value operator)))
	  (if (immediate? loc)
	      ;; Always absolute
	      (emit! (*call (make-relocation loc)))
	      (emit! (*call loc))))
	(pop-frame*)
	(k %eax)) ;;;;;;'do-call-result))

      (define (push-continuation k)
	(continuation-depth (+ (continuation-depth) 1))
	;;(dp `(push-continuation ,(continuation-depth)))
	(lambda (v)
	  ;;(dp `(pop-continuation ,(continuation-depth) ,v))
	  (continuation-depth (- (continuation-depth) 1))
	  (k v)))

      ((make-eval error undefined begin-env allocate-env end-env leave-env update-env load-env
		  unbound-variable-read load-literal load-closure do-if push-frame update-frame
		  do-primitive do-call push-continuation)
       exp))))

(define (t1)
  (jit-compile '(lambda (num)
		  (= num 0))))

(define (t2)
  (jit-compile '(lambda (num)
		  (define (f n) (if (zero? n) 1 (* n (f (- n 1)))))
		  (f num))))

