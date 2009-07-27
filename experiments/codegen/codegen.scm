(load "tinyscheme+cvs20090722/init.scm")

(define (check-arg pred val caller) #t)
(macro (:optional form)
  `(if (null? ,(cadr form)) ,(caddr form) (car ,(cadr form))))
(load "srfi-1.scm")

(define (relocation? x)
  (and (pair? x)
       (eq? (car x) 'relocation)))

(define (relocation-target x)
  (cadr x))

(define (flatten-and-pre-relocate instrs k)
  (define (walk instrs acc pos relocs k)
    (if (null? instrs)
	(k acc pos relocs)
	(let ((instr (car instrs))
	      (rest (cdr instrs)))
	  (cond
	   ((relocation? instr) (walk rest acc pos (cons (cons instr pos) relocs) k))
	   ((list? instr) (walk instr acc pos relocs
				(lambda (acc pos relocs)
				  (walk rest acc pos relocs k))))
	   ((number? instr) (walk rest (cons instr acc) (+ pos 1) relocs k))
	   ((string? instr) (walk rest
				  (append (reverse (map char->integer (string->list instr))) acc)
				  (+ pos (string-length instr))
				  relocs
				  k))
	   (else (error "Invalid instruction in stream" instr))))))
  (walk instrs '() 0 '()
	(lambda (acc pos relocs)
	  (k (reverse acc) (reverse relocs)))))

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

(define (register=? x y)
  (eq? x y))

(define (register? x)
  (symbol? x))

(define (immediate? x)
  (or (number? x)
      (position-independent-immediate? x)))

(define (position-independent-immediate? x)
  (and (pair? x)
       (eq? (car x) 'position-independent)))

(define (position-independent x)
  (list 'position-independent x))

(define (position-independent-address x)
  (cadr x))

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
  (if (position-independent-immediate? i)
      (let ((address (position-independent-address i)))
	(list `(relocation ,address) 0 0 0 0))
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
		(mod-r-m opcode target)
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

(define (*call loc)
  (cond
   ((immediate? loc)
    (list #xE8 (imm32 loc)))
   ((or (register? loc) (memory? loc))
    (list #xFF (mod-r-m 2 loc)))
   (else
    (error "*call: Invalid location" loc))))

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
  (list->string (map integer->char codevec)))

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
   (*call (position-independent mk_integer-addr))))

(define puts-addr (lookup-native-symbol "puts"))
(define puts (prelude-function 4
	      (*mov (@ %ebp 12) %eax)
	      (_CAR)
	      (*mov (@ %eax 4) %eax)
	      (*mov %eax (@ %esp 0))

	      ;(*mov puts-addr %eax)
	      ;(*call %eax)
	      (*call (position-independent puts-addr))

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
    (let ((continuation-depth (make-parameter 0)))
      (define (error key val) (12345678 'magic-error-procedure key val))
      (define (undefined) (load-literal 17))
      (define (allocate-env name v)
	(write `(allocate-env ,name ,v)) (newline)
	'local)
      (define (update-env name old-annotation v)
	(write `(update-env ,name ,old-annotation)) (newline)
	old-annotation)
      (define (load-env name annotation v)
	(write `(load-env ,name ,annotation)) (newline)
	v)
      (define (unbound-variable-read name)
	(write `(load-implicit-global ,name)) (newline)
	'implicit-global-value)
      (define (load-literal x)
	(write `(load-literal ,x)) (newline)
	x)
      (define (load-closure formals f)
	(write `(load-closure ,formals)) (newline)
	(parameterize ((continuation-depth 0))
	  (write `(IN================)) (newline)
	  (f formals (lambda (v)
		       (write `(return)) (newline)
		       v))
	  (write `(OUT===============)) (newline)
	  'closure-result))
      (define (do-if v tk fk)
	(write `(do-if ,v)) (newline)
	(write `tk) (newline)
	(tk)
	(write `fk) (newline)
	(fk))
      (define (push-frame count k)
	(write `(push-frame ,count)) (newline)
	k)
      (define (update-frame index v)
	(write `(update-frame ,index ,v)) (newline)
	v)
      (define (do-primitive names vals expressions k)
	(write `(%assemble ,names ,vals ,expressions))
	(k 'primitive-result))
      (define (do-call operator operands k)
	(write `(do-call ,(if (= (continuation-depth) 0)
			      'tailcall
			      'normalcall) ,operator ,operands))
	(newline)
	(k 'do-call-result))
      (define (push-continuation k)
	;;(write `(push-continuation)) (newline)
	(continuation-depth (+ (continuation-depth) 1))
	(lambda (v)
	  ;;(write `(pop-continuation ,v)) (newline)
	  (continuation-depth (- (continuation-depth) 1))
	  (k v)))
      ((make-eval error undefined allocate-env update-env load-env unbound-variable-read
		  load-literal load-closure do-if push-frame update-frame
		  do-primitive do-call push-continuation)
       exp))))

(jit-compile '(lambda (num)
		(define (f n) (if (zero? n) 1 (* n (f (- n 1)))))
		(f num)))
