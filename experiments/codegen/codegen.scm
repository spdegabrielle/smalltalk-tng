(load "tinyscheme+cvs20090722/init.scm")

(define (check-arg pred val caller) #t)
(load "srfi-1.scm")

(define (assemble base . instrs)
  (define (prepend instr base)
    (cond
     ((list? instr) (fold prepend base instr))
     ((number? instr) (cons instr base))
     (else (error "Expected instruction either list or number" instr))))
  (fold prepend base instrs))

(define (_RET) #xC3)

(define eax 0)
(define ecx 1)
(define edx 2)
(define ebx 3)
(define esp 4)
(define ebp 5)
(define esi 6)
(define edi 7)

(define (mod-r-m* mode r1 r2)
  (+ (* mode #x40)
     (* r1 #x08)
     r2))

(define (mod-r-m mode r1 r2)
  (let ((b (mod-r-m* mode r1 r2)))
    (if (= r2 esp)
	(list b #x24)
	b)))

(define (imm32 i)
  (list (modulo i 256)
	(modulo (quotient i 256) 256)
	(modulo (quotient i 65536) 256)
	(modulo (quotient i 16777216) 256)))

(define (onebyte? n)
  (and (< n 128) (>= n -128)))

(define (load32 targetreg srcreg offset)
  (if (and (zero? offset)
	   (not (or (= srcreg esp)
		    (= srcreg ebp))))
      (list #x8b (mod-r-m 0 targetreg srcreg))
      (if (onebyte? offset)
	  (list #x8b (mod-r-m 1 targetreg srcreg) offset)
	  (list #x8b (mod-r-m 2 targetreg srcreg) (imm32 offset)))))

(define (sub32 reg i)
  (if (onebyte? i)
      (list #x83 (mod-r-m* 3 5 reg) i)
      (if (= reg eax)
	  (list #x2d (imm32 i))
	  (list #x81 (mod-r-m* 3 5 reg) (imm32 i)))))

(define (add32 reg i)
  (if (onebyte? i)
      (list #x83 (mod-r-m* 3 0 reg) i)
      (if (= reg eax)
	  (list #x05 (imm32 i))
	  (list #x81 (mod-r-m* 3 0 reg) (imm32 i)))))

(define (load32m targetreg addr)
  (if (= targetreg eax)
      (list #xa1 (imm32 addr))
      (list #x8b (mod-r-m 0 targetreg 5) (imm32 addr))))

(define (store32m srcreg addr)
  (if (= srcreg eax)
      (list #xa3 (imm32 addr))
      (list #x89 (mod-r-m 0 srcreg 5) (imm32 addr))))

(define (load32i targetreg i)
  (list (mod-r-m 2 7 targetreg) (imm32 i)))

(define (mov32 targetreg srcreg)
  (list #x89 (mod-r-m* 3 srcreg targetreg)))

(define (push32 reg)
  (mod-r-m 1 2 reg))

(define (pop32 reg)
  (mod-r-m 1 3 reg))

(define (_CAR) (list (load32 eax eax 4)))
(define (_CDR) (list (load32 eax eax 8)))

(define (code->binary codevec)
  (list->string (map integer->char (reverse codevec))))

(define (simple-function . instrs)
  (let ((code (apply assemble '() instrs)))
    (write (reverse code)) (newline)
    (build-native-function (code->binary code))))

;; for 32bit offset, eax <- [eax + ofs] is 8B 80 XX XX XX XX

	   ;; #x8b #x04 #x24			;; movl (%esp), %eax
	   ;; #x8b #x44 #x24 #x04		;; movl 4(%esp), %eax
	   ;; #x8b #x44 #x24 #x08		;; movl 8(%esp), %eax

(define x (simple-function
	   ;;#x8b #x44 #x24 #x08		;; movl 8(%esp), %eax
	   (load32 eax esp 8)
	   (_CAR)
	   (_RET)))

(define real-code (list #x55 #x89 #xe5 #x83 #xec #x08 #x8b #x45 #x0c #xc9 #xc3))

(define y (simple-function
	   (push32 ebp)
	   (mov32 ebp esp)
	   (sub32 esp 8)
	   (load32 eax ebp 12)
	   (_CAR)
	   (mov32 esp ebp)
	   (pop32 ebp)
	   (_RET)))
