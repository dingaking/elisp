;; 아래 문서를 사용하여 elisp의 사용방법을 정리합니다.
;; http://ageofblue.blogspot.kr/2012/01/emacs-lisp-1.html
;;

;; Find Function Manual
;; C-h f -> 함수 입력 후 엔터, ex) C-h f:message

;; 키에 연결된 함수 찾기
;; C-h k -> 키에 연결된 함수 찾기, ex) C-h k:C-x b

;; interactive function
;; 사용자에게 직접 노출된 함수/명령
;; M-x:명령 -> 사용자가 직접 호출이 가능하다

;; Symbol
;; List 형태의 표현식의 첫 번째 위치에 있을때는 함수로 사용됩니다.
;; 함수 이외의 경우는 값으로 계산됩니다.
fill-column
(message "width %d" fill-column)
;; fill-column 심벌은 값으로 message 심벌은 함수로 계산됩니다.
(fill-column)
;; fill-column 심벌을 함수로 사용하려고 하면 에러가 발생합니다.
message
;; message 심벌을 값으로 사용하려고 하면 에러가 발생합니다.
'message
;; 심벌의 앞에 '(quote, 작은 따옴표)를 붙여 계산을 미룰 수 있습니다.
;; 'message를 계산하면 message 심벌이 리턴되는 표현식을 만들 수 있습니다.
(+ 1 2)
'(+ 1 2)
;; quote는 인자를 평가하지 않고 인자 자체를 리턴합니다.
;; 따라서 (+ 1 2)는 계산값인 3을 리턴하고
;; '(+ 1 2)는 계산식인 (+ 1 2)를 리턴합니다.

;; List
;; Lisp는 List로 구성된 계산식을 해석하는 언어입니다.
;; 따라서 언어의 근간인 List를 이해하는 것은 매우 중요합니다.
;; cons(constructor) : list를 만드는 함수
;; param1 : car(address)
;; param2 : cdr(decrement)
;; return : cons
(cons 1 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;           +---> 2 (cdr)   ;;
;;  cons     |               ;;
;;     +---+-|-+             ;;
;;     | o | o |             ;;
;;     +-|-+---+             ;;
;;       |                   ;;
;;       +---> 1 (car)       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 하나의 콘스 구조
;; 각각의 박스는 값을 가리킵니다.
;; 값 1과 2는 car와 cdr로 접근할 수 있습니다.
;; car : cons를 인자로 받아 첫번째 값 1을 반환합니다.
;; cdr : cons를 인자로 받아 두번째 값 2를 반환합니다.

(car (cons 1 2))
(cdr (cons 1 2))

;; 좀더 복잡한 cons 구조입니다.
(cons 1 (cons 2 (cons 3 4)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  cons          cons          cons                 ;;
;;     +---+---+     +---+---+     +---+---+         ;;
;;     | o | o------>| o | o------>| o | o------> 4  ;;
;;     +-|-+---+     +-|-+---+     +-|-+---+         ;;
;;       |             |             |               ;;
;;       +---> 1       +---> 2       +---> 3         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 위 cons 구조는 lisp의 모든 데이터 구조의 기본입니다.
;; cons의 오른쪽 박스가 다른 cons 구조를 가리킵니다.
;; 이러한 방식으로 여러 개의 cons를 묶을 수 있습니다.
;; list 또한 cons의 구조를 활용하여 표현할 수 있습니다.
;; list의 끝을 나타내기 위해 nil을 마지막 원소로 넣습니다.

(cons 1 nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  cons                   ;;             
;;     +---+---+           ;;
;;     | o | o------> nil  ;;
;;     +-|-+---+           ;;
;;       |                 ;;
;;       +---> 1           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cons 1 (cons 2 (cons 3 nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  cons          cons          cons                   ;;
;;     +---+---+     +---+---+     +---+---+           ;;
;;     | o | o------>| o | o------>| o | o------> nil  ;;
;;     +-|-+---+     +-|-+---+     +-|-+---+           ;;
;;       |             |             |                 ;;
;;       +---> 1       +---> 2       +---> 3           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

()
;;비어 있는 리스트

(cons 1 ())
(cons 1 nil)
(cons 1 (cons 2 nil))
(cons 1 (cons 2 (cons 3 nil)))
(list 1 2 3)


;; Meta Programming

(message "width %d" fill-column)
;; width xx

(eval (message "width %d" fill-column))
;; width xx

'(message "width %d" fill-column)
;;(message "width %d" fill-column)

(eval '(message "width %d" fill-column))
;; width xx

(eval (list 'message "width %d" 'fill-column))
;; width xx

(cdr (list 'message "width %d" 'fill-column))
;; ("width %d" fill-column)

(cdr (list 'message "width %d" fill-column))
;; ("width %d" xx)

(car (list 'message "width %d" 'fill-column))
;; message

;; message 함수를 이용한 출력이 아닙니다.
;; message 함수를 이용한 출력을 위한 코드를 만든것입니다.
