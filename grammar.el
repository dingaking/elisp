;; 아래 문서를 사용하여 elisp의 사용방법을 정리합니다.
;; http://ageofblue.blogspot.kr/2012/01/emacs-lisp-1.html
;;

;; Find Function Manual
;; C-h f -> 함수 입력 후 엔터, ex) C-h f:message

;; 키에 연결된 함수 찾기
;; C-h k -> 키에 연결된 함수 찾기, ex) C-h k:C-x b

;; 환경에 저장된 변수 찾기
;; C-h v -> 환경에 저장된 변수 찾기, ex) C-h v:tetris-score <- 테트리스 점수

;; 바인딩 도움말
;; C-h b -> 키 입력이 어떤 맴에 어떤 함수로 연결되었는지 확인합니다.
;; ex) C-h b : 도움말 페이지 표시
;; 현재 사용자의 버퍼에 어떠한 기능들이 제공되며 함수 바인딩에 대한 정보를 제공합니다.

;; interactive function
;; 사용자에게 직접 노출된 함수/명령
;; M-x:명령 -> 사용자가 직접 호출이 가능하다

;; IELM 모드
;; Inferior Emacs Lisp Mode
;; 스크립트형 언어들이 제공하는 REPL(Read-Eval-Print Loop)와 같은 환경
;; M-x ielm


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

;; Environment
;; 값을 가지고 있는 심벌을 변수라고 부릅니다.
;; 만약 값이 없는 심벌을 계산하면 어떻게 될까요?
no-such-a-var
;; Symbol's value as variable is void: no-such-a-var
;; 주어진 심벌의 변수로서의 값이 없다는 에러가 발생합니다.
;; 변수 및 함수가 저장되는 공간을 환경(environment)라고 부릅니다.
;; 환경은 이름(심벌)->값의 매핑을 답고 있는 단순한 저장공간입니다.


;; setq
;; 심벌에 값을 할당
(setq tetris-score 100)
;; tetris-score 심벌에 100을 할당

;; 특수 연산자(special operator) 또는 특수 형태(special form)
;; Lisp의 일바적인 계산법을 따르지 않는 연산자를 말합니다.
;; 이러한 특수 연산자에는 심벌에 값 할당, 함수 선언, 조건문, 매크로가 있습니다.
;; 심벌을 생성하는 것, 함수를 선언하는 것은 등은 계산을 할 수 없기 때문입니다.

;; 조건문(if)
;; (if COND THEN ELSE...)
;; COND != nil => THEN
;; COND == nil => ELSE

;; Lisp 모듈에서 사용되는 함수/변수의 이름은 모듈의 이름으로 시작합니다.
;; 조건문이 특수 형태가 아니라면 어떤 현상이 발생할까요?
;; 계산을 하기 위해 함수 인자 위치에 있는 THEN과 ELSE의 모든 표현식을 계산합니다.
(message "hello %s" (if (eq 1 2)
                        "hello"
                      "world"))
;; "hello world"


;; 함수의 정의(defun)
;; (defun NAME ARGLIST [DOCSTRING] BODY...)

;; if : 조건문
;; defun : 함수 심벌 -> 함수의 정의
;; setq : 전역 변수 심벌 -> 값
;; let : 지역 변수 심벌 -> 값


;; 맵(map)
;; 키입력과 바인드된 함수의 정보를 담고 있는 변수입니다.
;; global-map
;; (list ("C-a" . 'move-beginning-of-line)
;;      ("C-e" . 'move-end-of-line)
;;      ...)
;; "C-a"와 같은 입력키와 move-beginning-of-line과 같은 함수의 심볼로 이루어집니다.
;; 이맥스는 여러개의 맵을 가지고 있고 이들을 체인으로 엮어서 관리합니다.
;; 사용자가 키를 입력하면 위의 맵부터 순차적으로 함수를 찾습니다.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;            +---------- 사용자의 키 입력                                  ;;
;;            |                                                         ;;
;;   +--------v--------+                                                ;;
;;   | local-map?      | (("C-a" . `soo-magic-beg-of-line) ...)         ;;
;;   +--------|--------+                                                ;;
;;   +--------v--------+                                                ;;
;;   | minor-mode-map* | (("C-." . `flyspell-auto-correct-word) ...)    ;;
;;   +--------|--------+                                                ;;
;;   +--------v--------+                                                ;;
;;   | major-mode-map+ | (("M-e" . `c-end-of-statement) ...)            ;;
;;   +--------|--------+                                                ;;
;;   +--------v--------+                                                ;;
;;   | global-map      | (("C-a" . `move-beginning-of-line) ...)        ;;
;;   +-----------------+                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; local-map, minor-mode-map, major-mode-map은 현재 버퍼 안에서 영향을 미칩니다.
;; global-map은 전역적인 이맥스 키입력에 영향을 미칩니다.

;; 모드(Mode)
;; 이맥스는 현재 작업 파일에 유용한 함수와 키맵의 그룹을 로딩합니다.
;; 예를 들어 C언어로 짜여진 파일이 열린 경우 C언어에 
