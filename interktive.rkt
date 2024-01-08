#lang racket

(require racketscript/interop racketscript/browser)

;; macros to make life easier
(define-syntax-rule (hash-set- hs key value)
  (set! hs (hash-set! hs key value)))

;; short for hash-ref with default value '()
(define-syntax-rule (_ ui name)
  (hash-ref ui name '()))

(define-syntax-rule (_? ui name def)
  (hash-ref ui name def))

(define-syntax-rule (_arr-ref arr i)
  (js-string->string ($ ( (_ ((_ arr 'get) i) 'dom)) 'innerText)))



;; failed(sorta works) hashmap attempt
(define (make-hash)
  '())

(define (change-ele hash k v)
  (if (eq? (car (car hash)) k)
      (cons (cons k v) (cdr hash))
      (cons (car hash) (change-ele (cdr hash) k v))))

(define (hash-set! hash key value)
  (let [(entry (assoc key hash))]
    (if entry
        (change-ele hash key value)
        (cons (cons key value) hash))))

(define (hash-ref hash key default)
  (let ((entry (assoc key hash)))
    (if entry
        (cdr entry)
        default)))


;; helper function
(define (remove-at-index lst index)
  (cond
    ((null? lst) lst) ; If the list is empty, return the empty list
    ((= index 0) (cdr lst)) ; If index is 0, remove the first element
    (else
     (cons (car lst) ; Keep the current element
           (remove-at-index (cdr lst) (- index 1)))))) ; Recur with the rest of the list and decremented index



; (make-hash)

;; contains all the ui elements
(define *ui* (make-hash))

;; contains the action function
(define *action-function* (lambda () 1))


;; helper functions to deal with class-lists

(define (add-class ls new-class)
  (if (member new-class ls)
      ls          ; If the element is already present, return the original list
      (append ls (list new-class))))

(define (remove-class ls cls)
  ; ($$ console.log ls)
  ; ($$ console.log cls)
  (filter (lambda (x)  (not (equal? x cls))) ls))

;;-------------------------------------------------
;;
;; Every component is essentially a hash map with different properties.
;; All of them have the property `dom` which holds the DOM object of the
;; component. There needs to be `hide` and `display` methods installed
;; into the object so that we display only the required objects.
;; Other functions are defined depending on the component.
;;
;;-------------------------------------------------


;; array element (togglable)

(define (ele x)
  (define hs (make-hash))
  (define node 
        ($$ document.createElement (symbol->string 'button)))
  ($$ node.addEventListener "click" 
        (lambda (x)
        (if ($$ node.classList.contains "untoggled")
          (begin 
            ; (hash-set- hs 'selected? (list #t))
            ($/:= ($ node 'dataset 'selected) #t)

            ; ($$ console.log "I AM GOING TO BE SELECED" (hash-ref hs 'selected? '()))
            ; ($$ node.setAttribute (symbol->string 'class) (symbol->string 'toggled))
            (hash-set- hs 'classlist 
              (remove-class (hash-ref hs 'classlist '()) (symbol->string 'untoggled)))
            (hash-set- hs 'classlist 
              (add-class (hash-ref hs 'classlist '()) (symbol->string 'toggled)))
            ; ($$ console.log hs)
            ; ($$ console.log hs)
            ($$ node.setAttribute (symbol->string 'class) 
              (string-join (hash-ref hs 'classlist '()) " "))
            )
          (begin 
            ; (hash-set- hs 'selected? (list #f))
            ($/:= ($ node 'dataset 'selected) #f)
            (set! hs (hash-set! hs 'classlist 
              (remove-class (hash-ref hs 'classlist '()) (symbol->string 'toggled))))
            (set! hs (hash-set! hs 'classlist 
              (add-class (hash-ref hs 'classlist '()) (symbol->string 'untoggled))))
            ($$ node.setAttribute (symbol->string 'class) 
              (string-join (hash-ref hs 'classlist '()) " "))
          ))
            ; ($$ console.log "I AM  SELECED" (hash-ref hs 'selected? '()))
          ))
  ($/:= ($ node 'dataset 'selected) #f)
  (hash-set- hs 'classlist (list (symbol->string 'untoggled)))
  (hash-set- hs 'selected? (list #f))

  (hash-set- hs 'hide 
    (lambda () 
      (hash-set- hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))
      ))
  (hash-set- hs 'display 
    (lambda () 
      (hash-set- hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))))
  (hash-set- hs 'add-class 
    (lambda (cls) 
      (hash-set- hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string cls)))
      ))
  (hash-set- hs 'remove-class 
    (lambda (cls) 
      (hash-set- hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string cls)))))
  (hash-set- hs 'get-data
    (lambda (param)
      (js-string->string ($ node 'dataset param))))
  (hash-set- hs 'dom 
    (lambda () 
      ($$ node.replaceChildren "")
      ($$ node.setAttribute (symbol->string 'class) (string-join (hash-ref hs 'classlist '()) " "))
      ; ($$ console.log ($ node 'classList))
      
      (if (or (string? x) (symbol? x) (number? x))
          ($$ node.append x)
          ($$ node.append ((hash-ref x 'dom '()))))
      node))
  hs)

;; array - list of array elements
(define (array ls)
  ; (define node
  ;   ($$ document.createElement (symbol->string 'div)))
  ; (for-each (lambda (child) 
  ;             ($$ node.append child))
  ;           (map ele ls))
  (define hs (make-hash))
  (hash-set- hs 'classlist (list (symbol->string 'array)))
  (hash-set- hs 'hide 
    (lambda () 
      (for-each 
        (lambda (x)
          ((hash-ref x 'hide (lambda () 1))))
        (hash-ref hs 'children '()))
      (hash-set- hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))
      ))
  (hash-set- hs 'display 
    (lambda ()
      (for-each 
        (lambda (x)
          ((hash-ref x 'display (lambda () 1))))
        (hash-ref hs 'children '()))
      (hash-set- hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))))
  ; (set! hs (hash-set! hs 'children 
  ;   (map 
  ;     (lambda (x)
  ;       (if (or (string? x) (number? x) (symbol? x))
  ;         (ele x)
  ;         x))
  ;     ls)))
  (hash-set- hs 'children (make-vector (length ls)))
  (for ([i (range 0 (length ls))]) 
    (vector-set! (hash-ref hs 'children '()) i 
      (let ([x (list-ref ls i)])
        (if (or (string? x) (number? x) (symbol? x))
          (ele x)
            x))))
  (hash-set- hs 'children (vector->list (hash-ref hs 'children '())))

  (hash-set- hs 'get 
    (lambda (i)
      (list-ref (hash-ref hs 'children '()) i)))
  
  (hash-set- hs 'set 
    (lambda (i val)
      (define tmp (list->vector (hash-ref hs 'children '())))
      (vector-set! tmp i 
        (if (or (string? val) (number? val)) 
          (begin
            ; ($$ console.log "I AM HERE" val)
            (ele val))
          val))
      (hash-set- hs 'children (vector->list tmp))))
  (hash-set- hs 'append
    (lambda (val)
      (hash-set- hs 'children
        (append (hash-ref hs 'children '()) 
          (if (or (string? val) (number? val)) 
            (begin
              ; ($$ console.log "I AM HERE" val)
              (list (ele val)))
            (list val))))
      ($$ console.log "OKHERE" (hash-ref hs 'children '()) )))
  (hash-set- hs 'remove
    (lambda (idx)
      (hash-set- hs 'children
        (remove-at-index (hash-ref hs 'children '()) idx))))
  (hash-set- hs 'get-content
    (lambda () 
      (map 
        (lambda (x)
          ($ ((_ x 'dom)) 'innerText))
        (_ hs 'children))))
  ; (hash-set- hs 'add-childre)
  
  (set! hs (hash-set! hs 'dom 
    (lambda ()
      (define arr-node 
            ($$ document.createElement (symbol->string 'div)))
      ($$ arr-node.setAttribute (symbol->string 'class) (string-join (hash-ref hs 'classlist '()) " ")) 
      (map 
        (lambda (x) 
          (if (or (string? x) (number? x))
              ($$ arr-node.append (ele x))
              (begin 
                ; ($$ console.log "usedful" (hash-ref x 'classlist 1337))
                ($$ arr-node.append ((hash-ref x 'dom '()))))))
        (hash-ref hs 'children '()))
        arr-node)))
   (set! hs (hash-set! hs 'get-selected 
    (lambda () 
      (car (foldl 
        (lambda (x v) 
          (if (equal? "true" ((hash-ref x 'get-data (lambda (x) 0)) 'selected)) 
              (cons (cons (cdr v) (car v)) (+ (cdr v) 1))
              (cons (car v) (+ (cdr v) 1))))
        (cons '() 0)
        (hash-ref hs 'children '())))
        )))
  hs)


;; action button
(define (button x)
  
  ; ($$ node.append x)
  (define hs (make-hash))
  (set! hs (hash-set! hs 'classlist (list (symbol->string 'btn))))
  (set! hs (hash-set! hs 'hide 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string 'hinden))))
      )))
  (set! hs (hash-set! hs 'display 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))))))
  (set! hs (hash-set! hs 'dom 
    (lambda () 
      (define node 
        ($$ document.createElement (symbol->string 'button)))
      ($$ node.setAttribute (symbol->string 'class) (string-join (hash-ref hs 'classlist '()) " "))
      (if (or (string? x) (symbol? x) (number? x))
          ($$ node.append x)
          ($$ node.append ((hash-ref x 'dom '()))))
      node)))
  hs)

;; plain text
(define (text x)
  
  ; ($$ node.append x)
  (define hs (make-hash))
  (set! hs (hash-set! hs 'classlist (list (symbol->string 'txt))))
  (hash-set- hs 'content x)
  (hash-set- hs 'set-content 
    (lambda (x)
      (hash-set- hs 'content x)))
  (set! hs (hash-set! hs 'hide 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string 'hinden))))
      )))
  (set! hs (hash-set! hs 'display 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))))))
  (set! hs (hash-set! hs 'dom 
    (lambda () 
      (define node 
        ($$ document.createElement (symbol->string 'div)))
      ($$ node.setAttribute (symbol->string 'class) (string-join (hash-ref hs 'classlist '()) " "))
      (if (or (string? (hash-ref hs 'content '())) (symbol? (hash-ref hs 'content '())) (number? (hash-ref hs 'content '())))
          (begin 
            ; ($$ console.log x)
            ($$ node.append (hash-ref hs 'content '())))
          ($$ node.append ((hash-ref (hash-ref hs 'content '()) 'dom '()))))
      node)))
  hs)

(define (input x)
  
  ; ($$ node.append x)
  (define hs (make-hash))
  (define node 
        ($$ document.createElement (symbol->string 'input)))
  (set! hs (hash-set! hs 'classlist (list (symbol->string 'inp))))
  (hash-set- hs 'content x)
  (hash-set- hs 'set-content 
    (lambda (x)
      (hash-set- hs 'content x)))
  (hash-set- hs 'get-value
    (lambda ()
      (js-string->string  ($ node 'value))))
  (set! hs (hash-set! hs 'hide 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (add-class (hash-ref hs 'classlist '()) (symbol->string 'hinden))))
      )))
  (set! hs (hash-set! hs 'display 
    (lambda () 
      (set! hs (hash-set! hs 'classlist 
        (remove-class (hash-ref hs 'classlist '()) (symbol->string 'hinden)))))))
  (set! hs (hash-set! hs 'dom 
    (lambda () 
      
      ($$ node.setAttribute (symbol->string 'class) (string-join (hash-ref hs 'classlist '()) " "))
      (if (or (string? (hash-ref hs 'content '())) (symbol? (hash-ref hs 'content '())) (number? (hash-ref hs 'content '())))
          (begin 
            ; ($$ console.log x)
            ($$ node.setAttribute "placeholder" (hash-ref hs 'content '())))
          ($$ node.setAttribute "placeholder" ((hash-ref (hash-ref hs 'content '()) 'dom '()))))
      node)))
  hs)

(define (def-ui ls)
  ; (map (lambda (x) (set! *ui* (hash-set! *ui* (car x) (cdr x)))) ls))
  (map (lambda (x) (hash-set- *ui* (car x) (cdr x))) ls))

; (define (def-action ac)
;   (set! *action-function* ac))

(define *actions* (make-hash))
; (define *curr-action* '())
(define content ($$ document.createElement "div"))
($$ content.setAttribute "id" "content")
(define dropdown ($$ document.createElement "select"))
($$ dropdown.setAttribute "id" "dropdown")
($$ dropdown.addEventListener "change" 
  (lambda (e)
    (render content *ui*)))

(define (add-action name func ls)
  (define node ($$ document.createElement "option"))
  ($$ node.setAttribute "value" name)
  ($$ node.append name)
  ($$ dropdown.append node)
  (set! *actions* (hash-set! *actions* name (cons func ls)))
  *actions*)

(define hide-all 
  (lambda () 
    (for-each
      (lambda (x)
        "hewwo"
        ((hash-ref (cdr x) 'hide (lambda () 1))))
      *ui*)
    0))

(define show-ui
  (lambda (ls) 
    (for-each 
      (lambda (name)
        (let ([ele (hash-ref *ui* name '())])
          ((hash-ref ele 'display (lambda () 1)))))
      ls)))

(set! *action-function* 
  (lambda (name) 
    ; ($$ console.log "uwu" *actions*)
    (let ([act (hash-ref *actions* name '())])
      (let ([func (car act)]
            [ls (cdr act)])
           (hide-all)
           (show-ui ls)
           func))))



;;----------------------------------------------------



















(def-ui (list 
  (cons 'nested-arr (array (list (array '(1 2 3 4)) 2 3 4)))
  (cons 'arr (array '(2 1 5 7 3)))
  (cons 'txt1 (text "Not sorted"))
  (cons 'input1 (input "Enter a number"))
  ))

(add-action "swap" 
  (lambda (ui) 
    (let ([arr1 (_ ui 'arr)])
      (define swappers ((_ arr1 'get-selected))) ;; get the selected elements' indices
      (match swappers
        [(list a b) ;; need to select only two elements
          (let 
            ([tmp (_arr-ref arr1 a)]) ;; tmp = arr1[a]
            ((_ arr1 'set) a (_arr-ref arr1 b)) ;; arr1[a] = arr1[b]
            ((_ arr1 'set) b tmp) ;; arr1[b] = tmp

            (define actual-list 
              (map 
                (lambda (x) (string->number (js-string->string x))) 
                ((_ arr1 'get-content)))) ;; get the list of content in the array
            
            ;; add custom css to the elements that are in correct place
            (let [(sorted-list (sort actual-list <))]
              (for ([i (range 0 (length actual-list))])
                (if (equal? (list-ref actual-list i) (list-ref sorted-list i))
                    ((_ ((_ arr1 'get) i) 'add-class) 'correct)
                    ((_ ((_ arr1 'get) i) 'remove-class) 'correct)))
              
            ;; report if sorted or not in the text box
            ((_ (_ ui 'txt1) 'set-content) 
              (if (andmap (lambda (a b) (= a b)) actual-list sorted-list) 
                  "Sorted" "Not Sorted")))
            )]

        [_ ($$ window.alert "Please select only 2 elements")])
      )
     ui) 
  (list 'txt1 'arr))



(add-action "append"
  (lambda (ui) 
    (let ([arr1 (_ ui 'arr)])
        ((_ arr1 'append) 
          ((_ (_ ui 'input1) 'get-value))))
    ui) 
  (list 'input1 'arr))

(add-action "pop"
  (lambda (ui) 
    (let ([arr1 (_ ui 'arr)])
        ((_ arr1 'remove) (string->number ((_ (_ ui 'input1) 'get-value)))))
      ui)
  (list 'input1 'arr))



































;;----------------------------------------------------

*ui* ;; for debugging



(define body ($$ document.querySelector "body"))
;; select a div instead of body in order to embed 

($$ body.append dropdown)
($$ body.append content)


(define (render parent ui)
  ($$ parent.replaceChildren "")
  (define func (*action-function* (js-string->string ($ dropdown 'value))))
  (define dobtn ($$ document.createElement "button"))
  ($$ dobtn.setAttribute "id" "dobtn")
  ($$ dobtn.append (js-string->string ($ dropdown 'value)))
  ($$ dobtn.addEventListener "click"
    (lambda (x) 
      (render parent (func ui))))
  (for-each 
    (lambda (x) 
      (if (or (number? (cdr x)) (string? (cdr x)))
        ($$ parent.append (cdr x))
        ($$ parent.append ((hash-ref (cdr x) 'dom '())))))
    (reverse ui))
  ($$ parent.append dobtn))


;; call to render the elements
(render content *ui*)

; ;; css
; (define style-tag ($$ document.createElement "style"))
; ($$ style-tag.append ".toggled{background-color: #04AA6D;}\n.hinden{display:none;}\n.untoggled{}")
; ($$ body.append style-tag )


;; experimentation
(define (pop-up txt)
  (define sc ($$ document.createElement "script"))
  ($$ sc.append "alert(\"hi\")")
  ($$ body.append sc))