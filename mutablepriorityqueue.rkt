#lang racket

;; A Mutable Priority Queue ADT in Racket

(provide create-mpq mpq-add! mpq-remove! mpq-top mpq-empty?)

;; (create-mpq) creates a new mutable priority queue
;; create-mpq: Void -> MPQ

;; (mpq-add! i pri pq) add item i with priority pri to pq
;; mpq-add!: Any Int MPQ -> Void
;; effects: mutates pq

;; (mpq-remove! pq) removes the item with the largest priority in pq
;;   and returns the item removed.  
;;   If multiple items have the same largest priority, any can be removed
;; mpq-remove!: MPQ -> Any
;; requires: pq is non-empty
;; effects: mutates pq

;; (mpq-top pq) returns the item with the largest priority in pq.
;;   If multiple items have the same largest priority, any can be returned
;; mpq-top: MPQ -> Any
;; requires: pq is non-empty

;; (mpq-empty? pq) determines if pq is empty
;; mpq-empty?: MPQ -> Bool

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; do not modify the file above this line

;; INTEGRITY STATEMENT (modify if necessary)
;; I received help from the following sources:
;; None. I am the sole author of this work 

;; sign this statement by removing the line below and entering your name
;; Name: Yiming Zhong
;; login ID: y45zhong

;; IMPLEMENTATION BELOW

(struct mpq (lst) #:mutable)

;; (create-mpq) creates a new mutable priority queue
;; create-mpq: Void -> MPQ

(define (create-mpq) 
  (mpq empty))


;; (mpq-add! i pri pq) add item i with priority pri to pq
;; mpq-add!: Any Int MPQ -> Void
;; effects: mutates pq

;; add a helper function that deals iwth lists 

(define (mpq-add! i pri pq)
  ;; (add-to-lst i pri lst) add item i with pri priority to just the list
  ;; add-to-lst: Any Int (listof (list Any Int)) -> (lsitof (list Any Int))
  (define (add-to-lst i pri lst)
    (cond 
     [(empty? lst) (list (list i pri))]
     [(>= pri (second (first lst)))
      (cons (list i pri) lst)]
     [else 
      (cons (first lst) (add-to-lst i pri (rest lst)))]))
  (set-mpq-lst! pq (add-to-lst i pri (mpq-lst pq))))
          

;; (mpq-remove! pq) removes the item with the largest priority in pq
;;   and returns the item removed.  
;;   If multiple items have the same largest priority, any can be removed
;; mpq-remove!: MPQ -> Any
;; requires: pq is non-empty
;; effects: mutates pq

(define (mpq-remove! pq)
  (define removed (mpq-top pq))
  (set-mpq-lst! pq (rest (mpq-lst pq)))
  removed)


;; (mpq-top pq) returns the item with the largest priority in pq.
;;   If multiple items have the same largest priority, any can be returned
;; mpq-top: MPQ -> Any
;; requires: pq is non-empty

(define (mpq-top pq)
  (first (first (mpq-lst pq))))


;; (mpq-empty? pq) determines if pq is empty
;; mpq-empty?: MPQ -> Bool

(define (mpq-empty? pq) (empty? (mpq-lst pq))) 
