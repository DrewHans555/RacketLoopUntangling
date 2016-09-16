#lang racket/gui

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; the program begins at startProgram ;;;;;;;;;;;;;;;;;;;;;;;
;;;;; example execution:  (startProgram '((a o)(b u)(c u)...)) ;;;;;;;;;;;

(define startProgram
  (λ (tripcode)
    (cond
      [(isLegalTripcode? tripcode) 
        (cond
          [(unknottable? tripcode) display "The knot can be untangled!"]
          [(knot? tripcode) display "The knot cannot be untangled"]
        )]
      [else display "The tripcode you entered is not legal!"]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Helper Functions below ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns the first atom of a list ;;;
(define firstOf car)

;;; returns the second atom of a list ;;;
(define secondOf cadr)

;;; returns everything but the first of a list ;;;
(define restOf cdr)

;;; returns the last element of a list ;;;
(define (lastOf l)
  (cond 
      [(null? (cdr l)) (car l)]
      [else (lastOf (cdr l))]
  )
)

;;; returms all but the last element of a list ;;;
(define (allButLastOf l)
  (cond
      [(null? (cdr l)) '()]
      [else (cons (car l) (allButLastOf (cdr l)))]
  )
)

;;; returns the firstOf the crossing pair (ex: (a o) returns a) ;;;
(define crossingName
  (λ (crossing)
    (cond
      [(null? crossing) '()]
      [(pair? crossing) (firstOf crossing)]
      [else '()]
    )
  )
)

;;; returns the secondOf the crossing pair (ex: (a o) returns o) ;;;
(define crossingType
  (λ (crossing)
    (cond
      [(null? crossing) '()]
      [(pair? crossing) (secondOf crossing)]
      [else '()]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Untangling Functions below ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; returns #t if a knot's tripcode can be untangled ;;;
(define unknottable? 
  (λ (tripcode)
    (cond
      [(null? tripcode) #t]
      [(hasType1Move? tripcode) (unknottable? (removeType1Move tripcode))]
      [(hasSpecialType1Move? tripcode) (unknottable? (removeSpecialType1Move tripcode))]
      [(hasType2Move? tripcode) (unknottable? (removeType2Move tripcode))]
      [(hasSpecialType2Move? tripcode) (unknottable? (removeSpecialType2Move tripcode))]
      [else #f]
    )
  )
)

;;; returns #t if a knot's tripcode cannot be untangled ;;;
(define knot? 
  (λ (tripcode)
    (cond
      [(null? tripcode) #f]
      [(hasType1Move? tripcode) (knot? (removeType1Move tripcode))]
      [(hasSpecialType1Move? tripcode) (knot? (removeSpecialType1Move tripcode))]
      [(hasType2Move? tripcode) (knot? (removeType2Move tripcode))]
      [(hasSpecialType2Move? tripcode) (knot? (removeSpecialType2Move tripcode))]
      [else #t]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Type1 & Type2 Move Existance Functions below ;;;;;;;;;;;;;;;;

;;; returns #t if any type1 Reidemeister moves are found in tripcode ;;;
(define hasType1Move?
  (λ (tripcode)
    (cond
      [(null? tripcode) #f]
      [(equal? (length tripcode) 1) #f]
      [(and (eqv? (crossingName (firstOf tripcode)) (crossingName (secondOf tripcode))) 
            (not (eqv? (crossingType (firstOf tripcode)) (crossingType (secondOf tripcode))))
       ) #t]
      [else (hasType1Move? (restOf tripcode))]
    )
  )
)

;;; returns #t if the special type1 Reidemeister move is found in tripcode ;;;
(define hasSpecialType1Move?
  (λ (tripcode)
    (cond
      [(and (eqv? (crossingName (firstOf tripcode)) (crossingName (lastOf tripcode))) 
            (not (eqv? (crossingType (firstOf tripcode)) (crossingType (lastOf tripcode))))
       ) #t]
      [else #f]
    )
  )
)

;;; returns #t if any type2 Reidemeister moves are found in tripcode ;;;
(define hasType2Move?
  (λ (tripcode)
    (cond
      [(null? tripcode) #f]
      [(equal? (length tripcode) 1) #f]
      [(and (and (not(equal? (crossingName (firstOf tripcode)) (crossingName (secondOf tripcode))))
                 (equal? (crossingType (firstOf tripcode)) (crossingType (secondOf tripcode)))) 
            (hasMatchingPair? (firstOf tripcode) (secondOf tripcode) (restOf (restOf tripcode)))
       ) #t]
      [else (hasType2Move? (restOf tripcode))]
    )
  )
)

;;; returns #t if the special type2 Reidemeister move is found in tripcode ;;;
(define hasSpecialType2Move?
  (λ (tripcode)
    (cond
      [(and (and (not (eqv? (crossingName (lastOf tripcode)) (crossingName (firstOf tripcode))))
                 (eqv? (crossingType (lastOf tripcode)) (crossingType (firstOf tripcode))))
            (hasMatchingPair? (lastOf tripcode) (firstOf tripcode) (allButLastOf (restOf tripcode)))
       ) #t]
      [else #f]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Type1 & Type2 Moves Removal Functions below ;;;;;;;;;;;;;;;;;

;;; removes the first type1 Reidemeister move that is found in tripcode ;;;
(define removeType1Move 
  (λ (tripcode)
    (cond
      [(null? tripcode) '()]
      [(equal? (length tripcode) 1) tripcode]
      [(and (eqv? (crossingName (firstOf tripcode)) (crossingName (secondOf tripcode))) 
            (not (eqv? (crossingType (firstOf tripcode)) (crossingType (secondOf tripcode))))
       ) (remove-member (firstOf tripcode) (remove-member (secondOf tripcode) tripcode))]
      [else (cons (firstOf tripcode) (removeType1Move (restOf tripcode)))]
    )
  )
)

;;; removes the special type1 Reidemeister move that is found in tripcode ;;;
(define removeSpecialType1Move 
  (λ (tripcode)
    (cond
      [(and (eqv? (crossingName (firstOf tripcode)) (crossingName (lastOf tripcode))) 
            (not (eqv? (crossingType (firstOf tripcode)) (crossingType (lastOf tripcode))))
       ) (allButLastOf (restOf tripcode))]
    )
  )
)

;;; removes the first type2 Reidemeister move that is found in tripcode ;;;
(define removeType2Move 
  (λ (tripcode)
    (cond
      [(null? tripcode) '()]
      [(equal? (length tripcode) 1) '()]
      [(and (and (not (eqv? (crossingName (firstOf tripcode)) (crossingName (secondOf tripcode))))
                 (eqv? (crossingType (firstOf tripcode)) (crossingType (secondOf tripcode))))
            (hasMatchingPair? (firstOf tripcode) (secondOf tripcode) (restOf (restOf tripcode)))
       ) (removeMatchingPair (firstOf tripcode) (secondOf tripcode) (restOf (restOf tripcode)))]
      [else (cons (firstOf tripcode) (removeType2Move (restOf tripcode)))]
    )
  )
)

;;; removes the special type2 Reidemeister move that is found in tripcode ;;;
(define removeSpecialType2Move 
  (λ (tripcode)
    (cond
      [(and (and (not (eqv? (crossingName (lastOf tripcode)) (crossingName (firstOf tripcode))))
                 (eqv? (crossingType (lastOf tripcode)) (crossingType (firstOf tripcode))))
            (hasMatchingPair? (lastOf tripcode) (firstOf tripcode) (allButLastOf (restOf tripcode)))
       ) (removeMatchingPair (lastOf tripcode) (firstOf tripcode) (allButLastOf (restOf tripcode)))]
    )
  )
)

;;; remove-member removes the first instance of member from the list l ;;;
(define remove-member
  (λ (member l)
    (cond
      [(null? l) '()]
      [(eqv? member (firstOf l)) (restOf l)]
      [else (cons (firstOf l) (remove-member member (restOf l)))]
    )
  )
)

;;; returns #t if a type2 move exists ;;;
(define hasMatchingPair?
  (λ (member1 member2 tripcode)
    (cond
      [(null? tripcode) #f]
      [(equal? (length tripcode) 1) #f]
      [(or (and (eqv? (crossingName member1) (crossingName (firstOf tripcode)))
                (eqv? (crossingName member2) (crossingName (secondOf tripcode))))
           (and (eqv? (crossingName member2) (crossingName (firstOf tripcode)))
                (eqv? (crossingName member1) (crossingName (secondOf tripcode))))
       ) #t]
      [else (hasMatchingPair? member1 member2 (restOf tripcode))]
    )
  )
)

;;; helps removeType2Move by finding and removing the matching pair ;;;
(define removeMatchingPair
  (λ (member1 member2 tripcode)
    (cond
      [(null? tripcode) '()]
      [(equal? (length tripcode) 1) '()]
      [(or (and (eqv? (crossingName member1) (crossingName (firstOf tripcode)))
                (eqv? (crossingName member2) (crossingName (secondOf tripcode))))
           (and (eqv? (crossingName member2) (crossingName (firstOf tripcode)))
                (eqv? (crossingName member1) (crossingName (secondOf tripcode))))
       ) (restOf (restOf tripcode))]
      [else (cons (firstOf tripcode) (removeMatchingPair member1 member2 (restOf tripcode)))]
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Legality Functions below ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; isLegalTripcode? returns #f if tripcode is not legal ;;;
(define (isLegalTripcode? tc)
  (cond
    [(null? tc) #f]
    [(not(hasLegalTypes? tc)) #f]
    [(not(has2OfEachName? tc tc)) #f]
    [else #t]
  )
)

;;; hasLegalTypes? returns #f if a crossingType is not an o or u or O or U ;;;
(define (hasLegalTypes? tc)
  (cond
    [(null? tc) #t]
    [(and (not(eqv? (crossingType (firstOf tc)) 'o))
          (not(eqv? (crossingType (firstOf tc)) 'u))
          (not(eqv? (crossingType (firstOf tc)) 'O))
          (not(eqv? (crossingType (firstOf tc)) 'U))
     ) #f]
    [else (hasLegalTypes? (restOf tc))]
  )
)

;;; has2OfEachName? visits each crossingName of tcCopy and counts number of occurences in original tc ;;;
(define (has2OfEachName? tc tcCopy)
  (cond
    [(null? tcCopy) #t]
    [(not(= (count-occurrences (crossingName (firstOf tcCopy)) tc) 2)) #f]
    [else (has2OfEachName? tc (restOf tcCopy))]
  )
)

;;; count-occurences counts the number of times item appears as a crossingName in l ;;;
(define (count-occurrences item l)
    (cond 
      [(null? l) 0]
      [(eqv? (crossingName (firstOf l)) item) (+ 1 (count-occurrences item (restOf l)))]
      [else (count-occurrences item (restOf l))]
    )
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROGRAM END ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;