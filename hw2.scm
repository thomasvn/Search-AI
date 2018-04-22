; For this assignment, you will be implementing an algorithm that will sort a 
; list of locations on a map into order using only swap operations, so that 
; they form a continuous path through adjacent locations.

; Initial State: ordered list of locations
; Goal State: list of locations where any two locations adjacent to each other 
;               in the list are adjacent to each other in the map

; Transition Model: Each state may be transmuted to another state by switching 
;                       two locations.

; Cost Model: The goal of your search is to find a solution that requires the 
;               fewest number of swaps.


; ------------------------- Helper Functions -------------------------
; This function returns the nth item of a list
(define (nth-item index list)
    ; base case, index is equal to 0, return
    (cond ((equal? 1 index) (car list))
        ; decrement the index and get tail of list
        (#t (nth-item (- index 1) (cdr list)))))


; This function replaces the nth item of a list with another value
(define (replace-nth-item index list val)
    ; base case, return the tail of the list pre-pended with the new value
    (cond ((equal? 1 index) (cons val (cdr list)))
        ; pre-pend new list with original values that were iterated over
        (#t (cons (car list) (replace-nth-item (- index 1) (cdr list) val)))))


; This function swaps two elements in a list when passed their indices
(define (swap-elements i j lst)
    ; Temp variable to hold i
    (let ((temp (nth-item i lst)))
        ; Assign i=j then assign j=temp
        (replace-nth-item j (replace-nth-item i lst (nth-item j lst)) temp)
    )
)


; This function searches through a 2d list to see if a list's head has a 
; specific element
(define (2d-has-element? val list)
    ; Iterate through list and return list if the head is equal to val
    (cond ((equal? (car (car list)) val) (car list))
        (#t (2d-has-element? val (cdr list)))
    )
)


; This function searches through a list to see if it contains a specific element
(define (has-element? val list)
    ; Iterate through list and return element
    (cond ((equal? (car list) val) val)
        ; If element is not found, return an empty list
        ((null? list) '())
        (#t (has-element? val (cdr list)))
    )
)


; This function checks whether two locations are adjacent to each other
(define (is-adjacent? loc1 loc2 amap)
    (let ((result (has-element? loc2 (2d-has-element? loc1 amap))))
        (cond ((equal? result '()) #f)
            (#t #t)
        )
    )
)


; Helper function for all-swaps
(define (swap-helper curr target)
    ; Base Case: if current + 1 = target, we return ((curr target))
    (cond ((= (+ curr 1) target) (list (list curr target)))
        (#t (cons (list curr target) (swap-helper (+ curr 1) target)))
    )
)


; This function returns a list of all possible swaps that can be performed given
; a starting number and a target number
(define (all-swaps curr target)
    ; Base Case: Call all swaps, until target is decremented to value of 1
    (cond ((= (- target 1) 1) (list (list curr target)))
        ; Append list from the swap-helper with another swap-helper list with a decremented target
        (#t (append (swap-helper curr target) (all-swaps curr (- target 1))))
    )
)


; This function performs all possible swaps on the list of elements
(define (swap-all elements swaps)
    ; Base Case: If we've gone through the entire list of swaps, return empty list
    (cond ((null? swaps) '())
        (#t (cons (swap-elements (nth-item 1 (car swaps)) (nth-item 2 (car swaps)) elements) (swap-all elements (cdr swaps))))
    )
)


; This function formats the output of the swapped elements
; (define (format-swapped elements swaps)

; )


; This function will return all children of the current state by returning all
; possible swaps that can be made.
(define (get-children frontier)  ; TODO: Is this actually called the frontier?
    (swap-all (nth-item 1 frontier) (all-swaps 1 (length (nth-item 1 frontier))))

    ; Iterate through all-swaps and actually swap those elements
    ; Append new swaps to a list
)


(get-children '((Alabama Arizona Alaska) ()))


; (load "map.scm")
; (is-adjacent? 'Florida 'Georgia adjacency-map)


; (define (dfs frontier)
;    (cond
;        ((null? frontier)  ; Base Case: if you have a null list, you failed
;            #f )
;        ((goal-test (car frontier))  ; Check the head of the frontier if it's the goal state
;            (car frontier))
;        (#t  ; Append children of frontier with the cdr of the frontier back into dfs
;            (dfs (append (get-children car(frontier)) (cdr frontier))))
;     )
; )


; ------------------------------ Main Function ------------------------------
; This function implements an iterative-deepening depth first search to reach
; our goal state. 
; 
; If goal state cannot be reached, the function will output #f.
; If the goal state is reached the first return item will be the original list 
; of locations sorted into a valid solution state. The second return item will 
; be a list of pairs indicating which items need to be swapped in order to reach 
; the solution state.
; (define (id-dfs locations)
;     body)


; ------------------------------ Tests ------------------------------
; (id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
; ; $1 = ((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 2) (1 4) (4 5)))
; (id-dfs '(California))
; ; $2 = ((California) ())
; (id-dfs '(Arizona Alaska))
; ; $3 = #f
