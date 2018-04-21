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


; TODO: Move adjacency map to a separate function
; ---------------------------- Adjacency Map ----------------------------
; adjacency map: a list of lists; each of these sublists includes a location as 
;                 its first item and all locations that are adjacent to that 
;                 location as its remaining items.
(define adjacency-map '(
    (Alabama Mississippi Tennessee Georgia Florida)
    (Alaska)
    (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
    (Arizona California Nevada Utah New-Mexico)
    (California Arizona Nevada Oregon)
    (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
    (Connecticut New-York Massachusetts Rhode-Island)
    (Delaware Maryland Pennsylvania New-Jersey)
    (Florida Alabama Georgia)
    (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
    (Hawaii)
    (Idaho Oregon Washington Montana Wyoming Utah Nevada)
    (Indiana Illinois Michigan Ohio Kentucky)
    (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
    (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
    (Kansas Colorado Nebraska Missouri Oklahoma)
    (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
    (Louisiana Texas Arkansas Mississippi)
    (Maine New-Hampshire)
    (Maryland Virginia West-Virginia Pennsylvania Delaware)
    (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
    (Michigan Wisconsin Indiana Ohio)
    (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
    (Mississippi Louisiana Arkansas Tennessee Alabama)
    (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
    (Montana Idaho Wyoming South-Dakota North-Dakota)
    (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
    (Nevada California Arizona Utah Idaho Oregon)
    (New-Hampshire Maine Vermont Massachusetts)
    (New-Jersey Delaware Pennsylvania New-York)
    (New-Mexico Texas Oklahoma Colorado Arizona)
    (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
    (North-Carolina South-Carolina Georgia Tennessee Virginia)
    (North-Dakota Montana South-Dakota Minnesota)
    (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
    (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
    (Oregon Washington Idaho Nevada California)
    (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
    (Rhode-Island Connecticut Massachusetts)
    (South-Carolina Georgia North-Carolina)
    (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
    (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
    (Texas New-Mexico Oklahoma Arkansas Louisiana)
    (Utah Nevada Idaho Wyoming Colorado Arizona)
    (Vermont New-York Massachusetts New-Hampshire)
    (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
    (Washington Oregon Idaho)
    (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
    (Wisconsin Minnesota Iowa Illinois Michigan)
    (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))


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


; This function returns a list of all possible swaps that can be performed. The
; parameter passed represents the number of elements there are
; ASSUMPTION: At least two items in this list
(define (all-swaps curr target)
    ; Base Case: if current + 1 = target, we return ((curr target))
    (cond ((= (+ curr 1) target) (list (list curr target)))
        (#t (cons (list curr target) (all-swaps (+ curr 1) target)))
    )
)

(define (all-swaps-2 curr target)
    ; Call all-swaps from curr target, then curr target - 1, then curr target -2
    (cond ((= (- target 1) 1) (list (list curr target)))
        (#t (append (all-swaps curr target) (all-swaps-2 curr (- target 1))))
    )
)

; (all-swaps 1 5)
(all-swaps-2 1 4)


; TODO: Adjacency Map Function Call
; (is-adjacent? 'Florida 'Georgia '(
;     (Alabama Mississippi Tennessee Georgia Florida)
;     (Alaska)
;     (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
;     (Arizona California Nevada Utah New-Mexico)
;     (California Arizona Nevada Oregon)
;     (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
;     (Connecticut New-York Massachusetts Rhode-Island)
;     (Delaware Maryland Pennsylvania New-Jersey)
;     (Florida Alabama Georgia)
;     (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
;     (Hawaii)
;     (Idaho Oregon Washington Montana Wyoming Utah Nevada)
;     (Indiana Illinois Michigan Ohio Kentucky)
;     (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
;     (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
;     (Kansas Colorado Nebraska Missouri Oklahoma)
;     (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
;     (Louisiana Texas Arkansas Mississippi)
;     (Maine New-Hampshire)
;     (Maryland Virginia West-Virginia Pennsylvania Delaware)
;     (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
;     (Michigan Wisconsin Indiana Ohio)
;     (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
;     (Mississippi Louisiana Arkansas Tennessee Alabama)
;     (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
;     (Montana Idaho Wyoming South-Dakota North-Dakota)
;     (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
;     (Nevada California Arizona Utah Idaho Oregon)
;     (New-Hampshire Maine Vermont Massachusetts)
;     (New-Jersey Delaware Pennsylvania New-York)
;     (New-Mexico Texas Oklahoma Colorado Arizona)
;     (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
;     (North-Carolina South-Carolina Georgia Tennessee Virginia)
;     (North-Dakota Montana South-Dakota Minnesota)
;     (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
;     (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
;     (Oregon Washington Idaho Nevada California)
;     (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
;     (Rhode-Island Connecticut Massachusetts)
;     (South-Carolina Georgia North-Carolina)
;     (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
;     (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
;     (Texas New-Mexico Oklahoma Arkansas Louisiana)
;     (Utah Nevada Idaho Wyoming Colorado Arizona)
;     (Vermont New-York Massachusetts New-Hampshire)
;     (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
;     (Washington Oregon Idaho)
;     (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
;     (Wisconsin Minnesota Iowa Illinois Michigan)
;     (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
; ))


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


; State Representation
; ((Alabama Mississippi Florida) ((1 2) (2 3)))


; (id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
; ; $1 = ((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 2) (1 4) (4 5)))
; (id-dfs '(California))
; ; $2 = ((California) ())
; (id-dfs '(Arizona Alaska))
; ; $3 = #f


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
