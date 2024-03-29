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


; ------------------------- Include Functions -------------------------
(load "map.scm")


; ------------------------- Helper Functions -------------------------
; This function returns the nth item of a list
;   INPUT: (nth-item 2 '(1 2 3 4 5))
;   OUTPUT: 2
(define (nth-item index list)
    (cond 
        (
            ; Base Case: index is equal to 0, return
            (equal? 1 index)
                (car list)
        )
        (
            ; Recurse down the list by decrementing the index and getting tail of list
            #t 
                (nth-item (- index 1) (cdr list))
        )
    )
)


; This function replaces the nth item of a list with another value
;   INPUT: (replace-nth-item 1 '(1 2 3) 4)
;   OUTPUT: (4 2 3)
(define (replace-nth-item index list val)
    (cond 
        (
            ; Base Case: return the tail of the list pre-pended with the new value
            (equal? 1 index) 
                (cons val (cdr list))
        )
        (
            ; Pre-pend new list with original values that were iterated over
            #t
                (cons (car list) (replace-nth-item (- index 1) (cdr list) val))
        )
    )
)


; This function swaps two elements in a list when passed their indices
;   INPUT: (swap-elements 1 2 '(1 2 3))
;   OUTPUT: (2 1 3)
(define (swap-elements i j lst)
    (let 
        ; Temp variable to hold i
        ((temp (nth-item i lst)))
        ; Assign i=j then assign j=temp
        (replace-nth-item j (replace-nth-item i lst (nth-item j lst)) temp)
    )
)


; This function searches through a 2d list to see if a list's head has a 
; specific element
;   INPUT: (2d-has-element? 'California adjacency-map)
;   OUTPUT: (California Arizona Nevada Oregon)
(define (2d-has-element? val list)
    (cond 
        (
            ; Base Case: If list is null, return empty list
            (null? list) 
                '()
        )
        (
            ; If the head of a list has the target value, return that list
            (equal? (car (car list)) val) 
                (car list)
        )
        (
            ; Continue recursing through the 2d list
            #t 
                (2d-has-element? val (cdr list))
        )
    )
)


; This function searches through a list to see if it contains a specific element
;   INPUT: (has-element? 3 '(1 2 3))
;   OUTPUT: 3
(define (has-element? val list)
    (cond 
        (
            ; Base Case: list is empty, return empty list
            (null? list) 
                '()
        )
        (
            ; If we've found the item, return it
            (equal? (car list) val) 
                val
        )
        (
            ; Recurse down the list
            #t
                (has-element? val (cdr list))
        )
    )
)


; This function checks whether two locations are adjacent to each other
;   INPUT: (is-adjacent? 'California 'Nevada adjacency-map)
;   OUTPUT: #t or #f
(define (is-adjacent? loc1 loc2 amap)
    (let
        ; Result variable checks whether the sublist contains the second location
        ((result (has-element? loc2 (2d-has-element? loc1 amap))))
        (cond 
            (
                (equal? result '()) 
                    #f
            )
            (
                #t 
                    #t
            )
        )
    )
)


; Helper function for all-swaps
;   INPUT: (swap-helper 1 3)
;   OUTPUT: ((1 3) (2 3))
(define (swap-helper curr target)
    (cond 
        (
            ; Base Case: if current + 1 = target, we return ((curr target))
            (= (+ curr 1) target) 
                (list (list curr target))
        )
        (
            ; Continue recursing while incrementing current
            #t 
                (cons (list curr target) (swap-helper (+ curr 1) target))
        )
    )
)


; This function returns a list of all possible swaps that can be performed given
; a starting number and a target number
;   INPUT: (all-swaps 1 3)
;   OUTPUT: ((1 3) (2 3) (1 2))
(define (all-swaps curr target)
    (cond 
        (
            ; Base Case: If target is decremented to value of 1, return the list with current and target
            (= (- target 1) 1) 
                (list (list curr target))
        )
        (
            ; Append list from the swap-helper (which increments curr) with the list from all-swaps decrementing the target
            #t 
                (append (swap-helper curr target) (all-swaps curr (- target 1)))
        )
    )
)


; This function performs all possible swaps on the list of elements
;   INPUT: (swap-all ((Nevada Utah California) (California Nevada Utah) (Utah California Nevada)) ((1 3) (2 3) (1 2)))
;   OUTPUT: ((Utah Nevada California) (California Utah Nevada) (Nevada California Utah))
(define (swap-all elements swaps)
    (cond
        (
            ; Base Case: If we've gone through the entire list of swaps, return empty list
            (null? swaps) 
                '()
        )
        (
            ; Swap the elements, then recurse with the tail of the swaps
            #t 
                (cons (swap-elements (nth-item 1 (car swaps)) (nth-item 2 (car swaps)) elements) (swap-all elements (cdr swaps)))
        )
    )
)


; This function formats the output of the swapped elements
;   INPUT: (format-swapped ((Nevada Utah California) (California Nevada Utah) (Utah California Nevada)) ((1 3) (2 3) (1 2)) ())
;   OUTPUT: (((Nevada Utah California) ((1 3))) ((California Nevada Utah) ((2 3))) ((Utah California Nevada) ((1 2))))
(define (format-swapped elements swaps prev)
    (cond
        (
            ; Base Case: If we've gone through the entire list of swaps, return empty list
            (null? swaps)
                '()
        )
        (
            ; Pair the element with the swap that was made, then recursively continue
            ; Special case when prev list is null
            (null? prev)
                (cons (cons (car elements) (list (cons (car swaps) prev))) (format-swapped (cdr elements) (cdr swaps) prev))
        )
        (
            ; Pair the element with the swap that was made, then recursively continue
            #t
                (cons (cons (car elements) (list (append prev (list (car swaps))))) (format-swapped (cdr elements) (cdr swaps) prev))
        )
    )
)


; This function will return all children of the current state by returning all
; possible swaps that can be made.
;   INPUT: (get-children '((California Utah Nevada) ((1 2))))
;   OUTPUT: (((Nevada Utah California) ((1 3) (1 2))) ((California Nevada Utah) ((2 3) (1 2))) ((Utah California Nevada) ((1 2) (1 2))))
(define (get-children node)
    (let 
        ((swaps (all-swaps 1 (length (nth-item 1 node))))
        (prev-states (nth-item 2 node)))
        ; Swap all the states, then format the output
        (format-swapped (swap-all (nth-item 1 node) swaps) swaps prev-states)
    )
)


; This function will check if the current node we are at represents the goal state
;   INPUT: (is-goal-state? ‘((Alabama Alaska)()))
;   OUTPUT: #t or #f
(define (is-goal-state? node)
    ; state variable has list of swapped locations
    (let 
        ((state (car node))
        ; next-state variable is just the tail of the state variable formatted with the original input
        (next-state (cons (cdr (nth-item 1 node)) (list (nth-item 2 node)))))
        (cond 
            (
                ; If there's only a single item in the list, technically it is a goal state
                (null? (cdr state)) 
                    #t
            )
            (
                ; Check whether the first two items are adjacent
                (is-adjacent? (nth-item 1 state) (nth-item 2 state) adjacency-map)
                    (cond 
                        (
                            ; When only two states left, we have finished checking all states
                            (= (length state) 2) 
                                #t
                        )
                        (
                            ; Else, keep checking whether the next two states are adjacent
                            #t
                                (is-goal-state? next-state)
                        )
                    )
            )
            (
                ; If is-adjacent? did not return true for given two items, return false
                #t
                    #f
            )
        )
    )
)


; This function will format the starting point of the frontier to include 
; internal state representation
;   INPUT: (format-frontier '(California Utah Nevada))
;   OUTPUT: ((California Utah Nevada) ())
(define (format-frontier frontier)
    (list (cons frontier (list '())))
)


; This function takes a frontier, and iterates through all its nodes to check if
; one of the nodes is the goal-state
;   INPUT: (((California Nevada Utah) ((2 3))) ((Utah California Nevada) ((1 2))) ((California Utah Nevada) ()))
;   OUTPUT: #t or #f
(define (frontier-has-goal? frontier)
    (cond 
        (
            ; Check if the state of the head of the list is a goal state
            (is-goal-state? (car frontier)) 
                #t
        )
        (
            ; Base Case: No more items to check
            (null? (cdr frontier)) 
                #f
        )
        (
            ; Recurse down the list to check through all states
            #t
                (frontier-has-goal? (cdr frontier))
        )
    )
)


; This function will do an iterative depth first search on the frontier
;   INPUT: (i-dfs '((California Utah Nevada) ()) 2)
;   OUTPUT: #f or ((Utah Nevada California) ((1 2) (1 3)))
(define (i-dfs frontier max-depth)
    (cond
        (
            ; Base Case: If the frontier is empty, do not search
            (null? (car (car frontier)))
                #f
        )
        (
            ; Base Case: If frontier only has one element, no need to search
            (equal? (length (car (car frontier))) 1)
                #t
        )
        (#t
            (let
                ; The next-state variable describes the machine's state at the next node
                ((next-state 
                    (cond
                        ((null? (cdr frontier)) frontier)
                            (#t (cdr frontier))
                    )
                ))
                (cond
                    (
                        ; Return solution state if we've reached the goal state
                        (is-goal-state? (car frontier))
                            (car frontier)
                    )
                    (
                        ; Base Case: If not a goal state, but rest of list is empty there is no solution
                        (equal? (length (cdr frontier)) 1)
                            #f
                    )
                    (
                        ; If we are at our max depth, do not get children ... continue with DFS
                        (equal? (length (car (cdr (car frontier)))) max-depth)
                            (i-dfs next-state max-depth)
                    )
                    (
                        ; Recurse down the tree by generating children
                        #t 
                            (i-dfs (append (get-children (car frontier)) next-state) max-depth)
                    )
                )
            )
        )
    )
)


; Helper function for the id-dfs which maintains depth as an argument
;   INPUT: (id-dfs-helper '(California Utah Nevada) 0 2)
;   OUTPUT: #f or ((Utah Nevada California) ((1 2) (1 3)))
(define (id-dfs-helper locations curr-depth max-depth)
    (cond 
        (
            ; Base Case: If no locations, return false
            (null? locations)
                #f
        )
        (
            ; Base Case: If one location, return result
            (equal? (length locations) 1)
                (car (format-frontier locations))
        )
        (
            ; If there is a dfs solution with this current depth, return solution
            (list? (i-dfs (format-frontier locations) curr-depth))
                (i-dfs (format-frontier locations) curr-depth)
        )
        (
            ; Base Case: If we have tried all depths up to the max depth, return false
            (equal? curr-depth max-depth)
                #f
        )
        (
            ; If the DFS with the previous depth failed, and not yet at max depth,
            ; recurse to the next depth
            #t
                (id-dfs-helper locations (+ curr-depth 1) max-depth)
        )
    )
)


; ------------------ Iterative Deepening Depth First Search ------------------
; This function implements an iterative-deepening depth first search to reach
; our goal state. 
; 
; If goal state cannot be reached, the function will output #f.
; If the goal state is reached the first return item will be the original list 
; of locations sorted into a valid solution state. The second return item will 
; be a list of pairs indicating which items need to be swapped in order to reach 
; the solution state.
;   INPUT: (id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
;   OUTPUT: ((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 4) (2 5) (1 5)))
;   ASSUMES: The list of locations does not have sublists, and has valid entries
(define (id-dfs locations)
    (id-dfs-helper locations 1 (- (length locations) 1))
)


; ------------------------------ Tests ------------------------------
; (id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
; $1 = ((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 2) (1 4) (4 5)))
; (id-dfs '(California))
; ; $2 = ((California) ())
; (id-dfs '(Arizona Alaska))
; ; $3 = #f

; These tests should work
; (id-dfs '(California))
; (id-dfs '(California Oregon))
; (id-dfs '(California Washington Oregon))
; (id-dfs '(Idaho California Washington Oregon))
; (id-dfs '(Nevada Idaho California Washington Oregon))
; (id-dfs '(Nevada Idaho California Washington Oregon Arizona))
; (id-dfs '(Nevada Idaho California Washington Oregon Arizona Montana))
; (id-dfs '(Wyoming Nevada Idaho California Washington Oregon Arizona Montana))
; (id-dfs '(Utah Wyoming Nevada Idaho California Washington Oregon Arizona Montana))

; These tests should not work
; (id-dfs '())
; (id-dfs '(California New-York))
; (id-dfs '(California Maine Oregon))
; (id-dfs '(California Vermont Oregon Nevada))
; (id-dfs '(California Pennsylvania Oregon Nevada Washington))


; ------------------------------ A* Helpers ------------------------------
; This function will return all children of the current state by returning all
; possible swaps that can be made as well as its heuristic for reaching the
; goal state
;   INPUT: (get-children '((California Nevada) () 1))
;   OUTPUT: (((Nevada California) (1 2) 1))
(define (A*-get-children node)
    (let 
        ((swaps (all-swaps 1 (length (nth-item 1 node))))
        (prev-states (nth-item 2 node)))
        ; Swap all the states, then format the output
        (format-swapped (swap-all (nth-item 1 node) swaps) swaps prev-states)
    )
)


; This function counts how many locations are adjacent
;   INPUT: (A*-count-adj '(California Nevada Washington Oregon))
;   OUTPUT: 2
(define (A*-count-adj locations)
    (cond
        (
            ; Base Case: Once we've recursed through all locations, start adding up
            (null? (cdr locations))
                0
        )
        (
            ; If items are adjacent, add one to the count
            (equal? #t (is-adjacent? (car locations) (cadr locations) adjacency-map))
                (+ (A*-count-adj (cdr locations)) 1)
        )
        (
            ; If items are not adjacent, just propogate the count upwards
            #t
                (A*-count-adj (cdr locations))
        )
    )
)


; This function takes a normal node, and assigns it a heuristic. The heuristic 
; I'll be using is as follows:
; 
;       h(x) = length_of_locations_list - number_of_adjacent_locations
;       g(x) = cost so far
;       f(x) = h(x) + g(x)
; 
;   INPUT: (A*-new-node '((California Utah Nevada) ()))
;   OUTPUT: (((California) () 1))
(define (A*-new-node node)
    (append node (list (A*-count-adj (car node))))
)


; This function takes a node that already has a heuristic value. And uses that
; value to create the next heuristic.
; 
;       h(x) = length_of_locations_list - number_of_adjacent_locations
;       g(x) = cost so far
;       f(x) = h(x) + g(x)
; 
;   INPUT: (A*-next-node '((California Nevada) ()) 5)
;   OUTPUT: (((Nevada California) () 6))
(define (A*-next-node node gx)
    (append node (list (+ gx (A*-count-adj (car node)))))
)


; ------------------------------ A* Search ------------------------------
; Heuristic for optimistically estimating the number of swaps required to 
; complete a solution

; Since the heuristic represents cost, we want to take the smalles heuristic
; score in front of us

; Possible Heuristics
;   - number of locations that are currently adjacent to one another
;   - separate states into regions. higher # states within a region would result
;       in a better heuristic?


; This function performs an A* search using the following heuristic:
; 
;       h(x) = length_of_locations_list - number_of_adjacent_locations
;       g(x) = cost so far
;       f(x) = h(x) + g(x)
; 
;   INPUT: (A* '(Tennessee Iowa Kentucky North-Carolina Missouri))
;   OUTPUT: ((North-Carolina Tennessee Kentucky Missouri Iowa) ((1 4) (2 5) (1 5)))
;   ASSUMES: The list of locations does not have sublists, and has valid entries
(define (A* locations)
    (display "Not Complete Yet")

    ; Check base cases: null list, one item list

    ; Check if the head of the open list, is the goal state
        ; if it is, return this goal state

    ; Get all children and assign heuristics to all children

    ; Sort the states from smallest to largest heuristic value

    ; Recurse on the tail of the list
)
