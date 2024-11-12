(defun .LENGTH (L)
  (if (null L) 0  ; if the list is empty return 0
      (+ 1 (.LENGTH (cdr L))))) ; add one for each time you call the function

(defun .REMOVE-ALL (X L) 
  (cond 
    ((null L) nil)  ; if the list is empty return an empty list back                           
    ((equalp (car L) X) (.REMOVE-ALL X (cdr L)))  ; if the head of the list is equal to X, skip it and recall remove all
    (t (cons (car L) (.REMOVE-ALL X (cdr L)))))   ; else, keep the head and recall remove all
)

(defun .MAP (F L)
  (if (null L) nil  ; if the list is empty return an empty list back
      (cons (funcall F (car L)) (.MAP f (cdr L))))) ;add the result of the function to the fron of the recursive result and call map on the rest of the list

(defun .MERGE (L1 L2)
  (cond
    ((null L2) L1)       ; if L2 is empty return L1                       
    ((null L1) L2)       ; if L1 is empty return L2                      
    ((<= (car L2) (car L1))  ; if the front of L2 is less than or equal to L1
     (cons (car L2) (.MERGE (cdr L2) L1)))   ; add the front of L2 to the recursive result and call the function with L1 and the rest of L2 after the first item
    (t (cons (car L1) (.MERGE L2 (cdr L1))))))  ; else L1 is greater so add the head of L1 to the recursive result and call the function with L2 and the rest of L1 after the first item

(defun .ELEMENT-OF (X S)
  (cond
    ((null S) nil)   ; if the set is empty, return an empty set                          
    ((equalp (car S) X) t)    ; if the first element of the set equals X, return true                  
    (t (.ELEMENT-OF X (cdr S))))) ; else call the function on the rest of the set after the first item

(defun .INSERT (S X) 
    (if (.ELEMENT-OF X S) S  ; call element-of to see if the element is already in the set, if so just retun the set
        (cons X S))) ; if not, add x to the front of the set and then return 

(defun .DIFFERENCE (S1 S2)
    (cond
        ((null S1) nil)      ; if S1 is empty return an empty set                             
        ((.ELEMENT-OF (car S1) S2)   ; check if the front element of S1 is in S2                  
         (.DIFFERENCE (cdr S1) S2))   ; if it is, skip it and call difference on the rest of the list            
        (t (cons (car S1) (.DIFFERENCE (cdr S1) S2))))) ; if not, add the front of S1 to the result and call difference on the rest of the list

(defun .SUPERSETEQ (S1 S2)
  (cond
    ((null S2) t)    ; if S2 is empty, return true                             
    ((.ELEMENT-OF (car S2) S1)   ; check if the head of S2 is in S1              
     (.SUPERSETEQ S1 (cdr S2)))  ; if it is, call superseteq again giving S1 and the part of the list of S2 that follows the first item          
    (t nil))) ; if at any point the function gets to this, it means it did not get through the whole list, meaning there is an element in S2 that is not in S1 and it returns false

(defun .FACTORIAL (N)
    (if (<= N 1) ; if n is less than or equal to 1 return 1
        1
        (* N (.FACTORIAL (- n 1))))) ; else n is greater than 1 and multiply N by the factorial of n - 1

(defun .RIGHT-TRI (A B C)
  (and ; check first that all numbers are positive
       (> A 0)                                    
       (> B 0)                                    
       (> C 0)                                    
       (= (+ (* A A) (* B B)) (* C C)))) ; if they are, check if the pythagorean theorem holds 

(defun .NTH-FIBO (N)
 (cond
    ((= N 0) 0) ; if n is 0 return 0
    ((= N 1) 1) ; if n is 1 return 1
    (t (+ (.NTH-FIBO (- N 1)) (.NTH-FIBO (- N 2)))))) ; if not recursively call f(n) = f(n-1) + f(n-2)

(defun .POW (X Y)
  (cond 
    ((= Y 0) 1)
    ((> Y 0) (* X (.POW X (- Y 1))))
    (t (/ 1 (.POW X (- Y))))))

(defun .DIGIT-COUNT (N)
  (if (< N 10)
      1
      (+ 1 (.DIGIT-COUNT (/ N 10))))) ; returns the number of digits in n 
