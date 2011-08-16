;;53 : Longest increasing Sub-Seq
(fn k-lis [s]
	   (let [res (reverse (apply max-key count
		  (partition-by #(= % "z")
				(reduce 
				 (fn [l n] (let [p (first l)] (if (< p n) (cons n l)
								  (cons n (cons "z" l))))) [(first s)] (rest s)))))] 
	   (if (> (count res) 1) res '()))) 


;;54 : Partition a Seq
;;Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
(fn k-part [n s]
           (if (< (count s) n) '()
               (cons (first (split-at n s)) (k-part n (last (split-at n s))))));


;;56 : Implement distinct
(fn k-dist [n]
       (reverse 
         (reduce 
           (fn [l y] (if (some #(= % y) l) l (cons y l))) '() n  )))


;;58 : Function Composition
;;Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left.
(fn k-comp [f & fs]
       (let [fns (reverse (cons f fs))]
		(partial
		 (fn [fnlist & args]
		     (first (reduce #(list (apply %2 %1)) args fnlist))) fns)))

;;59 : Justraposition Implementation
;;Take a set of functions and return a new function that takes a variable number of arguments and returns a sequence containing the result of applying each function left-to-right to the argument list.
(fn k-juxt [f & fs]
       (let [fns (cons f fs)]
		(partial
		 (fn [fnlist & args]
		     (map #(apply %1 %2) fnlist (repeat args)))  fns )))


;;60 : Reductions Implementation
;;Write a function which behaves like reduce, but returns each intermediate value of the reduction. Your function must accept either two or three arguments, and the return sequence must be lazy.
(fn k-reductions ([f l]
        (lazy-cat (vector (first l)) (map f (k-reductions f l) (rest l))))
	([f a l] (k-reductions f (cons a l))))
