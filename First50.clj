;;34: Implement Range
;;Write a function which creates a list of all integers in a given range.
(fn k-range [a b]
	   (let [d (- b a)]
		(loop [n 1 r '()]
		   (if (> n d) r
		       (recur (inc n) (cons (- b n) r))))));

;;39:Interleave Two Seqs
;;Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.
(fn k-interleave [s1 s2]
  (apply concat 
    (map #(list %1 %2) s1 s2)));

;;40:Interpose a Seq
;;Write a function which separates the items of a sequence by an arbitrary value.
(fn k-interpose [x s2]
  (let [s1 (repeat (count s2) x)]
  (apply vector (drop-last (apply concat
    (map #(list %1 %2) s2 s1))))));


;;43 Reverse Interleave
(fn k-revinterleave [c n]
	  (map reverse
		 (reduce (fn [r s] (map #(cons %2 %1) r s))
			 (repeat n '())
			  (partition  n c))));
