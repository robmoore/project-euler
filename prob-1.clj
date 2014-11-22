; If we list all the natural numbers below 10 that are multiples
; of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
;
; Find the sum of all the multiples of 3 or 5 below 1000.
(defn euler-prob-1
  [max-val]
  (let [nat-num (range 1 max-val)
        fil-num (filter #(or (= (mod % 5) 0) (= (mod % 3) 0)) nat-num)]
    (reduce + fil-num)))

(euler-prob-1 1000)