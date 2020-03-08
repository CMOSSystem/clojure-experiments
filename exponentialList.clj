; calculate x^n, both natural numbers
(defn exp [x n]
     (if (zero? n) 1
         (* x (exp x (dec n)))))

; print cnt exponentials for fixed x
(defn ftv [x cnt] (doseq [i (range 1 cnt)] (println (exp x i))))
; print exponentials for x from 2 to p and n from 1 to cnt
(defn vtv [p cnt] (doseq [i (range 2 p)] (ftv i cnt)))

; call like (vtv 10 5)
