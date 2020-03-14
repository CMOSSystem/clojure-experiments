(defn mergeCustom [arrA arrB rem] 
    (if (empty? arrA)
      (concat rem arrB)
      (if (empty? arrB)
        (concat rem arrA)
        ;A and B not empty -> first element exists
        (if (< (first arrA) (first arrB))
          (mergeCustom (rest arrA) arrB (concat rem (list (first arrA))))
          (mergeCustom arrA (rest arrB) (concat rem (list (first arrB))))
        )
      )
    )
)
;call like (user/mergeCustom '(1 2 4) '(3 5) '())
