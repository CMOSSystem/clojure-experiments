(require '[clojure.string :as str])

; A solved cube
(def solved {:R '(:R :R :R :R :R :R :R :R :R)
  :U '(:U :U :U :U :U :U :U :U :U)
  :F '(:F :F :F :F :F :F :F :F :F)
  :L '(:L :L :L :L :L :L :L :L :L)
  :B '(:B :B :B :B :B :B :B :B :B)
  :D '(:D :D :D :D :D :D :D :D :D)})

(def test {:R '(:R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9)
:U '(:U1 :U2 :U3 :U4 :U5 :U6 :U7 :U8 :U9)
:F '(:F1 :F2 :F3 :F4 :F5 :F6 :F7 :F8 :F9)
:L '(:L1 :L2 :L3 :L4 :L5 :L6 :L7 :L8 :L9)
:B '(:B1 :B2 :B3 :B4 :B5 :B6 :B7 :B8 :B9)
:D '(:D1 :D2 :D3 :D4 :D5 :D6 :D7 :D8 :D9)})

;;;;;;;;;;;;;;;;;;;;;;
; Apply single moves ;
;;;;;;;;;;;;;;;;;;;;;;

;Return a single face of a state rotated by 90 degrees
(defn rotate-face [state face]
  (let [substate (face state)]
    (list
    (nth substate 6)
    (nth substate 3)
    (first substate)
    (nth substate 7)
    (nth substate 4)
    (nth substate 1)
    (last substate)
    (nth substate 5)
    (nth substate 2)
  )
  )
)
;Double move
(defn rotate-face-2 [state face]
  (rotate-face (hash-map face (rotate-face state face)) face)
)
;Triple move
(defn rotate-face-3 [state face]
  (rotate-face (hash-map face (rotate-face-2 state face)) face)
)

;Swap the top third of the 23 face with the 13 face to construct U moves. Returns the modified 23 face
(defn swap-top [state face23 face13]
  (list
  (first (face13 state))
  (nth (face13 state) 1)
  (nth (face13 state) 2)
  (nth (face23 state) 3)
  (nth (face23 state) 4)
  (nth (face23 state) 5)
  (nth (face23 state) 6)
  (nth (face23 state) 7)
  (nth (face23 state) 8)
  (last (face23 state))
  )
)

;x Rotation
(defn move-x [state]
  (hash-map
    :R (rotate-face state :R)
    :U (:F state)
    :F (:D state)
    :D (rotate-face-2 state :B)
    :B (rotate-face-2 state :U)
    :L (rotate-face-3 state :L)
  )
)

;y Rotation
(defn move-y [state]
  (hash-map
    :R (:B state)
    :U (rotate-face state :U)
    :F (:R state)
    :D (rotate-face-3 state :D)
    :B (:L state)
    :L (:F state)
  )
)

(defn move-u [state]
  (hash-map 
    :R (swap-top state :R :B)
    :U (rotate-face state :U)
    :F (swap-top state :F :R)
    :D (:D state)
    :B (swap-top state :B :L)
    :L (swap-top state :L :F)
  )
)

(defn move-f [state]
  (move-x (move-x (move-x (move-u (move-x state)))))
)

(defn move-r [state]
  (move-y (move-y (move-y (move-f (move-y state)))))
)

(defn move-d [state]
  (move-x (move-x (move-x (move-f (move-x state)))))
)

(defn move-z [state]
  (move-y (move-x (move-y (move-y (move-y state)))))
)

(defn move-y-opposite [move state] (move-y (move-y (move (move-y (move-y state))))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Apply move sequences ;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-move [move state]
  (case (str (first move))
  "R" (move-r state)
  "F" (move-f state)
  "U" (move-u state)
  "L" (move-y-opposite move-r state)
  "B" (move-y-opposite move-f state)
  "D" (move-d state)
  "x" (move-x state)
  "y" (move-y state)
  "z" (move-z state)
  (print "Unknown move")
  )
)

(defn apply-nmove [state nmove]
  (let [n (cond
    (= (count nmove) 1) 1
    (= (last nmove) \2) 2
    :else               3)]
  (->> (iterate (partial apply-move nmove) state)
    (drop 1)
    (take n)
    (last)))
)

(defn apply-sequence [state moves]
  (reduce apply-nmove state moves)
)

(defn apply-stringSequence [state moves]
  (reduce apply-nmove state (str/split moves #" "))
)

;;;;;;;;;;;;;;;;;;;;;;
; Printing functions ;
;;;;;;;;;;;;;;;;;;;;;;

;Assign a color for each face. R=Red, W=White, O=Orange, G=Green, Y=Yellow, B=Blue. Only the first char gets mapped, so R8 is the same as R here
(def color-map {"R" "R" "U" "W" "L" "O" "F" "G" "D" "Y" "B" "B"})

;Get a color for printing
(defn get-color [state face index]
  (let [k (-> state (get face) (nth index) (name) (first) (str))]
  (get color-map k))
)

;Get three consecutive colors as string for printing
(defn get-3cons-colors [state face index]
  (str
    (get-color state face index)
    (get-color state face (+ 1 index)) 
    (get-color state face (+ 2 index))
  )
)

(defn print-cube [state]
  (do
  (println "      -------")
  (println "      |" (get-3cons-colors state :U 0) "|")
  (println "      |" (get-3cons-colors state :U 3) "|")
  (println "      |" (get-3cons-colors state :U 6) "|")
  (println "-------------------------")
  (println "|" (get-3cons-colors state :L 0) "|" (get-3cons-colors state :F 0) "|" (get-3cons-colors state :R 0) "|" (get-3cons-colors state :B 0) "|")
  (println "|" (get-3cons-colors state :L 3) "|" (get-3cons-colors state :F 3) "|" (get-3cons-colors state :R 3) "|" (get-3cons-colors state :B 3) "|")
  (println "|" (get-3cons-colors state :L 6) "|" (get-3cons-colors state :F 6) "|" (get-3cons-colors state :R 6) "|" (get-3cons-colors state :B 6) "|")
  (println "-------------------------")
  (println "      |" (get-3cons-colors state :D 0) "|")
  (println "      |" (get-3cons-colors state :D 3) "|")
  (println "      |" (get-3cons-colors state :D 6) "|")
  (println "      -------")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate random scramble ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def max-scramble-length 25)
(def opposites {
  "R" "L" "L" "R"
  "U" "D" "D" "U"
  "F" "B" "B" "F"}
)

(defn next-move [cur-move]
  (str (rand-nth (into '() (disj #{"R" "U" "F" "D" "B" "L"} (str (first cur-move)) (opposites (str (first cur-move)))))) (rand-nth '("" "'" "2")))
)

(defn add-move [scr]
  (if (< (count scr) max-scramble-length)
  (add-move (flatten (list scr (next-move (last scr)))))
  scr
  )
)

(defn get-scramble []
  (str/join " " (add-move '()))
)


;Example: T-Perm. Only swaps 4 pieces.
;(print-cube (apply-stringSequence solved "R U R' U' R' F R2 U' R' U' R U R' F'"))
;
;Print randomly scrambled cube:
;(print-cube (apply-stringSequence solved (get-scramble)))
