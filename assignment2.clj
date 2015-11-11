;Zachary Green
;CS 380 Assignment 2
(ns assignment2)


(defn rot13
  "Rotates charcters of a string forward 13 letters with wrapping. Ignores
  non alphabet characters. Number literals equal to letter's ascii code."
  [text]
    (let [c (int text)]
      (cond
        ;Letters that require wrapping
        (or (and (> c 77) (< c 91)) (and (> c 109) (< c 121))) (char (- c 13))
        ;Letters that don't require wrapping
        (or (and (> c 64) (< c 75)) (and (> c 96) (< c 109))) (char (+ c 13))
        ;Non-letters
        :else (char c))))


(defn zap-chars
  "Removes a set of characters from a string. Casts chars to a set and removes
  each of them from text"
  [chars text]
  (apply str (remove (set chars) text)))


(defn shallow-reverse
  "Reverses a list shallowly. Will reverse the list but not nested lists. Uses
  into function to the build a new list."
  [lst]
  (into () lst))


(defn deep-reverse
  "Reverses a list deeply. Will reverse the list including nested lists using
  the map and shallow-reverse functions."
  [lst]
  (shallow-reverse (map (fn [l] (cond (coll? l) (deep-reverse l) :else l)) lst)))


;Function calls
;(println (apply str (map rot13 "Zachary Green")))
;(println (zap-chars "aeiou" "Zachary Green"))
;(println (shallow-reverse '(1 (2 (3 4 5)) (2 3))))
;(println (deep-reverse '(1 (2 (3 4 5)) (2 3))))

