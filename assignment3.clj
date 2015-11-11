;Zachary Green
(ns assignment3)

(defn abs
  "Return absolute value of n."
  [n]
  (max n (- n)))

(defn new-guess
  "Generate new guess for newton's method."
  [n guess]
  (/ (+ (/ n guess) guess) 2))

(defn check-guess
  "Check if guess within epsilon."
  [n guess]
  (- guess (/ n guess)))

(defn sqrt
  "Computes the square root of the positive number n using Newtown's Method.
  n: number to find square root of
  epsilon: specified tolerance of accuracy. When difference between guess and
    result is less than epsilon return the result.
  guess: initial approximation"
  [n epsilon guess]
  (cond
    ;Check if difference between n and guess is greater than epsilon using
    ;the absolute value of check-guess.
    (< epsilon (abs (check-guess n guess)))
      ;Generate new guess. Recurse with new value of guess using new-guess
      (sqrt n epsilon (nth (iterate (partial new-guess n) guess)  1))
    ;Return result.
    :else guess))

;(sqrt 3 0.0001 1)
