(ns arithmetic-operations
  (:use [clojure.test]))

(defn sum-of-squares
  "Write a program to find the sum of squares of a numeric vector.
  The program should work on a zero-length vector (with an answer of 0)."
  [v]
  (reduce + (map #(* % %) v)))

(deftest test-sum-of-squares
  (testing "Base cases"
    (are [x y] (= x y)
         (sum-of-squares []) 0
         (sum-of-squares [1 1]) 2
         (sum-of-squares [1 2]) 5
         (sum-of-squares [2 2]) 8)))

(defn- sum-of-squares-of-digits [n]
  (sum-of-squares
   (map #(mod % 10)
        (take-while #(not (zero? %))
                    (iterate #(quot % 10) n)))))

(defn happy?
  "Determines if a number is happy or not.

  A happy number is defined by the following process. Starting with any
  positive integer, replace the number by the sum of the squares of its
  digits, and repeat the process until the number equals 1 (where it will
  stay), or it loops endlessly in a cycle which does not include 1. Those
  numbers for which this process ends in 1 are happy numbers, while those
  that do not end in 1 are unhappy numbers (or sad numbers[1])."
  [n]
  (loop [curr n digits #{}]
    (= curr 1)))

(defn happy-numbers
  "Returns a lazy sequence of happy numbers
  (http://en.wikipedia.org/wiki/Happy_number)

  A happy number is defined by the following process. Starting with any
  positive integer, replace the number by the sum of the squares of its digits,
  and repeat the process until the number equals 1 (where it will stay), or it
  loops endlessly in a cycle which does not include 1. Those numbers for which
  this process ends in 1 are happy numbers, while those that do not end in 1
  are unhappy numbers.

  Task: Find and print the first 8 happy numbers. "
  []
  (filter #(happy? %) (iterate inc 1)))

;; http://en.wikipedia.org/wiki/Happy_number
(def happy-nums-below-500
     '(1, 7, 10, 13, 19, 23, 28, 31, 32, 44, 49, 68, 70, 79, 82, 86, 91, 94, 97,
          100, 103, 109, 129, 130, 133, 139, 167, 176, 188, 190, 192, 193, 203,
          208, 219, 226, 230, 236, 239, 262, 263, 280, 291, 293, 301, 302, 310,
          313, 319, 320, 326, 329, 331, 338, 356, 362, 365, 367, 368, 376, 379,
          383, 386, 391, 392, 397, 404, 409, 440, 446, 464, 469, 478, 487, 490,
          496))

(deftest test-happy-numbers
  (testing "First eight happy numbers for http://rosettacode.org/wiki/Happy_Number"
    (are [x y] (= x y)
         (take 8 (happy-numbers)) '(1 7 10 13 19 23 28 31)))

  (testing "Testing Happy numbers below 500"
    (are [x y] (= x y)
         (take (count happy-nums-below-500) (happy-numbers)) happy-nums-below-500)))

(defn ethiopian*
  "A method of multiplying integers using only addition, doubling, and halving.

  Method:

   1. Take two numbers to be multiplied and write them down at the top of two
      columns.
   2. In the left-hand column repeatedly halve the last number, discarding any
      remainders, and write the result below the last in the same column, until
      you write a value of 1.
   3. In the right-hand column repeatedly double the last number and write the
      result below. stop when you add a result in the same row as where the left
      hand column shows 1.
   4. Examine the table produced and discard any row where the value in the left
      column is even.
   5. Sum the values in the right-hand column that remain to produce the result
      of multiplying the original two numbers together"
  [x y]
  (reduce +
          (map second
               (filter #(not (even? (first %)))
                       (take-while #(pos? (first %))
                                   (map vector
                                        (iterate #(bit-shift-right % 1) x)
                                        (iterate #(bit-shift-left % 1) y)))))))

(deftest test-ethiopian*
  (are [x y] (= x y)
      (ethiopian* 2 3) 6
      (ethiopian* 2 -3) -6)

  ;; no overflow
  (is (> (ethiopian* 3
                     (int (/ Integer/MAX_VALUE 2.0))) Integer/MAX_VALUE)) )

(run-tests)
