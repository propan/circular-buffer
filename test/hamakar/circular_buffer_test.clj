(ns hamakar.circular-buffer-test
  (:use clojure.test
        hamakar.circular-buffer))

(deftest test-circular-buffer-creation
  (testing "The direction defines correct order of items"
    (are [a b] (= a b)
      (into (cbuf 4 :direction :left ) [1 2]) [nil nil 1 2]
      (into (cbuf 4 :direction :right ) [1 2]) [2 1 nil nil]
      (into (cbuf 4 :direction :left ) [1 2 3 4 5]) [2 3 4 5]
      (into (cbuf 4 :direction :right ) [1 2 3 4 5]) [5 4 3 2]))
  (testing "A buffer of specific type is backed with a right data structure"
    (let [size 5 cb (cbuf size)]
      (is (and (== size (count cb))
            (instance? clojure.lang.IPersistentVector (.items cb)))))
    (are [x size] (and (== size (count x)) (instance? clojure.core.Vec (.items x)))
      (cbuf 1 :type :boolean ) 1
      (cbuf 2 :type :byte ) 2
      (cbuf 3 :type :short ) 3
      (cbuf 4 :type :int ) 4
      (cbuf 5 :type :long ) 5
      (cbuf 6 :type :float ) 6
      (cbuf 7 :type :double ) 7
      (cbuf 8 :type :char ) 8))
  )

(deftest test-counted-interface
  (testing "Always returns correct size"
    (let [cb-0 (cbuf 3)
          cb-1 (into cb-0 [1 2 3 4 5 6 7])
          cb-2 (conj cb-0 8)]
      (are [x] (= 3 (count x))
        cb-0
        cb-1
        cb-2))))

(deftest test-indexed-interface
  (let [cb-l (into (cbuf 3 :direction :left ) [1 2 3 4 5 6 7])
        cb-r (into (cbuf 3 :direction :right ) [1 2 3 4 5 6 7])]
    (testing "nth always returns a value"
      (are [cb pos val] (= val (nth cb pos))
        cb-l 0 5
        cb-l 1 6
        cb-l 2 7
        cb-l 3 5
        cb-l 4 6
        cb-l 5 7
        cb-r 0 7
        cb-r 1 6
        cb-r 2 5
        cb-r 3 7
        cb-r 4 6
        cb-r 5 5))
    (testing "nth with not-found returns not-found when the index out of bounds"
      (are [cb pos val] (= val (nth cb pos :not-found ))
        cb-l 0 5
        cb-l 1 6
        cb-l 2 7
        cb-l 3 :not-found cb-l 4 :not-found cb-r 0 7
        cb-r 1 6
        cb-r 2 5
        cb-r 3 :not-found cb-r 4 :not-found ))))

(deftest test-persistent-collection-interface
  (let [cb-0 (into (cbuf 3) [1 2 3 4 5])
        cb-1 (into (cbuf 3) [1 2 3 4 6])
        cb-2 (into (cbuf 4) [1 2 3 4 5])
        cb-3 (into (cbuf 2 :type :boolean ) [true false true])
        cb-4 (into (cbuf 3 :direction :right ) [1 2 3 4 5])]
    (testing "="
      (are [x] (= x cb-0)
        cb-0
        [3 4 5]
        (list 3 4 5)
        (range 3 6)))
    (testing "cons"
      (are [result input] (= result (.cons input :foo ))
        [4 5 :foo ] cb-0
        [:foo 5 4] cb-4))
    (testing "empty"
      (are [a b val] (and (= (count a) (count b)) (zero? (.start b)) (every? #(= val %) b))
        cb-0 (empty cb-0) nil
        cb-3 (empty cb-3) false
        cb-4 (empty cb-4) nil))
    (testing "equiv"
      (are [a b] (true? (.equiv a b))
        cb-0 cb-0
        cb-1 cb-1
        cb-2 cb-2
        cb-3 cb-3
        cb-4 cb-4)
      (are [a b] (false? (.equiv a b))
        cb-0 cb-1
        cb-0 cb-2
        cb-1 cb-0
        cb-2 cb-0
        cb-1 cb-2
        cb-1 cb-3
        cb-0 cb-4))))

(deftest test-persistent-stack-interface
  (let [cb-ob-1 (into (cbuf 2) [1 2 3 4])
        cb-ob-2 (into (cbuf 2 :direction :right ) [1 2 3 4])
        cb-int (into (cbuf 3 :type :int ) [1 2 3])
        cb-bool (into (cbuf 4 :type :boolean :default true) [true false true])]
    (testing "peek"
      (are [cb v] (= v (peek cb))
        cb-ob-1 4
        cb-ob-2 4))
    (testing "pop"
      (are [a b] (= a b)
        [nil 3] (pop cb-ob-1)
        [3 nil] (pop cb-ob-2)
        [0 1 2] (pop cb-int)
        [true true true false] (pop cb-bool)
        [nil nil] (pop (pop cb-ob-1))))
    (testing "Combinations of conj and pop"
      (are [a b] (= a b)
        [3 4] (conj (pop (conj (pop cb-ob-1) 8)) 4)
        [4 3] (conj (pop (conj (pop cb-ob-2) 8)) 4)
        [0 1 8] (conj (pop (pop cb-int)) 8)
        [true true true true] (pop (pop (pop cb-bool)))))))
