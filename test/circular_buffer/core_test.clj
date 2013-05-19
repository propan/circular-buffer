(ns circular-buffer.core-test
  (:use clojure.test
        circular-buffer.core))

(deftest test-circular-buffer-creation
  (testing "Creation using circular-buffer function"
           (let [size 5
                 cb (circular-buffer size)] 
             (is (and (== size (count cb))
                      (instance? clojure.lang.IPersistentVector (.items cb))))))
  (testing "Creation using circular-buffer-of function"
    (are [x size] (and (== size (count x)) (instance? clojure.core.Vec (.items x)))
         (circular-buffer-of :boolean 1) 1
         (circular-buffer-of :byte 2) 2
         (circular-buffer-of :short 3) 3
         (circular-buffer-of :int 4) 4
         (circular-buffer-of :long 5) 5
         (circular-buffer-of :float 6) 6
         (circular-buffer-of :double 7) 7
         (circular-buffer-of :char 8) 8))
  )

(deftest test-counted-interface
  (testing "Always returns correct size"
    (let [cb-0 (circular-buffer 3)
          cb-1 (into cb-0 [1 2 3 4 5 6 7])
          cb-2 (conj cb-0 8)]
      (are [x] (= 3 (count x))
           cb-0
           cb-1
           cb-2))))

(deftest test-indexed-interface
  (let [cb (into (circular-buffer 3) [1 2 3 4 5 6 7])]
	  (testing "nth always returns a value"
       (are [pos val] (= val (nth cb pos))
             0 5
             1 6
             2 7
             3 5
             4 6
             5 7))
	  (testing "nth with not-found returns not-found when the index out of bounds"
	     (are [pos val] (= val (nth cb pos :not-found))
	          0 5
	          1 6
	          2 7
	          3 :not-found
	          4 :not-found))))

(deftest test-persistent-collection-interface
  (let [cb-0 (into (circular-buffer 3) [1 2 3 4 5])
        cb-1 (into (circular-buffer 3) [1 2 3 4 6])
        cb-2 (into (circular-buffer 4) [1 2 3 4 5])
        cb-3 (into (circular-buffer-of :boolean 2) [true false true])]
	  (testing "="
	       (are [x] (= x cb-0)
	            cb-0
	            [3 4 5]
	            (list 3 4 5)
	            (range 3 6)))
	  (testing "cons"
	      (are [result input] (= result (.cons input :foo))
	           [4 5 :foo] cb-0))
    (testing "empty"
        (are [a b val] (and (= (count a) (count b)) (zero? (.start b)) (every? #(= val %) b))
             cb-0 (empty cb-0) nil
             cb-3 (empty cb-3) false))
    (testing "equiv"
        (are [a b] (true? (.equiv a b))
             cb-0 cb-0
             cb-1 cb-1
             cb-2 cb-2
             cb-3 cb-3)
        (are [a b] (false? (.equiv a b))
             cb-0 cb-1
             cb-0 cb-2
             cb-1 cb-0
             cb-2 cb-0
             cb-1 cb-2
             cb-1 cb-3))))

(deftest test-persistent-stack-interface
  (let [cb-obj (into (circular-buffer 2) [1 2 3 4])
        cb-int  (into (circular-buffer-of :int 3) [1 2 3])
        cb-bool (into (circular-buffer-of :boolean 4 true) [true false true])]
    (testing "peek"
       (is (= 4 (peek cb-obj))))
    (testing "pop"
       (are [a b] (= a b)
            [nil 3] (pop cb-obj)
            [0 1 2] (pop cb-int)
            [true true true false] (pop cb-bool)
            [nil nil] (pop (pop cb-obj))))
    (testing "Combinations of conj and pop"
       (are [a b] (= a b)
            [3 4] (conj (pop (conj (pop cb-obj) 8)) 4)
            [0 1 8] (conj (pop (pop cb-int)) 8)
            [true true true true] (pop (pop (pop cb-bool)))))))
