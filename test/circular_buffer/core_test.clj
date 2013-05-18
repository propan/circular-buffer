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
  (testing "nth always returns a value"
     (let [cb (into (circular-buffer 3) [1 2 3 4 5 6 7])]
       (are [pos val] (= val (nth cb pos))
             0 5
             1 6
             2 7
             3 5
             4 6
             5 7)))
  (testing "nth with not-found returns not-found when the index out of bounds"
     (let [cb (into (circular-buffer 3) [1 2 3 4 5])]
       (are [pos val] (= val (nth cb pos :not-found))
            0 3
            1 4
            2 5
            3 :not-found
            4 :not-found))))

(deftest test-persistent-collection-interface
  (testing "="
     (let [cb (into (circular-buffer 3) [1 2 3 4 5])]
       (are [x] (= x cb)
            cb
            [3 4 5]
            (list 3 4 5)
            (range 3 6)))))
