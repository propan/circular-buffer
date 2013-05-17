(ns circular-buffer.core)

(deftype CircularBuffer [^clojure.lang.PersistentVector items
                         ^int size
                         ^int start
                         ^:unsynchronized-mutable ^int hashcode
                         _meta]
  
  Object
  (equals [this o]
    (cond
      (identical? this o) true
      (instance? CircularBuffer o)
      (and
        (= size (.size ^CircularBuffer o))
        (= start (.start ^CircularBuffer o))
        (= items (.items ^CircularBuffer o)))
      :else false))
  (hashCode [_]
    (when (== -1 hashcode)
      (set! hashcode (->> (int 17)
										      (unchecked-multiply-int 31)
										      (unchecked-add-int start)
										      (unchecked-multiply-int 31)
										      (unchecked-add-int (clojure.lang.Util/hash items)))))
    hashcode)
  
  
  clojure.lang.Counted
  (count [_] size)
  
  clojure.lang.IHashEq
  (hasheq [self] (hash self))
  
  clojure.lang.IMeta
  (meta [_] _meta)

  clojure.lang.IObj
  (withMeta [_ m] (CircularBuffer. items size start hashcode m))
  
  clojure.lang.Indexed
  (nth [_ i]
    (let [pos (rem (+ start i) size)]
      (nth items pos)))
  (nth [this i not-found]
     (let [z (int 0)]
       (if (and (>= i z) (< i size))
		       (.nth this i)
		       not-found)))
  
  clojure.lang.IPersistentCollection
  (cons [this val]
    (let [new-items (assoc items start val)]
      (CircularBuffer. new-items size (rem (inc start) size) -1 (meta this))))
  (empty [this]
    (CircularBuffer. (vec (repeat size nil)) size 0 -1 (meta this))) ; TODO: use the same vector type as in the original CB  
  (equiv [this o]
    (cond
      (or (instance? clojure.lang.IPersistentVector o) (instance? java.util.RandomAccess o))
        (and (= size (count o))
             (loop [i (int 0)]
               (cond
	               (= i size) true
	               (= (.nth this i) (nth o i)) (recur (inc i))
	               :else false)))
      (or (instance? clojure.lang.Sequential o) (instance? java.util.List o))
        (clojure.lang.Util/equiv (seq this) (seq o))
      :else false))
  
  clojure.lang.IPersistentStack
  (peek [this]
    (when (> size (int 0))
      (.nth this (dec size))))
  (pop [this]
    (.cons this nil))
  
  clojure.lang.Seqable
  (seq [_]
    (for [i (range size)]
      (nth items (rem (+ start i) size))))
  
  )

(defmethod print-method CircularBuffer [v w]
  ((get (methods print-method) clojure.lang.IPersistentVector) (seq v) w))

(defn circular-buffer
  "Creates an empty circular buffer of the given size."
  [size]
  (CircularBuffer. (vec (repeat size nil)) size 0 -1 nil))