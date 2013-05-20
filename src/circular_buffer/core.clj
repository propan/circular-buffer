(ns circular-buffer.core)

(defn- default-value
  [t]
  (case t
	  :any nil
	  :boolean false
	  0))

(defn- empty-vector
  [t size default]
	 (if (= t :any)
	     (vec (repeat size default))
	     (into (vector-of t) (repeat size default))))

(defn- ^long circular-nth
  [^long i ^long start ^long size direction]
  (let [offset (if (= :left direction) 0 1)]
	  (rem (+ start i offset) size)))

(declare circular-previous)

(defn- ^long circular-next
  [^long start ^long size direction]
  (if (= :left direction)
    (rem (inc start) size)
    (circular-previous start size :left)))

(defn- ^long circular-previous
  [^long start ^long size direction]
  (if (= :left direction)
    (rem (+ start size -1) size)
    (circular-next start :left)))
;
; TODO: include direction to hash and equals
;
(deftype CircularBuffer [^clojure.lang.IPersistentVector items
                         ^int size ^int start
                         ^:unsynchronized-mutable ^int hashcode
                          type default direction _meta]
  
  Object
  (equals [this o]
    (cond
      (identical? this o) true
      (instance? CircularBuffer o)
      (and
        (= size      (.size ^CircularBuffer o))
        (= start     (.start ^CircularBuffer o))
        (= direction (.direction ^CircularBuffer o))
        (= items     (.items ^CircularBuffer o)))
      :else false))
  (hashCode [_]
    (when (== -1 hashcode)
      (set! hashcode (->> (int 17)
										      (unchecked-multiply-int 31)
										      (unchecked-add-int start)
                          (unchecked-multiply-int 31)
										      (unchecked-add-int (clojure.lang.Util/hash direction))
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
  (withMeta [_ m] (CircularBuffer. items size start hashcode type default direction m))
  
  clojure.lang.Indexed
  (nth [_ i]
    (let [pos (circular-nth i start size direction)]
      (nth items pos)))
  (nth [this i not-found]
     (let [z (int 0)]
       (if (and (>= i z) (< i size))
		       (.nth this i)
		       not-found)))
  
  clojure.lang.IPersistentCollection
  (cons [this val]
    (let [new-items (assoc items start val)]
      (CircularBuffer. new-items size (circular-next start size direction) -1 type default direction (meta this))))
  (empty [this]
    (CircularBuffer. (empty-vector type size default) size 0 -1 type default direction (meta this)))  
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
    (let [prv-start (circular-previous start size direction)
          new-items (assoc items prv-start default)]
      (CircularBuffer. new-items size prv-start -1 type default direction (meta this))))
  
  clojure.lang.IPersistentVector
  (assocN [this i val]
    (let [new-items (assoc items (circular-nth i start size direction) val)]
      (CircularBuffer. new-items size start -1 type default direction (meta this))))
  
  clojure.lang.Associative
  (assoc [this k v]
    (if (clojure.lang.Util/isInteger k)
      (.assocN this k v)
      (throw (IllegalArgumentException. "Key must be integer"))))
  (containsKey [this k]
    (and (clojure.lang.Util/isInteger k)
         (<= 0 (int k))
         (< (int k) size)))
  (entryAt [this k]
    (when (.containsKey this k)
      (clojure.lang.MapEntry. k (.nth this (int k)))))
  
  clojure.lang.ILookup
  (valAt [this k not-found]
    (if (clojure.lang.Util/isInteger k)
        (.nth this (int k))
        not-found))
  (valAt [this k] (.valAt this k nil))
  
  clojure.lang.IFn
  (invoke [this k]
    (if (clojure.lang.Util/isInteger k)
      (let [i (int k)]
        (if (and (>= i 0) (< i size))
          (.nth this i)
          (throw (IndexOutOfBoundsException.))))
      (throw (IllegalArgumentException. "Key must be integer"))))
  
  clojure.lang.Seqable
  (seq [this]
    (for [i (range size)]
      (.nth this i)))

  clojure.lang.Sequential
  
  java.lang.Iterable
  (iterator [this]
    (let [i (java.util.concurrent.atomic.AtomicInteger. 0)]
      (reify java.util.Iterator
        (hasNext [_] (< (.get i) size))
        (next [_] (.nth this (dec (.incrementAndGet i))))
        (remove [_] (throw (UnsupportedOperationException.))))))
  
  java.util.Collection
  (contains [_ o] (boolean (some #(= % o) items)))
  (containsAll [this c] (every? #(.contains this %) c))
  (isEmpty [_] (zero? size))
  (toArray [this] (into-array Object (.seq this)))
  (toArray [this arr]
    (if (>= (count arr) size)
      (do
        (dotimes [i size]
          (aset arr i (.nth this i)))
        arr)
      (into-array Object this)))
  (size [_] size)
  (add [_ o] (throw (UnsupportedOperationException.)))
  (addAll [_ c] (throw (UnsupportedOperationException.)))
  (clear [_] (throw (UnsupportedOperationException.)))
  (^boolean remove [_ o] (throw (UnsupportedOperationException.)))
  (removeAll [_ c] (throw (UnsupportedOperationException.)))
  (retainAll [_ c] (throw (UnsupportedOperationException.)))
)

(defmethod print-method CircularBuffer [v w]
  ((get (methods print-method) clojure.lang.IPersistentVector) (seq v) w))

(defn cbuf
  "Creates an empty circular buffer backed by a new vector of a
  single type t, where t is one of :any :int :long :float
  :double :byte :short :char or :boolean."
  [size & {:keys [type direction default]
           :or {type :any direction :left default :type-default}}]
  (let [def-val  (if (= default :type-default)
                     (default-value type)
                      default)
        init-vec (empty-vector type size def-val)]
    (CircularBuffer. init-vec size 0 -1 type def-val direction nil)))