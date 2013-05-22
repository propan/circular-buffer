# circular-buffer

A circular buffer implementation backed by a persistent vector. Implements most of Clojure collections interfaces (and therefore supports
methods like conj, into, nth, etc) as well as Java collections interfaces.

The implementation provides the following options:

 ------------  ---------------------------------------------------------  ---------------------
    Option                           Description                                Default       
 ------------  ---------------------------------------------------------  ---------------------
  :type         defines the type of values, that will be stored in the     :any
                buffer and therefore the type of the structure that backs
                the buffer (either clojure.core.Vec or
                clojure.lang.PersistentVector).
                Possible values are :any :int :long :float :double :byte
                :short :char or :boolean.  

  :default      defines the default value in the buffer. If omitted         nil, 0, 0.0 or false
                depends on the type of values for which the buffer is
                configured. 

  :direction    defines the direction in which new elements are added      :left
                into the buffer                
 ------------  ---------------------------------------------------------  ---------------------


## Usage

Include the library in your leiningen project dependencies:

```clojure
[circular-buffer "0.1.0-SNAPSHOT"]
```

## Examples

```clojure
(use 'hamakar.circular-buffer)

; defines a new circular buffer of size 5
(def cb-left (cbuf 5)) ; #'hamakar.circular-buffer/cb-left

cb-left ; [nil nil nil nil nil]

(conj cb-left 4) ; [nil nil nil nil 4]

(into cb-left [1 2 3 4 5 6 7 8]) ; [4 5 6 7 8]

(subvec (into cb-left [1 2 3 4 5 6 7 8]) 2) ; [6 7 8]

(def cb-left (cbuf 4 :type :int :default -1)) ; #'hamakar.circular-buffer/cb-left

cb-left ; [-1 -1 -1 -1]

(def cb-right (into (cbuf 4 :direction :right) [1 2 3 4])) ; #'hamakar.circular-buffer/cb-right

cb-right ; [4 3 2 1]

(pop cb-right) ; [3 2 1 nil]

(peek cb-right) ; 4

(= cb-right (list 4 3 2 1)) ; true

(nth cb-right 0) ; 4
```

## License

Copyright © 2013 Pavel Prokopenko

Distributed under the Eclipse Public License, the same as Clojure.
