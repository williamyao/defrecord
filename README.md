#DEFRECORD

At this point I've used this little utility for creating classes in a few different projects, so it's worth splitting it off into its own library.

```lisp
(record:define rental ()
  (price 0.0) ; explicit default value
  (number-of-days  2)
  customer) ; implicit NIL default value

(record:define movie-rental (rental)
  movie-name)

;; BOA constructor
(defparameter black-beauty (movie-rental 15.00 4 "Bob" "Black Beauty"))
;; default values with MAKE-INSTANCE
(defparameter inception (make-instance 'movie-rental
                                       :price 20.00
                                       :customer "Alice"
                                       :movie-name "Inception"))

(defparameter another-black-beauty (record:copy-record black-beauty))

(defparameter inception-for-someone-else
              (record:copy-with inception
                                'customer "Eve"))
```
