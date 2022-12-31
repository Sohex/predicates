(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [an-item] (contains? a-set an-item)))

(defn pred-and [pred1 pred2]
  (let [pred1? (fn [n] (pred1 n))
        pred2? (fn [n] (pred2 n))]
    (fn [x] (and (pred1? x) (pred2? x)))))

(defn pred-or [pred1 pred2]
  (let [pred1? (fn [n] (pred1 n))
        pred2? (fn [n] (pred2 n))]
    (fn [x] (or (pred1? x) (pred2? x)))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [this-book-has-award? (fn [award] (contains? (:awards book) award))]
  (every? this-book-has-award? awards)))

(defn my-some [pred a-seq]
  (first (filter boolean (map pred a-seq)))) 

(defn my-every? [pred a-seq]
  (let [map-pred (map pred a-seq)]
    (= (count (filter true? map-pred)) (count map-pred)))) 

(defn prime? [n]
  (let [is-prime? (fn [r] (every? pos? (map mod (repeat (- n 2) n) r)))]
    (is-prime? (range 2 n))))
;^^
