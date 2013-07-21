(def map-defaults (fn [default keys]
                    (letfn [(rec [default keys result]
                              (if (empty? keys)
                                result
                                (rec default (rest keys) (cons (first keys) (cons default result)))))]
                      (apply hash-map (rec default keys '())))))

(def comparisons (fn [comp x y]
                   (let [a (apply comp (list x y))
                         b (apply comp (list y x))]
                     (cond (and (not a) (not b)) :eq
                            a :lt
                            b :gt))))

(def count-seq (fn [seq]
                 (letfn [(rec [seq n]
                           (if (empty? seq)
                             n
                             (rec (rest seq) (inc n))))]
                   (rec seq 0))))

(def my-interpose (fn [default lst]
                 (letfn [(rec [default lst result]
                           (if (empty? lst)
                             result
                             (rec default (butlast lst) (cons (last lst) (cons default result)))))]
                   (butlast (rec default lst '())))))

(def fibonacci (fn fib [n]
                 (cond (= n 1) '(1)
                       (= n 2) '(1 1)
                       true (concat (fib (- n 1)) (list (+ (nth (fib (dec n)) (- n 3)) (nth (fib (dec n)) (- n 2))))))))

(def my-reverse (fn my-rev [lst]
                  (if (empty? lst)
                    '()
                    (cons (last lst) (my-rev (butlast lst))))))

(defn infix-calc [& lst]
  (cond (= (count lst) 1) (nth lst 0)
        (= (count lst) 3) (apply (nth lst 1) (list (nth lst 0) (nth lst 2)))
        true (apply infix-calc (apply infix-calc (the-first lst 3)) (nthrest lst 3))))

(defn the-first [lst n]
  (reverse (nthrest (reverse lst) (- (count lst) n))))

(def infix-calculator (fn infix-calc [& lst]
                        (letfn [(the-first [lst n]
                                  (reverse (nthrest (reverse lst) (- (count lst) n))))]
                          (cond (= (count lst) 1) (nth lst 0)
                                (= (count lst) 3) (apply (nth lst 1) (list (nth lst 0) (nth lst 2)))
                                true (apply infix-calc (apply infix-calc (the-first lst 3)) (nthrest lst 3))))))


(def primes-list (fn [n] (letfn [(next-prime [n]
                  (letfn [(is-prime [x]
                            (not (= 0 (some #{0} (for [i (range 2 x)]
                                              (mod x i))))))]
                    (let [next-int (inc n)]
                      (if (is-prime next-int) next-int (next-prime next-int)))))]
                           (loop [i 0
                                  result [2]]
                             (if (> i (- n 2)) result
                                 (recur (inc i) (conj result (next-prime (last result)))))))))

(def sum-square-of-digits (fn [lst]
                            (letfn [(num2digits [n]
                                      (map #(read-string (str %)) (str n)))
                                    (sum-of-digits [n]
                                      (apply + (map #(* % %) (num2digits n))))]
                              (count (filter #(= % true) (map < lst (map sum-of-digits lst)))))))

(def replicate-sequence (fn [lst n]
                          (letfn [(splice [src items]
                                    (if (= 0 (count items))
                                      src
                                      (splice (conj src (first items)) (rest items))))]
                            (loop [i 0 result '()]
                              (if (> i (dec (count lst)))
                                (reverse result)
                                (recur (inc i) (splice result (repeat n (nth lst i)))))))))

(def replicate-seq-shorter (fn [lst n]
                             (loop [i 0 result '()]
                               (if (> i (dec (count lst)))
                                 result
                                 (recur (inc i) (concat result (repeat n (nth lst i))))))))

(defn splice [src items]
  (if (= 0 (count items))
    src
    (splice (conj src (first items)) (rest items))))