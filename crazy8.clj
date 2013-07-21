(defn start-game [num-players]
  nil)

(defn ask-human []
  nil)

(defn display []
  nil)

(defn display-all []
  nil)

(defn next-player []
  nil)

(defn is-valid-play [card]
  nil)

(defn last-card []
  nil)

(defn set-valid-card [card]
  nil)

(defn sort-hands []
  nil)

(defn mkstr [& args]
  nil)

(defn combine-symbols [& args]
  (apply str args))

(defn nthcdr [n lst]
  (if (zero? n)
    lst
    (nthcdr (- n 1) (rest lst))))

(defn my-subseq [lst start end]
  (let [remove-from-end (- (count lst) end)
        reversed (reverse lst)
        reversed-minus-tail (nthcdr remove-from-end reversed)]
    (nthcdr start (reverse reversed-minus-tail))))

(defn group [source n]
  (assert (not (zero? n)) "(group) error: zero length")
  (letfn [(rec [source acc]
            (let [rest (nthcdr n source)]
              (if (not (empty? rest))
                (rec rest (cons (my-subseq source 0 n) acc))
                (reverse (cons source acc)))))]
    (if (not (empty? source)) (rec source nil) nil)))

(defn my-repeat [symbol times]
  (if (zero? times)
    '()
    (cons symbol (my-repeat symbol (- times 1)))))

(defn index-of-match [s lst]
  (letfn [(rec [s lst inx]
            (if (empty? lst)
              nil
              (do
                (if (= (first lst) s)
                  inx
                  (rec s (rest lst) (+ inx 1))))))]
    (rec s lst 0)))

(def ^:dynamic *deck* '())

(defn new-deck-binding []
  (binding [*deck* '()]
    (doseq [suit '(C D H S)]
      (doseq [rank '(2 3 4 5 6 7 8 9 T J Q K A)]
        (set! *deck* (cons (combine-symbols rank suit) *deck*))))
    *deck*))

(defn new-deck []
  (for [suit '(C D H S)
        rank '(2 3 4 5 6 7 8 9 T J Q K A)]
    (str rank suit)))