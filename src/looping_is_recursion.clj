(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [rest-seq]
                 (if (<= (count rest-seq) 1)
                   (first rest-seq)
                   (recur (rest rest-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [rest-seq1 rest-seq2]
                 (if (and (empty? rest-seq1) (empty? rest-seq2))
                   true
                   (if (not (= (first rest-seq1) (first rest-seq2)))
                     false
                     (recur (rest rest-seq1) (rest rest-seq2)))))]
    (if (not (= (count seq1) (count seq2)))
      false
      (helper seq1 seq2))))


(defn find-first-index [pred a-seq]
  (loop [some-pred pred
         rest-seq a-seq
         index 0]
    (if (empty? rest-seq)
      nil
      (if (some-pred (first rest-seq))
        index
        (recur some-pred (rest rest-seq) (inc index))))))

(defn avg [a-seq]
  (loop [rest-seq a-seq
         sum 0
         n 0]
    (if (empty? rest-seq)
      (/ sum n)
      (recur (rest rest-seq) (+ sum (first rest-seq)) (inc n)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parity-seq #{}
         rest-a-seq a-seq]
    (if (empty? rest-a-seq)
      parity-seq
      (recur (toggle parity-seq (first rest-a-seq)) (rest rest-a-seq)))))

(defn fast-fibo [n]
  (loop [fib-n-1 0
         fib-n 1
         i n]
    (if (= i 0)
      0
      (if (= i 1)
        fib-n
        (recur fib-n (+ fib-n-1 fib-n) (dec i))))))

(defn cut-at-repetition [a-seq]
  (loop [seen? #{}
         cut-seq []
         rest-a-seq a-seq]
    (if (empty? rest-a-seq)
      cut-seq
      (if (contains? seen? (first rest-a-seq))
        cut-seq
        (recur (conj seen? (first rest-a-seq)) (conj cut-seq (first rest-a-seq)) (rest rest-a-seq))))))

