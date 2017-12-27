(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [result base exp]
                 (cond
                   (= 0 exp) result
                   :else (recur (* result base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last a-seq]
                 (cond
                   (empty? a-seq) last
                   :else (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [s1 seq1 s2 seq2]
    (cond
      (and (empty? s1) (empty? s2)) (= seq1 seq2)
      (empty? s1) false
      (empty? s2) false
      (not= (first s1) (first s1)) false
      :else (recur (rest s1) (rest s2)))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         s a-seq]
    (cond
      (empty? s) nil
      (pred (first s)) index
      :else (recur (inc index) (rest s)))))

(defn avg [a-seq]
  (loop [sum 0
         n 0
         s a-seq]
    (cond
      (empty? s) (if (zero? n)
                   0
                   (/ sum n))
      :else (recur (+ sum (first s)) (inc n) (rest s)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{} s a-seq]
    (cond
      (empty? s) result
      :else (recur (toggle result (first s)) (rest s)))))

(defn fast-fibo [n]
  (loop [current 2 f-n 1 f-n-1 1]
    (cond
      (= 0 n) 0
      (= 1 n) 1
      (= current n) f-n
      :else (recur (inc current) (+ f-n f-n-1) f-n))))

(defn cut-at-repetition [a-seq]
  (loop [result [] s a-seq]
    (cond
      (empty? s) result
      (contains? (set result) (first s)) result
      :else (recur (conj result (first s)) (rest s)))))

