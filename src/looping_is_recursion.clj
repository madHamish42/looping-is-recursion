
(ns looping-is-recursion)

(defn power [base exp]
  (let [rec (fn [val exp]
              (if (= exp 0)
                val
                (recur (* val base) (dec exp))))]
    (rec 1 exp)))

(defn last-element [a-seq]
  (let [rec (fn [last rst]
              (if (empty? rst)
                last
                (recur (first rst) (rest rst))))]
    (rec nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (not= (count seq1) (count seq2)) false
   (not= (first seq1) (first seq2)) false
   :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [indx 0
         a-seq a-seq]
    (cond (empty? a-seq) nil
          (pred (first a-seq)) indx
          :else (recur (inc indx) (rest a-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    nil
    (loop [num 0
           total 0
           a-seq a-seq]
      (if (empty? a-seq)
        (/ total num)
        (recur (inc num) (+ total (first a-seq)) (rest a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [result #{}
         a-seq a-seq]
    (if (empty? a-seq)
      result
      (recur (toggle result (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [fn-1 0
         fn 1
         n n]
    (if (= n 0)
      fn-1
      (recur fn (+ fn-1 fn) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [res []
         seq a-seq]
    (println res)
    (if (or (empty? seq)
            (contains? (set res) (first seq)))
      res
      (recur (conj res (first seq)) (rest seq)))))


