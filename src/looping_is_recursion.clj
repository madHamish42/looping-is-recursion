
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
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

(find-first-index nil? [])
