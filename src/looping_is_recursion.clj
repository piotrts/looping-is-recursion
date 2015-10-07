(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
         base base
         exp exp]
    (if (zero? exp)
      acc
      (recur (* acc base) base (dec exp)))))

(defn last-element [a-seq]
  (loop [a-seq-left a-seq]
    (if (empty? (rest a-seq-left))
      (first a-seq-left)
      (recur (rest a-seq-left)))))

(defn seq= [seq1 seq2]
  (loop [seq1 seq1
         seq2 seq2]
    (cond
      (and (empty? seq1) (empty? seq2))
        true
      (or (empty? seq1) (empty? seq2))
        false
      :else
        (if (= seq1 seq2)
          (recur (rest seq1) (rest seq2))
          false))))

(defn find-first-index [pred a-seq]
  (loop [a-seq-left a-seq
         index 0]
    (when (not (empty? a-seq-left))
      (if (pred (first a-seq-left))
        index
        (recur (rest a-seq-left) (inc index))))))

(defn avg [a-seq]
  (loop [a-seq-left a-seq
         acc-value 0
         acc-count 0]
    (if (empty? a-seq-left)
      (/ acc-value acc-count)
      (recur (rest a-seq-left) (+ acc-value (first a-seq-left)) (inc acc-count)))))

(defn parity [a-seq]
  (loop [left a-seq result #{}]
    (if (empty? left)
      result
      (recur
        (rest left)
        (if (some #{(first left)} result)
          (disj result (first left))
          (conj result (first left)))))))

(defn fast-fibo [n]
  (if (< n 2)
    n
    (loop [x (- n 2) fibl 1 fibll 0]
      (if (zero? x)
        (+ fibl fibll)
        (recur (dec x) (+ fibl fibll) fibl)))))

(defn cut-at-repetition [a-seq]
  (loop [acc (vector) left a-seq]
    (if (or (empty? left)
            (some #{(first left)} acc))
      acc
      (recur
        (conj acc (first left))
        (rest left)))))
