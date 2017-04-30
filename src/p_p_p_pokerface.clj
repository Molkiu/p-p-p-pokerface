(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst snd] card]
    (if(Character/isDigit fst)
      (Integer/valueOf (str fst))
      ({\A 14,\K 13,\Q 12,\J 11,\T 10} fst))))

(defn suit [card]
  (let [[fst snd] card]
    (str snd)))

(defn pair? [hand]
  (let [freq (set(map second(frequencies(map rank hand))))]
    (if(freq 2)
    true
    false)))

(defn three-of-a-kind? [hand]
  (let [freq (set(map second(frequencies(map rank hand))))]
    (if(freq 3)
    true
    false)))

(defn four-of-a-kind? [hand]
  (let [freq (set(map second(frequencies(map rank hand))))]
    (if(freq 4)
    true
    false)))

(defn flush? [hand]
  (let [freq (set(map second(frequencies(map suit hand))))]
    (if(freq 5)
    true
    false)))

(defn full-house? [hand]
  (let [freq (set(map second(frequencies(map rank hand))))]
    (if(and (freq 3) (freq 2))
    true
    false)))

(defn two-pairs? [hand]
  (let [freq (into [] (map second(frequencies(map rank hand))))]
    (if(or
         (and (contains? freq 2)
              (= (count (filter (fn [x] (= x 2)) freq)) 2))
         (four-of-a-kind? hand))
    true
    false)))
(def straighty ["2H" "3H" "3D" "4H" "6H"])

(defn straight? [hand]
  (let [handy (apply sorted-set(map rank hand))
        handyless (disj handy 14)]
    (if(=(count handy) 5)
      (if (contains? handy 14)
        (=(- (last handyless) (first handyless)) 3)
        (=(- (last handy) (first handy)) 4))
      false)))

(defn straight-flush? [hand]
  (and (flush? hand)(straight? hand)))

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
