(ns mars-rover.core
  (:require
   [clojure.spec.alpha :as s]))

;; ------------
;; Model
;; ------------

(s/def ::x-coordinate nat-int?)
(s/def ::y-coordinate nat-int?)
(s/def ::direction #{:N :E :S :W})

(s/def ::position
  (s/keys :req [::x-coordinate
                ::y-coordinate
                ::direction]))

(s/def ::turn-instruction #{:L :R})

(s/def ::x-limit nat-int?)
(s/def ::y-limit nat-int?)

;; Per the problem statement we can assume the lower bounds to be 0,0.
(s/def ::bounds
  (s/keys :req [::x-limit
                ::y-limit]))

(s/def ::instruction #{:M :L :R})

;; ------------
;; Manoeuver
;; ------------

(defn compute-turn
  [{::keys [direction] :as pos}
   instruction]
  (let [turns {:N {:L :W, :R :E}
               :E {:L :N, :R :S}
               :S {:L :E, :R :W}
               :W {:L :S, :R :N}}
        new-direction (get-in turns [direction instruction])]
    (assoc pos ::direction new-direction)))

(defn compute-move
  [{::keys [direction] :as pos}]
  (case direction
    :N (update pos ::y-coordinate inc)
    :E (update pos ::x-coordinate inc)
    :S (update pos ::y-coordinate dec)
    :W (update pos ::x-coordinate dec)))

(defn in-bounds?
  [{::keys [x-limit y-limit]}
   {::keys [x-coordinate y-coordinate]}]
  (and (<= 0 x-coordinate x-limit)
       (<= 0 y-coordinate y-limit)))

(defn execute-instruction
  [bounds pos instruction]
  (let [new-pos (case instruction
                  (:L :R) (compute-turn pos instruction)
                  :M (compute-move pos))]
    (if (in-bounds? bounds new-pos)
      new-pos
      pos)))

;; ------------
;; Parse input
;; ------------
