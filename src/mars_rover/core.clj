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

;; ------------
;; ???
;; ------------

(s/fdef compute-turn
  :args (s/cat :initial-direction ::direction
               :turn-instruction ::turn-instruction)
  :ret ::direction)

(defn compute-turn
  [initial-direction turn-instruction]
  (let [turns {:N {:L :W, :R :E}
               :E {:L :N, :R :S}
               :S {:L :E, :R :W}
               :W {:L :S, :R :N}}]
    (get-in turns [initial-direction turn-instruction])))
    
(s/fdef compute-move
        :args (s/cat :initial-position ::position)
        :ret ::position)

(defn compute-move
  [{::keys [direction] :as initial-position}]
  (case direction
      :N (update initial-position ::y-coordinate inc)
      :E (update initial-position ::x-coordinate inc)
      :S (update initial-position ::y-coordinate dec)
      :W (update initial-position ::x-coordinate dec)))

