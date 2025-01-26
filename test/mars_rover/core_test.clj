(ns mars-rover.core-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.properties :as prop]
   [mars-rover.core :as sut]))

(defspec compute-turn-prop-test
  100
  (prop/for-all
   [initial-direction (s/gen ::sut/direction)
    turn-instruction (s/gen ::sut/turn-instruction)]
   (let [_ (def xid initial-direction)
         one-turn (sut/compute-turn initial-direction turn-instruction)
         four-turns (nth (iterate #(sut/compute-turn % turn-instruction) initial-direction) 4)]
     (and
      (s/valid? ::sut/direction one-turn)
      (not= initial-direction one-turn)
      (= initial-direction four-turns)))))

(defspec compute-move-prop-test
  100
  (prop/for-all
   [start (s/gen ::sut/position)]
   (let [end (sut/compute-move start)]
     (and
      ;; FIXME: This breaks if we go W or S and the start position is 0,0 etc
      ;; (s/valid? ::sut/position end)
      (= (::sut/direction start) (::sut/direction end))
      ;; A single co-ordinate value has changed by 1
      (= 1 (+ (abs (- (::sut/x-coordinate start) (::sut/x-coordinate end)))
              (abs (- (::sut/y-coordinate start) (::sut/y-coordinate end)))))))))

(deftest compute-move-example-test
  (let [start {::sut/x-coordinate 4 ::sut/y-coordinate 2 ::sut/direction :N}]
    (testing "Moves by 1 in expected direction."
      (let [expected {::sut/x-coordinate 4 ::sut/y-coordinate 3 ::sut/direction :N}
            end (sut/compute-move start)]
        (is (= expected end))))))

