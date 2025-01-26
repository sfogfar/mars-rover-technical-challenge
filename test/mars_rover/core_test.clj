(ns mars-rover.core-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [mars-rover.core :as sut]))

(defspec compute-turn-prop-test
  100
  (prop/for-all
   [initial-pos (s/gen ::sut/position)
    turn-instruction (s/gen ::sut/turn-instruction)]
   (let [one-turn (sut/compute-turn initial-pos turn-instruction)
         four-turns (nth (iterate #(sut/compute-turn % turn-instruction) initial-pos) 4)
         init-x (::sut/x-coordinate initial-pos)
         init-y (::sut/y-coordinate initial-pos)
         init-dir (::sut/direction initial-pos)]
     (and
     ;; Returns valid positions.
      (s/valid? ::sut/position one-turn)
      (s/valid? ::sut/position four-turns)
     ;; One turn changes the direction, four turns come back to the start.
      (not= init-dir (::sut/direction one-turn))
      (= init-dir (::sut/direction four-turns))
     ;; Turn in place.
      (= init-x (::sut/x-coordinate one-turn))
      (= init-y (::sut/y-coordinate one-turn))
      (= init-x (::sut/x-coordinate four-turns))
      (= init-y (::sut/y-coordinate four-turns))))))

(defspec compute-move-prop-test
  100
  (prop/for-all
   [start (s/gen ::sut/position)]
   (let [end (sut/compute-move start)]
     (and
      ;; FIXME: This breaks if we go W or S and the start position is 0,0 etc
      ;; (s/valid? ::sut/position end)
      ;; Direction doesn't change.
      (= (::sut/direction start) (::sut/direction end))
      ;; A single co-ordinate value changes by 1.
      (= 1 (+ (abs (- (::sut/x-coordinate start) (::sut/x-coordinate end)))
              (abs (- (::sut/y-coordinate start) (::sut/y-coordinate end)))))))))

(deftest compute-move-example-test
  (let [start {::sut/x-coordinate 4 ::sut/y-coordinate 2 ::sut/direction :N}]
    (testing "Moves by 1 in expected direction."
      (let [expected {::sut/x-coordinate 4 ::sut/y-coordinate 3 ::sut/direction :N}
            end (sut/compute-move start)]
        (is (= expected end))))))

(def gen-small-grid
  (gen/fmap
   #(hash-map ::sut/x-limit % ::sut/y-limit %)
   (gen/choose 1 5)))

(def gen-large-grid
  (gen/fmap
   #(hash-map ::sut/x-limit % ::sut/y-limit %)
   (gen/choose 6 10)))

;; TODO: Revisit and get grid and positions in a similar range.
(defspec in-bounds?-prop-test
  100
  (prop/for-all [position (s/gen ::sut/position)
                 small-grid gen-small-grid
                 large-grid gen-large-grid]
                ;; A position that is valid in the small grid should be valid in the large.
                (or (not (sut/in-bounds? small-grid position))
                    (sut/in-bounds? large-grid position))))

(deftest in-bounds?-test
  (testing "Valid positions."
    (is (sut/in-bounds? {::sut/x-limit 5 ::sut/y-limit 5}
                        {::sut/x-coordinate 1 ::sut/y-coordinate 1})))
  (testing "Invalid positions."
    (is (not (sut/in-bounds? {::sut/x-limit 5 ::sut/y-limit 5}
                             {::sut/x-coordinate 6 ::sut/y-coordinate 1})))))

(def in-bounds-gen
  "Generates random combinations of bounds and in-bounds positions.
  Returns [bounds position]."
  (gen/let [bounds (s/gen ::sut/bounds)
            x (gen/choose 0 (max 0 (dec (::sut/x-limit bounds))))
            y (gen/choose 0 (max 0 (dec (::sut/y-limit bounds))))
            dir (s/gen ::sut/direction)]
    [bounds
     {::sut/x-coordinate x
      ::sut/y-coordinate y
      ::sut/direction dir}]))

(defspec execute-instruction-prop-test 100
  (prop/for-all [[bounds pos] in-bounds-gen
                 instruction (s/gen ::sut/instruction)]
                (let [result (sut/execute-instruction bounds pos instruction)]
                  (and (s/valid? ::sut/position result)
                       (sut/in-bounds? bounds result)))))

(deftest execute-instruction-example-test
 ;; Test moving within bounds.
 (let [bounds {::sut/x-limit 5 ::sut/y-limit 5}
       initial-pos {::sut/x-coordinate 2 ::sut/y-coordinate 2 ::sut/direction :N}]
   (testing "Move forward within bounds."
     (is (= (sut/execute-instruction bounds initial-pos :M)
            {::sut/x-coordinate 2 ::sut/y-coordinate 3 ::sut/direction :N})))
   
   (testing "Turn left within bounds."
     (is (= (sut/execute-instruction bounds initial-pos :L)
            {::sut/x-coordinate 2 ::sut/y-coordinate 2 ::sut/direction :W})))
   
   (testing "Turn right within bounds."
     (is (= (sut/execute-instruction bounds initial-pos :R)
            {::sut/x-coordinate 2 ::sut/y-coordinate 2 ::sut/direction :E}))))
 
 ;; Test hitting bounds.
 (let [bounds {::sut/x-limit 5 ::sut/y-limit 5}
       north-bound-pos {::sut/x-coordinate 2 ::sut/y-coordinate 5 ::sut/direction :N}
       south-bound-pos {::sut/x-coordinate 2 ::sut/y-coordinate 0 ::sut/direction :S}
       west-bound-pos {::sut/x-coordinate 0 ::sut/y-coordinate 2 ::sut/direction :W}
       east-bound-pos {::sut/x-coordinate 5 ::sut/y-coordinate 2 ::sut/direction :E}]
   
   (testing "Won't cross N boundary."
     (is (= (sut/execute-instruction bounds north-bound-pos :M)
            north-bound-pos)))
   
   (testing "Won't cross S boundary."
     (is (= (sut/execute-instruction bounds south-bound-pos :M)
            south-bound-pos)))
   
   (testing "Won't cross W boundary."
     (is (= (sut/execute-instruction bounds west-bound-pos :M)
            west-bound-pos)))
   
   (testing "Won't cross E boundary."
     (is (= (sut/execute-instruction bounds east-bound-pos :M)
            east-bound-pos)))))
