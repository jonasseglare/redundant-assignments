(ns redundant-assignments.mul
  (:import [Mul2d]))

(defn gen-array []  (double-array (take 100000 (repeatedly rand))))

(def test-data (vec (take 100 (repeatedly (fn [] {:array (gen-array)
                                                  :angle (rand)})))))

(defn f0 [x] (Mul2d/apply0 (:angle x) (:array x)))
(defn f [x] (Mul2d/apply (:angle x) (:array x)))

(defn benchmark [f]
  (dotimes [i 10]
    (let [angle 3.4342]
      (doseq [x (butlast test-data)]
        (f x))
      (time (f (last test-data))))))

(println "With adding '+ 0'")
(benchmark f0)
(println "Without adding '+ 0'")
(benchmark f)
