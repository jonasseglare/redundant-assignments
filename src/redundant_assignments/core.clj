(ns redundant-assignments.core
  (:import [CrossProdSum]))

(def var-counter (atom 0))

(defn genvar [prefix]
  (str prefix "_" (swap! var-counter inc)))

(defn gen-redundant-assignments [n type var-name expr]
  (let [intermediates (take n (repeatedly (partial genvar var-name)))
        lhs (reduce into [] [intermediates [var-name]])
        rhs (reduce into [] [[expr] intermediates])]
    (mapv (fn [l r]
            (str type " " l " = " r ";"))
          lhs
          rhs)))

(defn cross-product-element [n var-name i j]
  (let [A (str "x" i "y" j)
        B (str "x" j "y" i)]
    [(gen-redundant-assignments n "double" A (str "x" i "*y" j))
     (gen-redundant-assignments n "double" B (str "x" j "*y" i))
     (gen-redundant-assignments n "double" var-name (str A "-" B))]))

(defn method-name [n]
  (str "eval" n))

(defn calc-cross-prod [n]
  [(cross-product-element n "a" 1 2)
   (cross-product-element n "b" 2 0)
   (cross-product-element n "c" 0 1)])

(defn cross-product-elem-sum [n]
  [[(str "public static double eval" n "(double[] X, double[] Y) {")
    ["double sum = 0.0;"
     "assert(X.length == Y.length);"
     "int iters = X.length/3;"
     "for (int i = 0; i < iters; i++) {"
     ["int at = 3*i;"
      "double x0 = X[at + 0];"
      "double x1 = X[at + 1];"
      "double x2 = X[at + 2];"
      "double y0 = Y[at + 0];"
      "double y1 = Y[at + 1];"
      "double y2 = Y[at + 2];"
      (calc-cross-prod n)
      "sum += a + b + c;"]
     "}"]
    "return sum;"]
   "}"])

(defn gen-prod-prod-class [ns]
  ["public class CrossProdSum {"
   (map cross-product-elem-sum  ns)
   ["public static double eval(int n, double[] X, double[] Y) {"
    ["switch (n) {"
     (map (fn [i] (str  "case " i ": return eval" i "(X, Y);")) ns)
     "}"
     "assert(false);"
     "return -1;"]
    "}"
    ]
   "}"])

(defn to-indented [dst prefix x]
  (if (string? x)
    (str dst prefix x)
    (reduce (fn [dst x]
              (to-indented dst (str prefix "  ") x))
            dst x)))

(defn main-to-indented [x]
  (to-indented "" "\n" x))

(defn output-class [ns]
  (->> ns
       gen-prod-prod-class
       main-to-indented
       (spit "src/java/CrossProdSum.java")))

(def counts (->  (conj (set (map (fn [x]
                                   (Math/round x))
                                 (take 22 (iterate (partial * 1.3) 1.0))))
                       0)
                 sort))


(println "counts" counts)

(comment

  (output-class counts)
  )


(def l 10000)
(def X (double-array (take l (repeatedly rand))))
(def Y (double-array (take l (repeatedly rand))))

(defn exercise [n]
  (dotimes [i 100]
    (CrossProdSum/eval n X Y))
  (println "For n=" n)
  (let [r  (time (CrossProdSum/eval n X Y))]
    (println "   Result=" r)))

(doseq [i counts]
  (exercise i))
