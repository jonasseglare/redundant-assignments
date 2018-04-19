(ns redundant-assignments.core
  (:import [CrossProdSum]
           [org.codehaus.janino SimpleCompiler]))

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

(defn gen-prod-prod-class [class-name ns]
  ["public class " class-name " {"
   (map cross-product-elem-sum  ns)
   ["public double eval(int n, double[] X, double[] Y) {"
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
       (gen-prod-prod-class "CrossProdSum")
       main-to-indented
       (spit "src/java/CrossProdSum.java")))

(def counts (->  (conj (set (map (fn [x]
                                   (Math/round x))
                                 (take 18 (iterate (partial * 1.4) 1.0))))
                       0)
                 sort))


(println "counts" counts)

(defn cross-prod-janino-instance [ns]
  (let [cn "CrossProdSumJanino"
        sc (SimpleCompiler.)]
    (.cook sc (->> ns
                   (gen-prod-prod-class cn)
                   main-to-indented))
    (let [cl (.loadClass (.getClassLoader sc) cn)]
      (.newInstance cl))))

(def janino-cross-prod-class (cross-prod-janino-instance counts))

;; SimpleCompiler sc = new SimpleCompiler();
;;     sc.cook("public class Arne{ public float doWork(){return 42.0f;}}");
;;     Class<?> arneClass = sc.getClassLoader().loadClass("Arne");

(comment

  (output-class counts)
  )


(def l 10000)

(defn gen-array []
  (double-array (take l (repeatedly rand))))

(defn gen-pair []
  (vec (take 2 (repeatedly gen-array))))

(def test-data (vec (take 100 (repeatedly gen-pair))))

(defn exercise [f n]

  ;;; Warm up
  (doseq [[X Y] (butlast test-data)]
    (f n X Y))

  (println "n =" n)
  (let [[X Y] (last test-data)
        r  (time (f n X Y))]
    (println "   Result=" r)))

(defn evaluator-for-instance [instance]
  (fn  [n X Y]
    (.eval instance n X Y)))

(def javac-cross-prod (evaluator-for-instance (CrossProdSum.)))
(def janino-cross-prod (evaluator-for-instance janino-cross-prod-class))

(defn perform-redundant-test [f]
  (doseq [i counts]
    (exercise f i)))

(comment

  ;; Perform the experiment for the JDK javac compiled class
  (println "------ USING JAVAC")
  (perform-redundant-test javac-cross-prod)
  (println "------ USING JANINO")
  (perform-redundant-test janino-cross-prod)

  
  )
