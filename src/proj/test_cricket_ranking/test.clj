(ns org.karthik.matrix.test
   (:import (org.jblas DoubleMatrix Singular)))

(defn svds [s]
  (Singular/sparseSVD s)
  )

(defn svd [s]
  (Singular/fullSVD s))

(defn eye [n] (DoubleMatrix/eye n))

(defn zeros [m,n] (DoubleMatrix/zeros m n))

(let [r  (svd (eye 5))] (println (aget r 0))  (println (aget r 1)) (println (aget r 2)))

