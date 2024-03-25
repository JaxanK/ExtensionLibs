(ns jaxank.ExternalLibraries.StandardSpecs
  (:require [clojure.math :as math]
            [clojure.spec.alpha :as s]
            [jaxank.ExternalLibraries.JaxExtension :refer :all]
            ))


(s/def ::decent-number        (s/and number? #(nor (NaN? %) (infinite? %))))
(s/def ::decent-rad           (s/and number? #(nor (NaN? %) (infinite? %)) #(and (> % 0) (< % (* 2 math/PI)))))
(s/def ::non-negative         (s/and number? #(<= 0 %)))
(s/def ::non-negative-decent  (s/and number? #(<= 0 %) #(nor (NaN? %) (infinite? %))))

; ::pt spec will take either a vector [1 2 3] or a map {:X 1 :Y 2 :Z 3} and return it as the format of the map when conformed
(s/def ::X number?)
(s/def ::Y number?)
(s/def ::Z number?)
 
(defmulti pt-type type)
(defmethod pt-type clojure.lang.PersistentList [_]     (s/cat :X ::X :Y ::Y :Z ::Z))
(defmethod pt-type clojure.lang.PersistentVector [_]   (s/cat :X ::X :Y ::Y :Z ::Z))
(defmethod pt-type clojure.lang.PersistentArrayMap [_] (s/keys :req-un [::X ::Y ::Z]))
(s/def ::pt (s/multi-spec pt-type nil))


(defmulti pt-type2 type)
(defmethod pt-type2 clojure.lang.PersistentList [_]     (s/cat :X ::X :Y ::Y))
(defmethod pt-type2 clojure.lang.PersistentVector [_]   (s/cat :X ::X :Y ::Y))
(defmethod pt-type2 clojure.lang.PersistentArrayMap [_] (s/keys :req-un [::X ::Y]))
(s/def ::pt2 (s/multi-spec pt-type2 nil))

