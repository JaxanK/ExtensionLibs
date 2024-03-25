(ns jaxank.ExtensionLibs.StandardSpecs
  (:require [clojure.math :as math]
            [clojure.spec.alpha :as s]
            [jaxank.ExtensionLibs.JaxExtension :refer [nor unzipmap]]
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



(s/def ::ptvec2D (s/tuple ::x ::y))
(s/def ::ptvec3D (s/tuple ::x ::y ::z))
(s/def ::ptvec (s/or :2D ::ptvec2D  :3D ::ptvec3D))
(s/def ::ptmap (s/keys :req [::x ::y] :opt [::z]))
(s/def ::ptmap3D (s/keys :req [::x ::y ::z]))

(defn ptvec->ptmap "turns [_ _ _] into {:x _ :y _ :z _}  for 2D & 3D"  [ptvec]      (zipmap [:x :y :z] ptvec))
(defn ptmap->ptvec "turns {:x _ :y _ :z _} into [_ _ _]  for 2D & 3D"  [ptmap]  (case (count ptmap)
                                                                                  2 (unzipmap [:x :y]    ptmap)
                                                                                  3 (unzipmap [:x :y :z] ptmap)
                                                                                  nil))

(defn ptmap? "predicate for 2D & 3D points in map form" [ptmap] (and (map? ptmap) (contains? ptmap :x) (contains? ptmap :y) (every? number? (vals (select-keys ptmap [:x :y :z])))))
(defn ptvec2D? [pt] (s/valid? ::ptvec2D  pt))
(defn ptvec3D? [pt] (s/valid? ::ptvec3D  pt))



;|------ Line ------|
(s/def ::StartPt2D ::ptvec2D)
(s/def ::StartPt3D ::ptvec3D)
(s/def ::EndPt2D   ::ptvec2D)
(s/def ::EndPt3D   ::ptvec3D)
(s/def ::line2D (s/keys :req [::StartPt2D ::EndPt2D]))
(s/def ::line3D (s/keys :req [::StartPt3D ::EndPt3D]))


(s/def ::length (s/and number? pos?))
(s/def ::width  (s/and number? pos?))
(s/def ::height (s/and number? pos?))
(s/def ::points2D (s/coll-of ::ptvec2D))
(s/def ::points3D (s/coll-of ::ptvec3D))

;|------ 2D Shapes ------|;
(s/def ::Rectangle (s/keys :req [::length ::width]))
(s/def ::Polygon   (s/keys :req [::points2D]))

;|------ 3D Shapes ----|
(s/def ::Box (s/keys :req [::length ::width ::height]))

