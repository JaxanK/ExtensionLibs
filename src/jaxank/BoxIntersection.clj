(ns jaxank.BoxIntersection
  #_{:clj-kondo/ignore [:unused-namespace]}
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test.check.generators :as gen]

   [clojure.math :as coremath]
   [clojure.core.matrix :as mx]
   [clojure.core.matrix.operators :as mxop]
   [clojure.math.numeric-tower :as math]

   ;[jaxank.JaxExtension :as je]
   [jaxank.StandardSpecs :as StdSpecs]
   [jaxank.POSE :as PL]))

(defn Coordinates [pose [SBL SBW SBH]]
  (let [Cord {:O   [0     0     0]
              :X   [SBL   0     0]
              :Y   [0     SBW   0]
              :Z   [0     0     SBH]
              :XY  [SBL   SBW   0]
              :XZ  [SBL   0     SBH]
              :YZ  [0     SBW   SBH]
              :XYZ [SBL   SBW   SBH]}

        RotM    (mx/select pose [0 1 2] :butlast)
        TransM  (mx/select pose [0 1 2] :last)

        RotateCord (map #(mx/mmul RotM (val %)) Cord)
        TranslateCord (map #(mx/add TransM %) RotateCord)
        NewCord (zipmap [:O :X :Y :Z :XY :XZ :YZ :XYZ] TranslateCord)]
    NewCord))

(defn Coordinates->POSE [Cord] ;Doesnt work for rotated objects!!!
  (let [X (first (:O Cord))
        Y (second (:O Cord))
        Z (last (:O Cord))
        SBL (mx/magnitude (mx/sub (:X Cord) (:O Cord)))
        SBW (mx/magnitude (mx/sub (:Y Cord) (:O Cord)))
        SBH (mx/magnitude (mx/sub (:Z Cord) (:O Cord)))]
    {:POSE [[1 0 0 X] [0 1 0 Y] [0 0 1 Z] [0 0 0 1]] :SB [SBL SBW SBH]}))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(defn AllAxis [CordA CordB]
  (let [NAX (mx/sub (:X CordA) (:O CordA))
        NAY (mx/sub (:Y CordA) (:O CordA))
        NAZ (mx/sub (:Z CordA) (:O CordA))
        NBX (mx/sub (:X CordB) (:O CordB))
        NBY (mx/sub (:Y CordB) (:O CordB))
        NBZ (mx/sub (:Z CordB) (:O CordB))]
    {:NAX    (mx/normalise NAX)
     :NAY    (mx/normalise NAY)
     :NAZ    (mx/normalise NAZ)
     :NBX    (mx/normalise NBX)
     :NBY    (mx/normalise NBY)
     :NBZ    (mx/normalise NBZ)
     :CAX_BX (mx/normalise (mx/abs (mx/cross NAX NBX)))
     :CAX_BY (mx/normalise (mx/abs (mx/cross NAX NBY)))
     :CAX_BZ (mx/normalise (mx/abs (mx/cross NAX NBZ)))
     :CAY_BX (mx/normalise (mx/abs (mx/cross NAY NBX)))
     :CAY_BY (mx/normalise (mx/abs (mx/cross NAY NBY)))
     :CAY_BZ (mx/normalise (mx/abs (mx/cross NAY NBZ)))
     :CAZ_BX (mx/normalise (mx/abs (mx/cross NAZ NBX)))
     :CAZ_BY (mx/normalise (mx/abs (mx/cross NAZ NBY)))
     :CAZ_BZ (mx/normalise (mx/abs (mx/cross NAZ NBZ)))}))

(defn separated [cordA cordB axis]
  (let
   [projA (into [] (map #(mx/dot % axis) (vals cordA)))
    projB (into [] (map #(mx/dot % axis) (vals cordB)))
    SpanLength (-    (mx/emax (mx/join projA projB))     (mx/emin (mx/join projA projB)))
    SpanSum    (+ (- (mx/emax projA) (mx/emin projA)) (- (mx/emax projB) (mx/emin projB)))]
    (cond
      (mx/equals axis [0 0 0])   false
      (> SpanLength SpanSum) true
      :else false)))

(defn notouchseparated [cordA cordB axis]
  (let
   [projA (into [] (map #(mx/dot % axis) (vals cordA)))
    projB (into [] (map #(mx/dot % axis) (vals cordB)))
    SpanLength (- (mx/emax (mx/join projA projB)) (mx/emin (mx/join projA projB)))
    SpanSum    (+ (- (mx/emax projA) (mx/emin projA)) (- (mx/emax projB) (mx/emin projB)))]
    (cond
      (mx/equals axis [0 0 0])   false
      (>= SpanLength SpanSum) true
      :else false)))


(defn MinMaxIntCords [cord]
  (let
   [Xbound [(mx/emin (map first  (vals cord))) (mx/emax (map first  (vals cord)))]
    Ybound [(mx/emin (map second (vals cord))) (mx/emax (map second (vals cord)))]
    Zbound [(mx/emin (map last   (vals cord))) (mx/emax (map last   (vals cord)))]]
    [Xbound Ybound Zbound]))


(defn Intersection [pose1 [SBL1 SBW1 SBH1] pose2 [SBL2 SBW2 SBH2]]
  (let [CordA   (Coordinates pose1 [SBL1 SBW1 SBH1])
        CordB   (Coordinates pose2 [SBL2 SBW2 SBH2])
        AllAxisAB (AllAxis CordA CordB)]
    (cond
      (separated CordA CordB (:NAX    AllAxisAB)) false
      (separated CordA CordB (:NAY    AllAxisAB)) false
      (separated CordA CordB (:NAZ    AllAxisAB)) false
      (separated CordA CordB (:NBX    AllAxisAB)) false
      (separated CordA CordB (:NBY    AllAxisAB)) false
      (separated CordA CordB (:NBZ    AllAxisAB)) false
      (separated CordA CordB (:CAX_BX AllAxisAB)) false
      (separated CordA CordB (:CAX_BY AllAxisAB)) false
      (separated CordA CordB (:CAX_BZ AllAxisAB)) false
      (separated CordA CordB (:CAY_BX AllAxisAB)) false
      (separated CordA CordB (:CAY_BY AllAxisAB)) false
      (separated CordA CordB (:CAY_BZ AllAxisAB)) false
      (separated CordA CordB (:CAZ_BX AllAxisAB)) false
      (separated CordA CordB (:CAZ_BY AllAxisAB)) false
      (separated CordA CordB (:CAZ_BZ AllAxisAB)) false
      :else (let [Cord1 (MinMaxIntCords CordA)
                  Cord2 (MinMaxIntCords CordB)
                  Xmin  (max (mx/emin (first Cord1))  (mx/emin (first Cord2)))
                  Ymin  (max (mx/emin (second Cord1)) (mx/emin (second Cord2)))
                  Zmin  (max (mx/emin (last Cord1))   (mx/emin (last Cord2)))
                  Xmax  (min (mx/emax (first Cord1))  (mx/emax (first Cord2)))
                  Ymax  (min (mx/emax (second Cord1)) (mx/emax (second Cord2)))
                  Zmax  (min (mx/emax (last Cord1))   (mx/emax (last Cord2)))]

              (Coordinates->POSE   {:O   [Xmin Ymin Zmin]  ;Doesnt work for rotated objects!!!
                                    :X   [Xmax Ymin Zmin]
                                    :Y   [Xmin Ymax Zmin]
                                    :Z   [Xmin Ymin Zmax]
                                    :XY  [Xmax Ymax Zmin]
                                    :XZ  [Xmax Ymin Zmax]
                                    :YZ  [Xmin Ymax Zmax]
                                    :XYZ [Xmax Ymax Zmax]}))))) 

(defn NoTouchIntersection [pose1 [SBL1 SBW1 SBH1] pose2 [SBL2 SBW2 SBH2]]
  (let [CordA   (Coordinates pose1 [SBL1 SBW1 SBH1])
        CordB   (Coordinates pose2 [SBL2 SBW2 SBH2])
        AllAxisAB (AllAxis CordA CordB)]
    (cond
      (notouchseparated CordA CordB (:NAX    AllAxisAB)) false
      (notouchseparated CordA CordB (:NAY    AllAxisAB)) false
      (notouchseparated CordA CordB (:NAZ    AllAxisAB)) false
      (notouchseparated CordA CordB (:NBX    AllAxisAB)) false
      (notouchseparated CordA CordB (:NBY    AllAxisAB)) false
      (notouchseparated CordA CordB (:NBZ    AllAxisAB)) false
      (notouchseparated CordA CordB (:CAX_BX AllAxisAB)) false
      (notouchseparated CordA CordB (:CAX_BY AllAxisAB)) false
      (notouchseparated CordA CordB (:CAX_BZ AllAxisAB)) false
      (notouchseparated CordA CordB (:CAY_BX AllAxisAB)) false
      (notouchseparated CordA CordB (:CAY_BY AllAxisAB)) false
      (notouchseparated CordA CordB (:CAY_BZ AllAxisAB)) false
      (notouchseparated CordA CordB (:CAZ_BX AllAxisAB)) false
      (notouchseparated CordA CordB (:CAZ_BY AllAxisAB)) false
      (notouchseparated CordA CordB (:CAZ_BZ AllAxisAB)) false
      :else (let [Cord1 (MinMaxIntCords CordA)
                  Cord2 (MinMaxIntCords CordB)
                  Xmin  (max (mx/emin (first  Cord1)) (mx/emin (first Cord2)))
                  Ymin  (max (mx/emin (second Cord1)) (mx/emin (second Cord2)))
                  Zmin  (max (mx/emin (last   Cord1)) (mx/emin (last Cord2)))
                  Xmax  (min (mx/emax (first  Cord1)) (mx/emax (first Cord2)))
                  Ymax  (min (mx/emax (second Cord1)) (mx/emax (second Cord2)))
                  Zmax  (min (mx/emax (last   Cord1)) (mx/emax (last Cord2)))]

              (Coordinates->POSE   {:O   [Xmin Ymin Zmin]  ;Doesnt work for rotated objects!!!
                                    :X   [Xmax Ymin Zmin]
                                    :Y   [Xmin Ymax Zmin]
                                    :Z   [Xmin Ymin Zmax]
                                    :XY  [Xmax Ymax Zmin]
                                    :XZ  [Xmax Ymin Zmax]
                                    :YZ  [Xmin Ymax Zmax]
                                    :XYZ [Xmax Ymax Zmax]})))))

(comment ;Testing
  (defn correctshape [shape] (mx/mset (* (mx/transpose [1 1 1 39.3701]) (mx/transpose shape)) 3 3 1))

  (def shape1 [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [0.0 0.0 0 1.0]])

  (def shape2 [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [0.508 0 0.2031999999999998 1.0]])

  (def ShapeA (correctshape shape1))
  (def ShapeB (correctshape shape2))

  (def thishere (Intersection ShapeA [300.0 2.125 100] ShapeB [30 2.125 50]))

  (PL/POSE->PAAM (:POSE thishere))


  (Intersection [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [2.54 8.881784197001252E-16 2.0320000000000005 1.0]] 
                [30.0 2.125 40.0] 
                [[1.0 0.0 0.0 0.0] [0.0 1.0 0.0 0.0] [0.0 0.0 1.0 0.0] [2.54 8.881784197001252E-16 2.0320000000000005 1.0]] 
                [30.0 2.125 40.0])
  )
;; ;normal Axis Cases
;;   (separated Cord1 Cord2 (:NAX AllAxis12))
;;   (separated Cord1 Cord2 (:NAY AllAxis12))
;;   (separated Cord1 Cord2 (:NAZ AllAxis12))
;;   (separated Cord1 Cord2 (:NBX AllAxis12))
;;   (separated Cord1 Cord2 (:NBY AllAxis12))
;;   (separated Cord1 Cord2 (:NBZ AllAxis12))
;; ;Cross Product Cases
;;   (separated Cord1 Cord2 (:CAX_BX AllAxis12))
;;   (separated Cord1 Cord2 (:CAX_BY AllAxis12))
;;   (separated Cord1 Cord2 (:CAX_BZ AllAxis12))

;;   (separated Cord1 Cord2 (:CAY_BX AllAxis12))
;;   (separated Cord1 Cord2 (:CAY_BY AllAxis12))
;;   (separated Cord1 Cord2 (:CAY_BZ AllAxis12))

;;   (separated Cord1 Cord2 (:CAZ_BX AllAxis12))
;;   (separated Cord1 Cord2 (:CAZ_BY AllAxis12))
;;   (separated Cord1 Cord2 (:CAZ_BZ AllAxis12))

;;   (Intersection (correctshape shape3) [100 1.6 100] (correctshape shape4) [2 1.5 100])
;;   (Intersection ShapeA [1 3 3] ShapeB [2 1 3])
;;