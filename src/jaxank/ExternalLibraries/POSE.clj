(ns jaxank.ExternalLibraries.POSE
  "A Library for managing Position Orientation & Scale within an easily Extendable clojure map structure"
  (:refer-clojure :exclude [* - + == / min max abs])
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]
            [ghostwheel.core :as g :refer [>defn >defn- >fdef => | <- ?]]

            [clojure.math :as coremath]
            [clojure.core.matrix :as mx]
            [clojure.core.matrix.operators :as mxop :refer [* - + == / min max]] ;overrides *, + etc. for matrices
            [clojure.math.numeric-tower :as math]

            [jaxank.ExternalLibraries.JaxExtension :as je]
            [jaxank.ExternalLibraries.StandardSpecs :as StdSpecs]

            ))

(declare UnitSphereGeneratorfn)
(def numspec ::StdSpecs/decent-number)
(def radspec ::StdSpecs/decent-rad)

;;"POS" = Position Orientation Scale
;;"POSE" = POS with Extendable data. This library only cares about the POS data and will modify it
;;"PAAM" = Position, Axis-Angle, & Mirrored? format

;;------ Specs --------------------------
;;This is the form of the affine transform used for 3D operations.
;;4x4 matrix setup to transform vectors of form [x y z 1]
(s/def ::AffineVector     (s/tuple numspec numspec numspec  #{1}))
(s/def ::TransformRow     (s/tuple numspec numspec numspec numspec))
(s/def ::TransformRowLast (s/tuple  #{0}    #{0}    #{0}    #{1}))
(s/def ::AffineTransform  (s/tuple ::TransformRow
                                   ::TransformRow
                                   ::TransformRow
                                   ::TransformRowLast))
(s/def ::POSE ::AffineTransform)
(s/def ::decent-number (s/and number? #(je/nor (NaN? %) (infinite? %))))
;;An Affine Transform represents the full POS data of an object in 3D space.
;; A transform is not the cleanest way for a user or even us programmers to think about the data
;;  therefore we need this library to convert between more convienient forms to the core easy operable affine transform structure

;;------ Spec Format: Point-UnitSphere-Mirrored ----------
;;  Note this is format used by UI of IronCAD
(s/def ::xyzVector    (s/tuple numspec numspec numspec))
(s/def ::rxryrzVector (s/tuple radspec radspec radspec))
(s/def ::Position ::xyzVector)
(s/def ::AVX (s/and numspec))
(s/def ::AVY (s/and numspec))
(s/def ::AVZ (s/and numspec))
(s/def ::Rot (s/and radspec))
(s/def ::UnitSphere (s/and #_{:clj-kondo/ignore [:syntax]}
                     (s/keys :req [::AVX ::AVY ::AVZ ::Rot] :gen UnitSphereGeneratorfn)
                           #(je/isValueEqual? 0.00001 1.0 (math/sqrt (+ (math/expt (::AVX %) 2) (math/expt (::AVY %) 2) (math/expt (::AVZ %) 2))))))
(s/def ::Mirrored boolean?)
(s/def ::POSE-point-unitSphereCart-mirrored (s/keys :req [::Position ::UnitSphere ::Mirrored]))

;;|------ Spec Format: Euler Rotation ------|
;;Add if and only if needed

;;------ Misc specs ----
(s/def ::CartesianPlane #{:XY :XZ :YZ})

;;------ Conversion Functions -----

(>defn PAAM->POSE [paam] [::POSE-point-unitSphereCart-mirrored => ::AffineTransform]
       (let [AX  (::AVX (::UnitSphere paam))
             AY  (::AVY (::UnitSphere paam))
             AZ  (::AVZ (::UnitSphere paam))
             Angle (::Rot (::UnitSphere paam))
             Position (::Position paam)]

         [[(+ (* (- 1 (coremath/cos Angle)) AX AX) (coremath/cos Angle))
           (- (* (- 1 (coremath/cos Angle)) AX AY) (* AZ (coremath/sin Angle)))
           (+ (* (- 1 (coremath/cos Angle)) AX AZ) (* AY (coremath/sin Angle)))
           (nth Position 0)]

          [(+ (* (- 1 (coremath/cos Angle)) AX AY) (* AZ (coremath/sin Angle)))
           (+ (* (- 1 (coremath/cos Angle)) AY AY) (coremath/cos Angle))
           (- (* (- 1 (coremath/cos Angle)) AY AZ) (* AX (coremath/sin Angle)))
           (nth Position 1)]

          [(- (* (- 1 (coremath/cos Angle)) AX AZ) (* AY (coremath/sin Angle)))
           (+ (* (- 1 (coremath/cos Angle)) AY AZ) (* AX (coremath/sin Angle)))
           (+ (* (- 1 (coremath/cos Angle)) AZ AZ) (coremath/cos Angle))
           (nth Position 2)]
          [0 0 0 1]]))

#_{:clj-kondo/ignore [:unresolved-symbol]}
(>defn POSE->PAAM [pose] [::AffineTransform => ::POSE-point-unitSphereCart-mirrored] ;singularity @ 0 and coremath/PI. Requires Pure Rotation Matrix (det = +1 & R' * R =1)
       (let [NormPose (select pose [0 1 2] :butlast)
             ClosetoZero 0.0001
             XX (/ (+ (abs (select NormPose 0 0)) 1) 2)
             YY (/ (+ (abs (select NormPose 1 1)) 1) 2)
             ZZ (/ (+ (abs (select NormPose 2 2)) 1) 2)
             XY (/ (+ (select NormPose 0 1) (select NormPose 1 0)) 4)
             XZ (/ (+ (select NormPose 0 2) (select NormPose 2 0)) 4)
             YZ (/ (+ (select NormPose 2 1) (select NormPose 1 2)) 4)

             Singularity    (cond (and (< (abs (- (select NormPose 1 0) (select NormPose 0 1))) ClosetoZero)
                                       (< (abs (- (select NormPose 2 0) (select NormPose 0 2))) ClosetoZero)
                                       (< (abs (- (select NormPose 1 2) (select NormPose 2 1))) ClosetoZero))  1
                                  :else  -1)

             AxisAngle (coremath/acos (/ (- (+ (select NormPose 0 0) (select NormPose 1 1) (select NormPose 2 2)) 1) 2)) ;AxisAngle (coremath/acos (/ (- (+ (abs (select NormPose 0 0)) (abs (select NormPose 1 1)) (abs (select NormPose 2 2))) 1) 2))
             
             AxisX (cond
                     (= Singularity 1)                              (cond
                                                                      (identity-matrix? NormPose)                     1
                                                                      (and (< XX ClosetoZero)  (> XX (max YY ZZ)))    0
                                                                      (and (>= XX ClosetoZero) (> XX (max YY ZZ)))    (coremath/sqrt XX)
                                                                      (and (< YY ClosetoZero)  (> YY (max XX ZZ)))    0.7071
                                                                      (and (>= YY ClosetoZero) (> YY (max XX ZZ)))    (/ XY (coremath/sqrt YY))
                                                                      (and (< ZZ ClosetoZero)  (> ZZ (max YY XX)))    0.7071
                                                                      (and (>= ZZ ClosetoZero) (> ZZ (max YY XX)))    (/ XZ (coremath/sqrt ZZ))
                                                                      (and (= coremath/PI AxisAngle) (some true? (map pos? (first NormPose)))) 1
                                                                      :else 0)

                     (> ClosetoZero (* 2 (coremath/sin AxisAngle))) 1 ;(= 0 (* 2 (coremath/sin AxisAngle)))
                     (not= 0 (* 2 (coremath/sin AxisAngle)))        (/ (- (select NormPose 2 1) (select NormPose 1 2)) (* 2 (coremath/sin AxisAngle))))

             AxisY (cond
                     (= Singularity 1)                              (cond
                                                                      (identity-matrix? NormPose)                     0
                                                                      (and (< XX ClosetoZero)  (> XX (max YY ZZ)))    0.7071
                                                                      (and (>= XX ClosetoZero) (> XX (max YY ZZ)))    (/ XY (coremath/sqrt XX))
                                                                      (and (< YY ClosetoZero)  (> YY (max XX ZZ)))    0
                                                                      (and (>= YY ClosetoZero) (> YY (max XX ZZ)))    (coremath/sqrt YY)
                                                                      (and (< ZZ ClosetoZero)  (> ZZ (max YY XX)))    0.7071
                                                                      (and (>= ZZ ClosetoZero) (> ZZ (max YY XX)))    (/ YZ (coremath/sqrt ZZ))
                                                                      (and (= coremath/PI AxisAngle) (some true? (map pos? (second NormPose)))) 1
                                                                      :else 0)

                     (> ClosetoZero (* 2 (coremath/sin AxisAngle))) 1 ;(= 0 (* 2 (coremath/sin AxisAngle)))
                     (not= 0 (* 2 (coremath/sin AxisAngle)))        (/ (- (select NormPose 0 2) (select NormPose 2 0)) (* 2 (coremath/sin AxisAngle))))

             AxisZ (cond
                     (= Singularity 1)                              (cond
                                                                      (identity-matrix? NormPose)                     0
                                                                      (and (< XX ClosetoZero)  (> XX (max YY ZZ)))    0.7071
                                                                      (and (>= XX ClosetoZero) (> XX (max YY ZZ)))    (/ XZ (coremath/sqrt XX))
                                                                      (and (< YY ClosetoZero)  (> YY (max XX ZZ)))    0.7071
                                                                      (and (>= YY ClosetoZero) (> YY (max XX ZZ)))    (/ YZ (coremath/sqrt YY))
                                                                      (and (< ZZ ClosetoZero)  (> ZZ (max YY XX)))    0
                                                                      (and (>= ZZ ClosetoZero) (> ZZ (max YY XX)))    (coremath/sqrt ZZ)
                                                                      (and (= coremath/PI AxisAngle) (some true? (map pos? (last NormPose)))) 1
                                                                      :else 0)

                     (> ClosetoZero (* 2 (coremath/sin AxisAngle))) 1 ;(= 0 (* 2 (coremath/sin AxisAngle)))
                     (not= 0 (* 2 (coremath/sin AxisAngle)))        (/ (- (select NormPose 1 0) (select NormPose 0 1)) (* 2 (coremath/sin AxisAngle))))

             Ismirror (neg? (det NormPose))]

         #:thrawn.Libraries.POSE{:Position [(select pose 0 3) (select pose 1 3) (select pose 2 3)]
                                  :UnitSphere
                                  #:thrawn.Libraries.POSE{:AVX AxisX,
                                                          :AVY AxisY,
                                                          :AVZ AxisZ,
                                                          :Rot AxisAngle}
                                  :Mirrored Ismirror}))

  (s/exercise ::POSE 1)

;;------ Operation Functions ------
(>defn POSE-Translate [pose [x y z]] [::AffineTransform ::xyzVector => ::AffineTransform]
      (mx/add pose [[0 0 0 x] [0 0 0 y] [0 0 0 z] [0 0 0 0]]))


(>defn POSE-Rotate [pose [rx ry rz]] [::AffineTransform ::rxryrzVector => ::AffineTransform] ;added spec to confirm rn is in Rad
       (let [RotationX [[1                      0                      0                      0]
                        [0                      (coremath/cos rx)      (coremath/sin (- rx))  0]
                        [0                      (coremath/sin rx)      (coremath/cos rx)      0]
                        [0                      0                      0                      1]]

             RotationY [[(coremath/cos ry)      0                      (coremath/sin ry)      0]
                        [0                      1                      0                      0]
                        [(coremath/sin (- ry))  0                      (coremath/cos ry)      0]
                        [0                      0                      0                      1]]

             RotationZ [[(coremath/cos rz)      (coremath/sin (- rz))  0                      0]
                        [(coremath/sin rz)      (coremath/cos rz)      0                      0]
                        [0                      0                      1                      0]
                        [0                      0                      0                      1]]]

       (mx/mmul pose RotationX RotationY RotationZ)))

(>defn POSE-MirrorAboutPlane [pose plane] [::POSE ::CartesianPlane => ::AffineTransform]
       (let [SomePlane
             (cond
              (= plane :Z)  [[1 0 0 0]  [0 1 0 0]  [0 0 -1 0] [0 0 0 1]]    ;Mirror in Z plane
              (= plane :Y)  [[1 0 0 0]  [0 -1 0 0] [0 0 1 0]  [0 0 0 1]]    ;Mirror in Y plane
              (= plane :X)  [[-1 0 0 0] [0 1 0 0]  [0 0 1 0]  [0 0 0 1]]    ;Mirror in X plane
              :else         [[1 0 0 0]  [0 1 0 0]  [0 0 1 0]  [0 0 0 1]]
              )]
       (mx/mmul pose SomePlane)))

;;Notes:
;;    -  Ignoring Rest of Affine Scale transforms until we have a reason to do otherwise...
;;    -  Ignoring Shearing operations because we have no desire to distort the object

;; |------ Matrix Operations ------|
#_{:clj-kondo/ignore [:unresolved-symbol]}
(>defn POSE-Invert [pose] [::AffineTransform => ::AffineTransform]  ;Checks if square matrix, change name?
       (identity-matrix? (round (mmul pose (inverse pose)))))

;;----- Test.check Generators -----
(defn- UnitSphereGenerator [[theta phi]]
  (let [x (* (coremath/sin theta) (coremath/cos phi))
        y (* (coremath/sin theta) (coremath/sin phi))
        z    (coremath/cos theta)
        rot (rand (* 2 coremath/PI))]
    {::AVX x  ::AVY y ::AVZ z ::Rot rot}))
(defn- UnitSphereGeneratorfn []
  (gen/fmap UnitSphereGenerator (gen/tuple (gen/double* {:min 0 :max (* 2 coremath/PI)})
                                           (gen/double* {:min 0 :max coremath/PI}))))


;; (comment ;Testing

;;   (map POSE-MirrorAboutPlane (map first (s/exercise ::POSE)) (s/exercise ::CartesianPlane))
;;   (map POSE-Rotate (map first (s/exercise ::AffineTransform 10)) (map first (s/exercise ::rxryrzVector 10)))
;;   (map POSE-Translate (map first (s/exercise ::AffineTransform 10)) (map first (s/exercise ::xyzVector 10)))

;;   (POSE->PAAM (first (first (s/exercise ::POSE 1))))


;;   (map PAAM->POSE (map first (s/exercise ::POSE-point-unitSphereCart-mirrored)))



;;   (def thistest (round (* [1 1 1 1] (transpose [[1.0 0.0 0.0 0.0]
;;                                                 [0.0 1.0 0.0 0.0]
;;                                                 [0.0 0.0 1.0 0.0]
;;                                                 [1.1070307160264413E-17 -0.11482226517256822 -9.817036914660073E-18 1.0]]))))
;;   (def basic [[1 0 0 20] [0.0 1 0 1] [0 0 1 1] [0.0 0.0 0.0 1.0]])
;;   (def basevec [(/ coremath/PI 2) 0 0])

;;   (round (POSE-Rotate basic basevec))
;;   (det thistest)

;;   )