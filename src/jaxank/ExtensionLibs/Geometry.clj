(ns jaxank.ExtensionLibs.Geometry
  (:require [clojure.math :as m]
            [clojure.string :as str]
            [clojure.core.matrix :as mx]
            [jaxank.ExtensionLibs.JaxExtension :as je]
            [jaxank.ExtensionLibs.Specs :as ls]
            ))


#_{:clj-kondo/ignore [:unresolved-symbol]}
(comment
  ;This was the first version of the geometry code that I slapped together. I was also testing ideas of spec and had the wrong idea when I wrote this so it needs some improvement
  ;We have to be careful how many data types we define, but I think it makes sense to create a map for each of these items that way we can check it for compliance with a common set of functions

;TODO
;Switch to using basic vectors under matrix library
;Rewrite all functions to use matrix.core & new line type. Note there is also a distance function in matrix.core, so use it instead of calcs in linelength

;write specs for these...

  

;We will add planes and stuff soon too but we will have to think about how we want to define those, so leave for now
)


(defn line-length 
  ([line]          (mx/distance (:startPt line) (:endPt line)))
  ([startpt endpt] (mx/distance startpt endpt))
  )

(defn isOnLineSegment "returns testpoint if on segment, nil otherwise" [startpt endpt testpt]
  (if (je/isValueEqual? 0.001
                     (line-length startpt endpt) 
                     (+ (line-length startpt testpt) (line-length endpt testpt)))
    testpt 
    nil))


;TODO rewrite this in simpler form using core.matrix
(defn- intersHelper "Find the intersection of or shortest distance between two 3D lines" [line1Startpt line1Endpt line2Startpt line2Endpt]
  ;http://paulbourke.net/geometry/pointlineplane/
  ;http://paulbourke.net/geometry/pointlineplane/int2.lsp

  (let [nearzero 0.01
        P1  line1Startpt
        P2  line1Endpt
        P3  line2Startpt
        P4 line2Endpt

   ;; Strip xyz coordinates from lists P1, P2, P3 and P4, assign to variables
        X1 (first P1)    X2 (first P2)    X3 (first P3)    X4 (first P4)    ; x value
        Y1 (second P1)   Y2 (second P2)   Y3 (second P3)   Y4 (second P4)   ; y value
        Z1 (nth P1 2)    Z2 (nth P2 2)    Z3 (nth P3 2)    Z4 (nth P4 2)  ; z value


    ;; Calculate Relative coordinates of XYZ21, XYZ13 and XYZ43
        RelX21 (- X2 X1)  RelX43 (- X4 X3)  RelX13 (- X1 X3)        ; rx value
        RelY21 (- Y2 Y1)  RelY43 (- Y4 Y3)  RelY13 (- Y1 Y3)        ; ry value
        RelZ21 (- Z2 Z1)  RelZ43 (- Z4 Z3)  RelZ13 (- Z1 Z3)        ; rz value

    ;; Calculate the various dot products and denominator
        dot1343 (+ (* RelX13 RelX43) (* RelY13 RelY43) (* RelZ13 RelZ43))
        dot4321 (+ (* RelX43 RelX21) (* RelY43 RelY21) (* RelZ43 RelZ21))
        dot1321 (+ (* RelX13 RelX21) (* RelY13 RelY21) (* RelZ13 RelZ21))
        dot4343 (+ (* RelX43 RelX43) (* RelY43 RelY43) (* RelZ43 RelZ43))
        dot2121 (+ (* RelX21 RelX21) (* RelY21 RelY21) (* RelZ21 RelZ21))

        denom (- (* dot2121 dot4343) (* dot4321 dot4321))]

    (if (< (abs denom) nearzero)  ; are lines parallel?
      {:intersType "Lines Parallel and Equidistant, No intersection point exists"}

      (let [numer (- (* dot1343 dot4321) (* dot1321 dot4343))
              ;; u21 scale factor up line 1 to closest point to line21
              ;; if 0 > u21 < 1 closest point is within line section
              ;; if u21 < 0 closest point is beyond P1 end
              ;; or u21 > 1 closest point is beyond P2 end
            u21 (/ numer denom)

              ;; u43 is the scale factor up Line43 and works in the same way as u21
            u43 (/ (+ dot1343 (* dot4321 u21)) dot4343)

            X5 (+ X1 (* u21 RelX21))
            Y5 (+ Y1 (* u21 RelY21))
            Z5 (+ Z1 (* u21 RelZ21))

            X6 (+ X3 (* u43 RelX43))
            Y6 (+ Y3 (* u43 RelY43))
            Z6 (+ Z3 (* u43 RelZ43))

            intersStartPt [X5 Y5 Z5] ; note: I am kind of assuming that the 5 is on the first line and the 6 is on the second line
            intersEndPt   [X6 Y6 Z6]

              ; Calculate the distance between the points
            closedist (line-length (list X5 Y5 Z5) (list X6 Y6 Z6))]

        (if (< closedist nearzero)  ; are points nearly touching?
              ;; intersection point found
          {:intersType "point" :pt intersStartPt :Distance closedist :intersect? true :startPtOnsegment? (isOnLineSegment P1 P2 intersStartPt)  :endPtOnsegment? (isOnLineSegment P3 P4 intersStartPt) :onBothSegments? (and (isOnLineSegment P1 P2 intersStartPt) (isOnLineSegment P3 P4 intersEndPt))}

              ;; No intersection point found, shortest line will be given for closest distance between two lines
          {:intersType "line" :StartPt intersStartPt :EndPt intersEndPt :Distance closedist :intersect? false :startPtOnsegment? (isOnLineSegment P1 P2 intersStartPt)  :endPtOnsegment? (isOnLineSegment P3 P4 intersEndPt) :onBothSegments? (and (isOnLineSegment P1 P2 intersStartPt) (isOnLineSegment P3 P4 intersEndPt))})))))

(defn inters "Find the intersection of two lines v2" [line1Startpt line1Endpt line2Startpt line2Endpt MustBeOnSegments?]
  (let [result (intersHelper line1Startpt line1Endpt line2Startpt line2Endpt)]
    (cond
      (not= "POINT" (str/upper-case (:intersType result))) nil
      (not MustBeOnSegments?) (:pt result)
      (boolean (:onBothSegments? result)) (:pt result)
      :else nil)))

(defn GetMinX [& PtList]
  (apply clojure.core/min (map #(apply clojure.core/min (:X %)) PtList)))

(defn angle [startPt endPt]
  (cond
    ;2D point
    (and (ls/ptvec2D? startPt) (ls/ptvec2D? endPt))
    (let [[x y] (into [] (map - endPt startPt))]
      {:theta (m/atan (/ y x)) :phi (/ m/PI 2)})
    ;3D point
    (and (ls/ptvec3D? startPt) (ls/ptvec3D? endPt))

    (let [[xs ys zs]  startPt
          [xe ye ze]  endPt
          thetanumer (- xe xs)
          thetadenom (m/sqrt (+ (m/pow (- xe xs) 2.0) (m/pow (- ye ys) 2.0)))
          phinumer   (m/sqrt (+ (m/pow (- xe xs) 2.0) (m/pow (- ye ys) 2.0)))
          phidenom (- ze zs)]
      ;(if (or (zero? thetadenom) (zero? phidenom)) (println "DivideZeroISsue: " startPt endPt))
      {:theta (/ thetanumer thetadenom)
       :phi   (/   phinumer   phidenom)})))


(defn inRange [lowerVal higherval testval]
  (and (< lowerVal testval) (> higherval testval) testval))

(defn ptoffset [pt1 pt2]
  (into [] (map - pt2 pt1)))

(defn pointTranslate "Translates a point, if provided a theta value, then provided X & Y Values will be applied when coord system is rotated by theta"
  ([point x y z]
   (pointTranslate point x y z 0))
  ([point x y z theta]
   (let [thetaX theta
         thetaY (+ (/ m/PI 2.0) theta)
         xpart (- (* x (m/cos theta)) (* y (m/sin theta)))
         ypart (+ (* x (m/sin theta)) (* y (m/cos theta)))]
     [(+ xpart  (nth point 0))
      (+ ypart  (nth point 1))
      (+ 0.0 z (nth point 2))])))

(defn round-to "number is number, and n should be a multiple of 10 to specify number of decimal places"[num n]
  (/ (m/round (* (double n) num)) (double n)))
(defn point-round-to [pt n]
  (mapv #(round-to % n) pt))

