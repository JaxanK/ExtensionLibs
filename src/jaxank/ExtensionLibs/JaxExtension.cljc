(ns jaxank.ExtensionLibs.JaxExtension "Jaxan's Language Extension for Fundamental Functions that are basically language level addons. Okay to use :refer :all"
    (:require [clojure.math :as math]
              [clojure.string :as str]
              #?@(:clj  [[clojure.spec.alpha :as s]]
                  :cljs [[cljs.spec.alpha :as s]
                         ;[cljs.edn :refer [read-string]]
                         ])))


;With help from https://stackoverflow.com/questions/27822101/working-with-long-let-expressions-in-clojure-repl
(defmacro deflet [bindings]
  (apply list `do (for [[s expr] (partition-all 2 bindings)]
                    (if (vector? s)
                      (apply list `do (for [i (range (count s))]
                                        `(def ~(nth s i) (nth ~expr ~i)))) ; need to fix so that an expression gets evaled into a vector if so... so type check for function?
                      `(def ~s ~expr)))))

#?(:clj (do
   (defmacro maplet "Unique names in bindings will be used to construct a map and return it. Remaining expressions will be evaluated as well if supplied" [bindings & expressions]
     (let [uniqueVarNames (into #{} (filter symbol? (take-nth 2 bindings)))
           returnMap (str "{" (reduce #(str %1 " " %2) (for [x uniqueVarNames] (str ":" (name x) " " (name x)))) "}")]

       `(let ~bindings ~(apply list `do expressions) ~(read-string returnMap))))
    ;;  (maplet [x 4
    ;;         y 7
    ;;         z 7
    ;;         [z w] [1 2]
   
    ;;         x 18] (def hello w))
     
   ))

 ;|========= Misc functions =======|
#?(:clj (defn generateUniqueID [] (str (java.util.UUID/randomUUID))))


(defn unzipmap "Complement to core.zipmap. Order of keys will be order of list or vector returned. Will return a vector or a list depending on type of 1st arg." [keysInOrder map]
  (let [col (for [key keysInOrder] (key map))]
    (if (vector? keysInOrder)
      (vec col)
      col)))

;|=========== Logic Functions ===========|
(defmacro xor [& body]
  `(and (or ~@body) (or (= 1 (count [~@body])) (not (and ~@body)))))
(defmacro nand [& body]
  `(not (and ~@body)))
(defmacro nor [& body]
  `(not (or ~@body)))
(defn isValueEqual? "Compare if two values are equal within a tolerance. Defaults to 0.001 if no tolerance supplied"
  ([value1 value2]
   (isValueEqual? 0.001 value1 value2))
  ([nearZero value1 value2]
   (> nearZero (abs (- value2 value1)))))
(defmacro ifnil [val default]
  `(let [temp# ~val] (if temp# temp# ~default)))

;|=========== String Functions ===========|
(defmacro includes? [str substr]
  `(and (string? ~str) (clojure.string/includes? ~str ~substr)))
(defmacro includesAny? [str & substrs]
  `(some true? (for [i# (flatten ~(vec substrs))] (includes? ~str i#))))
(defmacro includesAll? [str & substrs]
  `(every? true? (for [i# (flatten ~(vec substrs))] (includes? ~str i#))))

;|====== Predicates ======|
(defn atom? [thing]
  #?(:clj (instance? clojure.lang.Atom thing)
     :cljs (instance? cljs.core.Atom thing)))

;|=========== Printing Functions ==========|
(defmacro prnt "Runs prn but returns the value input instead of nil. Useful for checking values inline." [singleParam]
  `(do (prn ~singleParam) singleParam))
(defn printBigly "Really print something like you mean it" [text]
  (println)
  (println "----------------------------------------------------------------------")
  ;TODO write this to wrap text within the dashes
  (println text)
  (println "----------------------------------------------------------------------"))


;|========== Collection Functions =========|
(defn update-all-using-filter [filterFunction operationFunction fullList]
  (let [listOfType (filter filterFunction fullList)
        listNotOfType (remove filterFunction fullList)]
    (into listNotOfType (map operationFunction listOfType))))

#?(:clj (defn LVS->UUID-key-map "List Vector or Set gets converted to a new map with unique UUIDs used for each entry" [lvs Add-ID-to-This-Tag-If-Value-Is-Map]
          (->> lvs
               (map (fn [val] (let [newUUID (keyword (generateUniqueID))
                                    val (if-let [x Add-ID-to-This-Tag-If-Value-Is-Map]
                                          (assoc val x newUUID)
                                          val)]
                                (vector newUUID val))))
               (into {}))))

;ChatGPT
(defn find-closest "Find Closest Number in a vector/set/list of numbers" [numbers target]
  (let [numbers (vec numbers) ;allows sets or lists to be passed in
        distances (map #(Math/abs (- % target)) numbers)
        min-distance (apply min distances)
        index (->> distances
                   (map-indexed vector)
                   (filter #(= min-distance (second %)))
                   (map first)
                   first)]
    (get numbers index)))

;|========== Threading Functions ==========|
(def KillAllThreads false)
(defn runFunctionEveryMilliseconds [func ms]
  (eval `(do (def ContinueToRunThreadBool# true)
             (future (while (and ContinueToRunThreadBool# (not KillAllThreads))
                       (~func)
                       (Thread/sleep ~ms)))
             (fn [] (def ContinueToRunThreadBool# false)))))


;|==== Symbol convenience ===========|
(def != not=)
;(defmacro != [& rest] `(not= @rest))

;/======Passthroughs ===========/
(def PI math/PI)

;|====== Specs ======|
(defn exerciseOne [spec] (first (first (s/exercise spec 1))))


;|====== Error Handling ======|
(defmacro assert-throw "Custom Macro to check for a true value (like assert) but will cause a throw (Exception. message) instead of an assert so it can be caught by a try-catch statement. Will also pass value of x through."
  [x message]
  `(if-let [xresult#  ~x] xresult# (throw (Exception. ~message))))

;|====== Math Extensions ======|
;From Scad-clj library but I like them
(def pi Math/PI)
(def tau (* 2 pi))

(defn rad->deg [radians]
  (/ (* radians 180) pi))
(defn deg->rad [degrees]
  (* (/ degrees 180) pi))

(defn meters->inches [val]
  (/ (* val 1000) 25.4))
(defn inches->meters [val]
  (/ (* val 25.4) 1000))


;|======= Namespaces =====|
(defmacro ns-dsc "Namespace dot and slash corrector macro. Useful to refer to a deeper namespace from a root alias. \n
                   Keyword Input Example:    ::bb/ns1.ns2.ns3   will convert to   :*bb_path*.ns1.ns2/ns3 \n
                    Symbol Input Example:      bb/ns1.ns2.ns3   will convert to    *bb_path*.ns1.ns2/ns3"
  [nscorrect]

  (cond  (keyword? nscorrect) (let [x (str (namespace nscorrect) "." (str/replace (name nscorrect) "/" "."))
                                    lio (str/last-index-of x ".")
                                    rv (str (subs x 0 lio) "/" (subs x (+ 1 lio)))]
                                (keyword rv))
         (symbol? nscorrect)  #?(:clj (let [nscorrect2 (read-string (str "::" nscorrect))
                                            x (str (namespace nscorrect2) "." (str/replace (name nscorrect2) "/" "."))
                                            lio (str/last-index-of x ".")
                                            rv (str (subs x 0 lio) "/" (subs x (+ 1 lio)))]
                                        (symbol rv))
                                 :cljs (throw (js/Error. "Sorry, symbols not supported for this macro in clojurescript")) ;Note: This cljs error throw untested
                                 )))

#?(:clj (defn ns-dsc-fn "Namespace dot and slash corrector macro. Useful to refer to a deeper namespace from a root alias. \n
                   Keyword Input Example:    ::bb/ns1.ns2.ns3   will convert to   :*bb_path*.ns1.ns2/ns3 \n
                    Symbol Input Example:      bb/ns1.ns2.ns3   will convert to    *bb_path*.ns1.ns2/ns3
                 Can also pass in a string and (ns-dsc-fn (read-string str)) will be called
                 "
          [nscorrect]

          (cond  (string? nscorrect) (ns-dsc-fn (read-string nscorrect))
            (keyword? nscorrect) (let [x (str (namespace nscorrect) "." (str/replace (name nscorrect) "/" "."))
                                       lio (str/last-index-of x ".")
                                       rv (str (subs x 0 lio) "/" (subs x (+ 1 lio)))]
                                   (keyword rv))
            (symbol? nscorrect)  #?(:clj (let [nscorrect2 (read-string (str "::" nscorrect))
                                               x (str (namespace nscorrect2) "." (str/replace (name nscorrect2) "/" "."))
                                               lio (str/last-index-of x ".")
                                               rv (str (subs x 0 lio) "/" (subs x (+ 1 lio)))]
                                           (symbol rv))
                                    :cljs (throw (js/Error. "Sorry, symbols not supported for this macro in clojurescript")) ;Note: This cljs error throw untested
                                    ))))
