(ns dk.debugcore
  (:use seesaw.core
        dk.herringbone)
	(:require [hiphip.double :as hiphip]
            [hiphip.array :as harray])
  (:import [squidpony.squidcolor SColor SColorFactory]
           [squidpony.squidgrid.gui SGPane]
           [squidpony.squidgrid.gui.awt.event SGKeyListener SGKeyListener$CaptureType ]
           [squidpony.squidgrid.fov TranslucenceWrapperFOV BasicRadiusStrategy]
           [squidpony.squidgrid.gui.swing SwingPane]
           [java.awt Font Component Point]
           [java.awt.event KeyListener KeyEvent]
           [javax.swing JOptionPane JSpinner]
           [java.io File])
  (:gen-class))
(set! *warn-on-reflection* true)
(native!)
(def wide 62)
(def high 62)
(def ^Long iw (- wide 2)) ;inner width
(def ^Long ih (- high 2)) ;inner height

(def wall 9999.0)
(def floor 2000.0)
(def ^Double GOAL 0.0)

(def cleared-levels (atom {}))
(def dlevel (atom 0))
(defn ^"[Z" init-full-seen [] (let [ ^"[Z" res1d (make-array Boolean/TYPE (* wide high))]
                   (doseq [i (range (* wide high))]
                     (aset res1d i false))
                     res1d))

(def player (atom {:pos 0 :show \@ :hp 200 :vision 12 :dijkstra nil :seen nil :full-seen (init-full-seen)}))
(defn make-player-label [] (label :text (str "AdventureMan. HP " (:hp @player)) :size [150 :by 16]))
(defn make-welcome-message [] (label :text "Welcome to Ruins in... Something!":size [690 :by 16]))
(defn make-other-message [script] (label :text script :size [690 :by 16]))
(def monsters (atom (vec (for [i (range 1 16)] (atom {:pos 0 :show \M :hp 11 :vision 12 :dijkstra nil :ident (first (clojure.string/lower-case (Integer/toString i 16)))})))))
(def ^TranslucenceWrapperFOV fov (TranslucenceWrapperFOV. ))

(defn visible-monsters [] (filter (complement nil?) (for [mon @monsters] (if (> (aget (:seen @player) (mod (:pos @mon) wide) (quot (:pos @mon) wide)) 0)
                                                (label :text (str "<" (:ident @mon) "> Monster. HP " (:hp @mon)))
                                                nil))))

(def f (frame :title "Ruins in Roswell" :on-close :exit :size [777 :by 700]))

(defn display [^Component content]
  (config! f :content content)
  (. content setVisible true)
  content)

(defn acquire [kw] (select (to-root f) kw))

(def sp (frame :title "Select Flee Multiplier" :id :flee :on-close :hide :visible? false))

(defn spin []
  (let [content ^JSpinner (spinner :id :flee-spinner
;           :listen [:focus-gained (fn [s] (println (.requestFocusInWindow (acquire [:#messages]))))]
           :model (spinner-model -1.5 :from -5.0 :to -0.8 :by 0.05))]
  (config! sp :content content)
  (. content setVisible true)
    (-> sp pack! show!)))

(defn acquire-sp [kw] (select (to-root sp) kw))
(defn ^SGPane pane [] (SwingPane. wide high (try (let [fnt (File. "zodiac_square.ttf")]
                                                   (.deriveFont (Font/createFont Font/TRUETYPE_FONT fnt) 8.0))
                                              (catch Exception e (Font. Font/MONOSPACED Font/PLAIN 14.0)))))

(defn stats-pane [] (border-panel :south
                                  (vertical-panel :id :entities :items (concat [(make-player-label)]
                                                                 (visible-monsters))
                                                                 :size [150 :by 600])
                                  ;:north (vertical-panel :items [(button
                                  ;                                  :text "Show Flee Mult."
                                  ;                                  :listen [:action (fn [eh] (show! sp)
                                  ;                                                     ;       (.nextFocus (acquire [:#flee]))
                                  ;                                                     )])
                                  ;                                 (button
                                  ;                                  :text "Hide Flee Mult."
                                  ;                                  :listen [:action (fn [eh] (hide! sp))])
                                  ;                                 ])
                                  ))
(defn messages-pane [] (scrollable
                        (vertical-panel :id :messages :items [(make-welcome-message)])
                        :vscroll :always :size [600 :by 70]))
(defn fib-seq []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn ^doubles make-bones []
  (let [hvec (map #(map (fn [s] (vec s)) %) horiz)
        vvec (map #(map (fn [s] (vec s)) %) vert)
        oh (+ 20 ih)
        ow (+ 20 iw)
        seeds (drop 2 (take (quot (* oh ow) 160) (fib-seq)))
        initial (hiphip/amake [i (* ow oh)] wall)
        shown (char-array (* ow oh) \#)]
    (loop [next-fill 0 started-indent 0]
      (if (>= (+ (* 10 ow ) next-fill) (* ow oh))
          initial
          (let [hofull (nth hvec (mod (nth seeds (mod next-fill (count seeds))) (count hvec)))
                    ho (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull))]
            (when (< (+ (* 10 ow) 20 next-fill) (* ow oh))
                (doseq [nf (range 10)]
                                         (hiphip/afill! [[i eh] initial :range [(+ (* ow nf) next-fill) (+ 20 (* ow nf) next-fill)]]
                                                                  (do (nth (nth ho nf) (- i (* ow nf) next-fill))))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* ow nf) next-fill) (+ 20 (* ow nf) next-fill)]]
                                                                  (nth (nth hofull nf) (- i (* ow nf) next-fill)))))
                (recur
                 (long
                   (if (< (mod (+ 40 next-fill) ow) (mod next-fill ow))
                     (condp = started-indent
                       0 (+ (* ow 10) 10 (- next-fill (mod next-fill ow )))
                       1 (+ (* ow 10) 20 (- next-fill (mod next-fill ow )))
                       2 (+ (* ow 10) 30 (- next-fill (mod next-fill ow )))
                       3 (+ (* ow 10)  0 (- next-fill (mod next-fill ow )))
                       )
                     (+ 40 next-fill) ) )
                 (long (if (< (mod (+ 40 next-fill) ow) (mod next-fill ow))
                   (mod (inc started-indent) 4)
                   started-indent))
                 ))))
    (loop [next-fill (* 10 ow) started-indent 1]
      (if (>= (+ 10 (mod next-fill ow)) ow)
          initial
        (let [vefull (nth vvec (mod (nth seeds (mod next-fill (count seeds))) (count vvec)))
                    ve (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull))]
                (when (< (+ (* 19 ow) 10 next-fill) (* ow oh))
                  (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+ (* ow nf) next-fill) (+ 10 (* ow nf) next-fill)]]
                                                                  (nth (nth ve nf) (- i (* ow nf) next-fill)))
                      (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* ow nf) next-fill) (+ 10 (* ow nf) next-fill)]]
                                                                  (nth (nth vefull nf) (- i (* ow nf) next-fill)))))
                (recur
                 (long
                   (if (< (mod (+ 40 (quot next-fill ow)) oh) (quot next-fill ow))
                     (condp = started-indent
                       0 (+ (* ow 10) 10 (mod next-fill ow))
                       1 (+ (* ow 20) 10 (mod next-fill ow))
                       2 (+ (* ow 30) 10 (mod next-fill ow))
                       3 (+           10 (mod next-fill ow))
                       )
                       (+ (* 40 ow) next-fill) ) )
                 (long (if (< (mod (+ 40 (quot next-fill ow)) oh) (quot next-fill ow))
                   (mod (inc started-indent) 4)
                   started-indent))
                 ))))
    ;(doall (map #(println (apply str %)) (partition ow (vec shown))))
    [(hiphip/amake [i (* wide high)] (if (or
			(= (mod i wide) 0)
			(= (mod i wide) (dec wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 wall
		 (hiphip/aget initial (+ (* 10 ow) -10 (* 20 (quot i wide)) (- i (dec wide) (* 2 (quot i wide)))))))

     (harray/amake Character/TYPE [i (* wide high)] (if (or
			(= (mod i wide) 0)
			(= (mod i wide) (dec wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 \#
		 (aget shown (+ (* 10 ow) -10 (* 20 (quot i wide)) (- i (dec wide) (* 2 (quot i wide)))))))
                                       ]))

(defn ^doubles make-bones-original []
  (let [seed (rand-int (count horiz))
        initial (horiz seed)
        hvec (map #(map (fn [s] (vec s)) %) horiz)
        vvec (map #(map (fn [s] (vec s)) %) vert)
        initial (hiphip/amake [i (* iw ih)] wall)
        shown (char-array (* iw ih) \#)]
    (loop [horiz true starting-horiz true next-fill 0]
      (if (>= (+ (* 20 iw ) next-fill) (* iw ih))
          initial
          (if horiz
              (let [hofull (rand-nth hvec)
                    ho (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull))]
                (doseq [nf (range 10)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth ho nf) (- i (* iw nf) next-fill)))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth hofull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                   (not starting-horiz)
                   (not horiz))
                 (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                   (not starting-horiz)
                   starting-horiz)
                 (long (if starting-horiz
                   (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                     (+ (* iw 10) (- next-fill (mod next-fill iw )))
                     (+ 30 next-fill) )
                   (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                       (+ (* iw 30) (- next-fill (mod next-fill iw )))
                       (+ 30 next-fill) )))))
              (let [vefull (rand-nth vvec)
                    ve (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull))]
                (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth ve nf) (- i (* iw nf) next-fill)))
                  (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth vefull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                   (not starting-horiz)
                   (not horiz))
                 (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                   (not starting-horiz)
                   starting-horiz)
                 (long (if starting-horiz
                   (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                     (+ (* iw 10) (- next-fill (mod next-fill iw )))
                     (+ 10 next-fill) )
                   (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                       (+ (* iw 30) (- next-fill (mod next-fill iw )))
                       (+ 10 next-fill) )))))
        )))
    (loop [ horiz false next-fill (+ (* iw 20) 10)]
      (if (>= (+ (* 20 iw ) next-fill) (* iw ih))
          initial
          (if horiz
              (let [hofull (rand-nth hvec)
                    ho (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull))]
                (doseq [nf (range 10)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth ho nf) (- i (* iw nf) next-fill)))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth hofull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                   false
                   (not horiz))
                   (long (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                       (+ 10 (* iw 40) (- next-fill (mod next-fill iw )))
                       (+ 30 next-fill) ))))
              (let [vefull (rand-nth vvec)
                    ve (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull))]
                (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth ve nf) (- i (* iw nf) next-fill)))
                  (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth vefull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                   false
                   (not horiz))
                   (long (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                     (+ 10 (* iw 40) (- next-fill (mod next-fill iw )))
                     (+ 10 next-fill) ))))
        )))
    (loop [ horiz false next-fill (+ (* iw 30) 20)]
      (if (>= (+ (* 20 iw ) next-fill) (* iw ih))
          initial
          (if horiz
              (let [hofull (rand-nth hvec)
                    ho (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) hofull))]
                (doseq [nf (range 10)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth ho nf) (- i (* iw nf) next-fill)))
                                       (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 20 (* iw nf) next-fill)]]
                                                                  (nth (nth hofull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                   false
                   (not horiz))
                   (if (< (mod (+ 30 next-fill) iw) (mod next-fill iw))
                       (+ 20 (* iw 40) (- next-fill (mod next-fill iw )))
                       (+ 30 next-fill) )))
              (let [vefull (rand-nth vvec)
                    ve (vec (map #(replace {\# wall \. floor \$ floor \~ floor \% floor \+ floor} %) vefull))]
                (doseq [nf (range 20)] (hiphip/afill! [[i eh] initial :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth ve nf) (- i (* iw nf) next-fill)))
                  (harray/afill! Character/TYPE [[i eh] shown :range [(+ (* iw nf) next-fill) (+ 10 (* iw nf) next-fill)]]
                                                                  (nth (nth vefull nf) (- i (* iw nf) next-fill))))
                (recur
                 (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                   false
                   (not horiz))
                   (if (< (mod (+ 10 next-fill) iw) (mod next-fill iw))
                     (+ 20 (* iw 40) (- next-fill (mod next-fill iw )))
                     (+ 10 next-fill) )))
        )))
    [(hiphip/amake [i (* wide high)] (if (or
			(= (mod i wide) 0)
			(= (mod i wide) (dec wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 wall
		 (hiphip/aget initial (- i (dec wide) (* 2 (quot i wide))))))

     (harray/amake Character/TYPE [i (* wide high)] (if (or
			(= (mod i wide) 0)
			(= (mod i wide) (dec wide))
			(< i wide)
			(> i (- (* wide high) wide)))
		 \#
		 (aget shown (- i (dec wide) (* 2 (quot i wide))))))
                                       ]))

(defn ^"[[F" dungeon-resistances [^doubles dungeon]
                 (let [res2d (make-array Float/TYPE wide high)]
                   (doseq [x (range wide) y (range high)]
                     (aset res2d x y (if
                                       (= (hiphip/aget dungeon (+ x (* wide y))) wall)
                                       (float 1.0)
                                       (float 0.0))))
                   res2d))

(defn run-fov-player
  [entity dungeon]
    (let [^"[[F" calculated (. fov calculateFOV (:res @dungeon) (mod (:pos @entity) wide) (quot (:pos @entity) wide) 1.0 (/ 1.0 (:vision @entity)) BasicRadiusStrategy/DIAMOND)]
      (doseq [ idx (range (* wide high))]
         (aset ^"[Z" (:full-seen @entity) ^Integer idx
            (boolean (or (aget ^"[Z" (:full-seen @entity) idx)
                (if (> (aget ^"[[F" calculated (mod idx wide) (quot idx wide)) 0)
                  true
                  false)

                   ))))
      calculated)
  )

(defn run-fov
  [entity dd]
    (let [^"[[F" calculated (. fov calculateFOV (:res @dd) (mod (:pos @entity) wide) (quot (:pos @entity) wide) 1.0 (/ 1.0 (:vision @entity)) BasicRadiusStrategy/DIAMOND)]
      calculated)
  )

(defn init-dungeon ([dngn] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (* iw ih))] (if (= (hiphip/aget dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc GOAL) (inc ctr))) (recur ctr))))))
  ([dngn entity] (loop [ctr 0] (if (>= ctr 1) dngn (let [rand-loc (rand-int (* iw ih))] (if (and
                                                                                    (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @monsters))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget dngn rand-loc) floor))
			                                                        (recur (do
                                                                       (swap! entity assoc :pos rand-loc)
                                                                       ;(when (= \# (aget ^chars shown (:pos @entity)))
                                                                       ;   (println "Monster intersecting with wall"))

                                                                       (inc ctr)))
                                                              (recur ctr))))))
  ([dngn entity starting-cell] (loop [ctr 0] (if (>= ctr 1) dngn (let [rand-loc (rand-nth (keep-indexed #(if (= %2 starting-cell) %1) (vec dngn)))]
                                                                   (if (and (apply distinct? (concat (filter (complement nil?)
                                                                                                             (map (fn [atm] (if (= (:pos @atm) 0) nil (:pos @atm))) @monsters))
                                                                                                        [rand-loc (:pos @player)]))
                                                                                                 (= (hiphip/aget dngn rand-loc) starting-cell))
			                                                        (recur (do (swap! entity assoc :pos rand-loc) (inc ctr))) (recur ctr)))))))

(defn alter-dungeon
  ([dngn cell] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (* iw ih))]
                                                    (if (= (hiphip/aget dngn rand-loc) floor)
			                                                        (recur (do (hiphip/aset dngn rand-loc cell) (inc ctr))) (recur ctr))))))
  ([dngn shown cell shown-cell filt] (loop [ctr 0] (if (>= ctr  1) dngn (let [rand-loc (rand-int (* iw ih))]
                                                         (if (filt (hiphip/aget ^doubles dngn rand-loc))
			                                                        (recur (do (aset ^chars shown ^int rand-loc ^char shown-cell) (hiphip/aset ^doubles dngn rand-loc cell) (inc ctr))) (recur ctr)))))))

(defn find-cells [a cell-kind]
  (let [dngn (vec a)]
  	  (into {} (for [x (keep-indexed #(if (= %2 cell-kind) %1) dngn)] [x cell-kind]))))

(defn find-goals [a]
  (find-cells a GOAL))

(defn find-walls [a]
  (let [dngn (vec a)]
  	  (into {} (for [x (keep-indexed #(if (>= %2 wall) %1) dngn)] [x wall]))))

(defn find-floors [a]
  (find-cells a floor))

(defn find-lowest [a]
  (let [low-val (apply min (vec a))]
    (find-cells a low-val)))

(defn find-monsters [m]
    (into {} (for [mp (map #(:pos @%) m)] [mp wall])))

(defn step [a i x]
  (if (>= x wall)
    x
    (let [n (hiphip/aget a (- i wide))
          s (hiphip/aget a (+ i wide))
          w (hiphip/aget a (- i 1 ))
          e (hiphip/aget a (+ i 1 ))
          mn (min n s w e)]
      (if (>= mn wall) x (if (> (dec x) mn) (inc mn) x)))))

(def open (atom {}))

(defn dijkstra
  ([a]
     (do (dijkstra a (find-walls a) (find-lowest a))))
  ([a ent]
     (do (dijkstra a (dissoc (merge (find-walls a) (find-monsters @monsters)) (:pos @ent)) (find-lowest a))))
  ([a closed open-cells]
     (reset! open open-cells)
     (while (not (empty? @open))
     	     (let [newly-open (atom {})]
     	     (doall (for [[i v] @open]
    			  (let [n (- i wide)
    			        s (+ i wide)
    			        w (- i 1 )
    			        e (+ i 1 )
    			        ]
    			        (if (or (closed n) (@open n) (>= (inc v) (hiphip/aget a n))) nil (do (hiphip/aset a n (inc v)) (swap! newly-open assoc n (inc v))))
    			        (if (or (closed s) (@open s) (>= (inc v) (hiphip/aget a s))) nil (do (hiphip/aset a s (inc v)) (swap! newly-open assoc s (inc v))))
    			        (if (or (closed w) (@open w) (>= (inc v) (hiphip/aget a w))) nil (do (hiphip/aset a w (inc v)) (swap! newly-open assoc w (inc v))))
    			        (if (or (closed e) (@open e) (>= (inc v) (hiphip/aget a e))) nil (do (hiphip/aset a e (inc v)) (swap! newly-open assoc e (inc v))))
     	     	     	     )))
     	     (reset! open @newly-open)))
       a
       ))

(defn prepare-bones []
         (let [dungeon-z (make-bones)
               dungeon (first dungeon-z)
               dngn-eh (init-dungeon dungeon)]
                      (loop [
                          start (double-array (map #(if (< % wall) floor %) (replace {floor wall} (vec (dijkstra dungeon)))))
                          worst (apply max (filter (partial > wall) (vec (dijkstra (hiphip/aclone start)))))
                          shown (last dungeon-z)]
                        (if (> worst (/ (+ wide high) 4))

                            [(double-array (map-indexed #(if (= %2 GOAL)
                                                                       (do (aset ^chars shown %1 \<) 10001.0)
                                                                       (if (< %2 wall) floor %2))
                                                                    (alter-dungeon (dijkstra (init-dungeon start)) shown 10002.0 \> #(and (> % (/ (+ wide high) 4)) (< % floor)))))
                                                                shown]
                                    (let [d0 (make-bones)
                                          d2 (double-array (map #(if (< % wall) floor %) (replace {floor wall} (vec (dijkstra (first d0))))))
                                          d3 (init-dungeon d2)
                                          w2 (apply max (filter (partial > wall) (vec (dijkstra (hiphip/aclone d2)))))]
                                      (recur d3 w2 (harray/afill! char [[i x] ^chars (last d0)] (if (= (aget ^doubles d3 i) wall) \# x))))))))

(defn freshen [dd ^SGPane p & args]
  (let [player-fov-new (swap! player assoc :seen (run-fov-player player dd))
                                     ]
                                 (doseq [x (range wide) y (range high)]
                                                 (when
                                                   (or args
                                                    (<= (+ (Math/abs ^int (- (mod (:pos @player) wide) x)) (Math/abs ^int (- (quot (:pos @player) wide) y))) 13)
 ;                                                          (>= (- (mod (:pos @player) wide) x) -16)
;                                                           (<= (Math/abs ^int (- (quot (:pos @player) wide) y)) 16)
  ;                                                         (>= (- (quot (:pos @player) wide) y) -16)
                                                     )
                                                   (. p placeCharacter x
                                                                     y
                                                                     (if (= (aget ^doubles (:dungeon @dd) (+ x (* wide y))) wall) \# (aget ^chars (:shown @dd) (+ x (* wide y))))
                                                                     SColor/BLACK
                                                                     (if (= (aget ^doubles (:dungeon @dd) (+ x (* wide y))) wall)
                                                                       (if
                                                                         (> (aget (:seen @player) x y) 0)
                                                                         (SColorFactory/blend SColor/BLACK SColor/CYPRESS_BARK_RED (aget (:seen @player) x y))
                                                                         (if
                                                                             (aget ^"[Z" (:full-seen @player) (+ x (* wide y)))
                                                                             SColor/BOILED_RED_BEAN_BROWN
                                                                             SColor/BLACK))
                                                                       (if
                                                                         (> (aget (:seen @player) x y) 0)
                                                                           (SColorFactory/blend SColor/BLACK SColor/CREAM (aget (:seen @player) x y))
                                                                           (if
                                                                             (aget ^"[Z" (:full-seen @player) (+ x (* wide y)))
                                                                               SColor/DARK_GRAY
                                                                               SColor/BLACK
                                                                             ))))
                                                      ))
                              (do (. p placeCharacter (mod (:pos @player) wide)
                                                      (quot (:pos @player) wide)
                                                      (:show @player)
                                                      SColor/BLACK
                                                      SColor/CREAM))
                              (doseq [monster @monsters] (when (> (aget (:seen @player) (mod (:pos @monster) wide) (quot (:pos @monster) wide)) 0)
                                                          (. p placeCharacter (mod (:pos @monster) wide)
                                                                              (quot (:pos @monster) wide)
                                                                              (:show @monster)
                                                                              SColor/FOREIGN_CRIMSON

                                                      (SColorFactory/blend SColor/BLACK SColor/CREAM (aget (:seen @player) (mod (:pos @monster) wide) (quot (:pos @monster) wide))))))
                              (.refresh p)
                              (config! (acquire [:#entities]) :items (concat [(make-player-label)] (visible-monsters)))
                              (-> f show! )))

(defn damage-player [entity dd ^SGPane p]
  (do (swap! entity assoc :hp (- (:hp @entity) (inc (rand-int 6))))
    (if (<= (:hp @player) 0)
      (do
        (doseq [^KeyListener kl (vec (.getKeyListeners ^Component f))] (.removeKeyListener ^Component f kl))
        (freshen dd p)
        (show! (pack! (dialog
            :content (str "GAME OVER.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @player))))))
                          " squares and reached floor " (inc @dlevel) "."
                          )
            :success-fn (fn [jop] (System/exit 0))))))
      (freshen dd p))))

(defn damage-monster
  ([entity dd ^SGPane p]
  (do (swap! entity assoc :hp (- (:hp @entity) (inc (rand-int 6))))
    (when (<= (:hp @entity) 0)
      (reset! monsters (remove #(= % entity) @monsters)))
     (freshen dd p)))
  ([entity dd ^SGPane p dice die-size]
  (do (swap! entity assoc :hp (- (:hp @entity) (reduce + (repeatedly dice #(inc (rand-int die-size))))))
    (when (<= (:hp @entity) 0)
      (reset! monsters (remove #(= % entity) @monsters)))
     (freshen dd p))))


(defn move-monster [mons dd ^SGPane p]
  (let [flee-map (let [first-d (hiphip/aclone ^doubles (:dungeon @dd))
                                                     d-eh (aset first-d (int (:pos @player)) GOAL)
                                                     new-d (hiphip/afill! [[idx x] (dijkstra first-d)]
                                                                        (if (= (:pos @player) idx)
                                                                          10007.0
                                                                          (if (>= x wall)
                                                                            wall
                                                                            (Math/floor (* (selection (acquire-sp [:#flee-spinner])) x)))
                                                                          ))]
                   (dijkstra new-d))]
                             (doseq [monster mons]
                               (let [monster-fov-new (run-fov monster dd)]
                                 (if (> (:hp @monster) 2)
                                   (when (> (aget monster-fov-new (mod (:pos @player) wide) (quot (:pos @player) wide)) 0)
                                     (do (swap! monster assoc :dijkstra
                                               (let [new-d (hiphip/aclone ^doubles (:dungeon @dd))] (aset new-d (int (:pos @player)) GOAL) (dijkstra new-d monster)))))
                                   (when (> (aget monster-fov-new (mod (:pos @player) wide) (quot (:pos @player) wide)) 0)
                                     (do (swap! monster assoc :dijkstra flee-map))))))
                             (doseq [mon mons]
                                (let [oldpos (:pos @mon)]
                                  (when (<= (aget (:seen @player) (mod (:pos @mon) wide) (quot (:pos @mon) wide)) 0)
                                                          (. p placeCharacter (mod (:pos @mon) wide)
                                                                              (quot (:pos @mon) wide)
                                                                              (if (aget ^"[Z" (:full-seen @player) (:pos @mon))
                                                                                (aget ^chars (:shown @dd) (:pos @mon))
                                                                                \space)
                                                                              SColor/BLACK
                                                                              (if
                                                                                (aget ^"[Z" (:full-seen @player) (:pos @mon))
                                                                                  SColor/DARK_GRAY
                                                                                  SColor/BLACK
                                                                             )))
                                (if (:dijkstra @mon) (let [orig-pos (:pos @mon)
                                                          adjacent (shuffle [
                                                                    (- orig-pos wide)
                                                                    (+ orig-pos wide)
                                                                    (- orig-pos 1)
                                                                    (+ orig-pos 1)])
                                                          lowest (reduce #(if (and
                                                                               (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [%2 (:pos @player)]))
                                                                               (< (aget ^doubles (:dijkstra @mon) %2) (aget ^doubles (:dijkstra @mon) %1))
                                                                               (= (aget ^doubles (:dungeon @dd) %2) floor))
                                                                            %2
                                                                            %1)
                                                                         orig-pos
                                                                         adjacent)]
                                                      (swap! mon assoc :pos lowest)
                                                      (when (or (= (- (:pos @player) wide) (:pos @mon))
                                                                (= (+ (:pos @player) wide) (:pos @mon))
                                                                (= (- (:pos @player) 1   ) (:pos @mon))
                                                                (= (+ (:pos @player) 1   ) (:pos @mon)))
                                                        (damage-player player dd p)))

                                 ((rand-nth [
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(- (:pos @%) wide) (:pos @player)]))
                                                      (= (aget ^doubles (:dungeon @dd) (- (:pos @%) wide)) floor)) (swap! % assoc :pos (- (:pos @%) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(+ (:pos @%) wide) (:pos @player)]))
                                                      (= (aget ^doubles (:dungeon @dd) (+ (:pos @%) wide)) floor)) (swap! % assoc :pos (+ (:pos @%) wide)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(- (:pos @%) 1) (:pos @player)]))
                                                      (= (aget ^doubles (:dungeon @dd) (- (:pos @%) 1)) floor)) (swap! % assoc :pos (- (:pos @%) 1)))
                                          #(when (and (apply distinct? (concat (map (fn [atm] (:pos @atm)) mons) [(+ (:pos @%) 1) (:pos @player)]))
                                                      (= (aget ^doubles (:dungeon @dd) (+ (:pos @%) 1)) floor)) (swap! % assoc :pos (+ (:pos @%) 1)))]
                                            ) mon)))
;                               (when (= \# (aget ^chars (:shown @dd) (:pos @mon)))
;                                 (println "Monster intersecting with wall"))
                                 )))
(defn escape-dialog
  [pc mons dd ^SGPane p ^SGKeyListener kl]
  (do
    (. kl flush)
    (try
      (let [dlg
         (dialog
            :content (do
                      (. kl flush)
                       (str "YOU ESCAPED.  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                          " squares."))
            :success-fn (fn [jop] (System/exit 0)))]
      (show! (pack! dlg)))
        (catch Exception iae (println "Please, don't press the keys so fast...")))))

(defn ascend
  [pc mons dd ^SGPane p]
  (swap! cleared-levels assoc @dlevel (assoc @dd :full-seen (aclone ^"[Z" (:full-seen @pc))))
  (swap! dlevel dec)
  (let [
                                dd1 (:dungeon (get @cleared-levels @dlevel))
                                dungeon-res (:res (get @cleared-levels @dlevel))
                                shown (:shown (get @cleared-levels @dlevel))
                                player-calc  (init-dungeon dd1 pc 10002.0)
                                monster-calc (doall (map #(do (init-dungeon dd1 %)(swap! % assoc :dijkstra nil)) @monsters))]
                            (harray/afill! boolean [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" (:full-seen (get @cleared-levels ^int @dlevel)) i))
                            (reset! dd {:dungeon dd1 :shown shown :res dungeon-res})
                            (freshen dd p :full)))

(defn descend
  [pc mons dd ^SGPane p]
  (swap! cleared-levels assoc @dlevel (assoc @dd :full-seen (aclone ^"[Z" (:full-seen @pc))))
  (swap! dlevel inc)
  (if (contains? @cleared-levels @dlevel)
    (let [
      dd1 (:dungeon (get @cleared-levels @dlevel))
      dungeon-res (:res (get @cleared-levels @dlevel))
      shown (:shown (get @cleared-levels @dlevel))
      player-calc  (init-dungeon dd1 pc 10001.0)
      monster-calc (doall (map #(do (init-dungeon dd1 %) (swap! % assoc :dijkstra nil)) @mons))]
      (harray/afill! Boolean/TYPE [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" (:full-seen (get @cleared-levels ^int @dlevel)) i))
      (reset! dd {:dungeon dd1 :shown shown :res dungeon-res})
      (freshen dd p :full))
    (let [
      dd0 (prepare-bones)
      dd1 (first dd0)
      dungeon-res (dungeon-resistances dd1)
      shown (last dd0)
      player-calc  (init-dungeon dd1 pc 10001.0)
      monster-calc (doall (map #(init-dungeon dd1 %) @mons))
      blank-seen (init-full-seen)]
      (harray/afill! Boolean/TYPE [[i x] ^"[Z" (:full-seen @pc)] (aget ^"[Z" blank-seen i))
      (reset! dd {:dungeon dd1 :shown shown :res dungeon-res})
      (freshen dd p :full))))

(defn move-player [pc mons dd ^SGPane p ^SGKeyListener kl newpos]
  (do (if
        (and (apply distinct? (conj (map (fn [atm] (:pos @atm)) @mons) newpos))
             (or (= (aget ^doubles (:dungeon @dd) newpos) floor) (= (aget ^doubles (:dungeon @dd) newpos) 10001.0) (= (aget ^doubles (:dungeon @dd) newpos) 10002.0)))
        (do
          (swap! pc assoc :pos newpos)
          (condp = (aget ^doubles (:dungeon @dd) newpos)
            floor (do
                    (move-monster @mons dd p)
                    (freshen dd p))
            10001.0 (do
                      (. kl flush)
                      (if (= @dlevel 0)
                         (do
                           (config! (acquire [:#messages]) :items (conj (config (acquire [:#messages]) :items) (make-other-message
                             (str "YOU ESCAPED.  You explored "
                               (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                               " squares."))))
                           (escape-dialog pc mons dd p kl))
                         (do (ascend pc mons dd p)
                           (config! (acquire [:#messages]) :items (conj (config (acquire [:#messages]) :items) (make-other-message
                             (str "YOU ASCEND TO FLOOR " (inc @dlevel) "...  You explored "
                               (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                               " squares.")))))
                    )) ;(show! (pack! (ascend-dialog pc mons dd p kl)))
            10002.0 (do
                      (. kl flush)
                      (descend pc mons dd p)
                      (config! (acquire [:#messages]) :items (conj (config (acquire [:#messages]) :items) (make-other-message
                        (str "YOU DESCEND TO FLOOR " (inc @dlevel) "...  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                           " squares.")))))
                     ;(show! (pack! (descend-dialog pc mons dd p kl)))
            (println "Something's wrong."))
          )
        (when (= (aget ^doubles (:dungeon @dd) newpos) floor)

          (doseq [mon @mons] (when (= (:pos @mon) newpos)
                                   (damage-monster mon dd p)))
          (move-monster @mons dd p)
          (freshen dd p)))))

(defn shoot [pc mons dd ^SGPane p ^SGKeyListener kl target]
    (let [mon-list (drop-while #(not= target (:ident @%)) @mons)]
      (when (seq mon-list)
        (let [tgt (first mon-list)]
      (when (> (aget (:seen @pc) (mod (:pos @tgt) wide) (quot (:pos @tgt) wide)) 0)
        (damage-monster tgt dd p 1 2)
        (move-monster @mons dd p)
        (freshen dd p))
    ))))


(defn show-dungeon []
	(invoke-later
	        (let [dd0 (prepare-bones)
                dd1 (first dd0)
                shown-bones (last dd0)
                ;dd0 (double-array (map #(if (not= % wall) floor wall) (replace {floor wall} (dijkstra dungeon))))
                dd-eh (doseq [i (range 3)] (init-dungeon dd1))
                ^doubles dd (dijkstra dd1)
                p (pane)
                p-eh (display (border-panel :center (do (.refresh p) p)))
                worst (apply max (filter (partial > wall) (vec dd)))
                freshen2 (fn []
                                 (doseq [x (range wide) y (range high)]
                                                 (. p placeCharacter x
                                                                     y
                                                                     (if (= (aget dd (+ x (* wide y))) wall) \space (aget ^chars shown-bones (+ x (* wide y))))
                                                                     SColor/BLACK
                                                                     (if (= (aget dd (+ x (* wide y))) wall)
                                                                       SColor/BLACK
                                                                       (if (= (aget dd (+ x (* wide y))) GOAL)
                                                                         SColor/ORANGUTAN
                                                                         ;SColor/CREAM
                                                                         (SColorFactory/blend SColor/CREAM SColor/DARK_BLUE_LAPIS_LAZULI (/ (aget dd (+ x (* wide y))) worst))
                                                                           )))
                                                      )
                              (.refresh p)
                              (config! (acquire [:#entities]) :items (concat [(make-player-label)] (visible-monsters)))
                              (-> f pack! show! ))
                ]
            (doseq [row (partition wide (vec shown-bones))]
              (println (apply str (map #(if (= % \#) \# \.) row))))
            (freshen2)
            )))


(defn -main
	[& args]
;  (comment ;"Remove these semicolons to view a dungeon when you run"
  (invoke-later
	        (let [dd0 (prepare-bones)
                dd (first dd0)
                dungeon-res (dungeon-resistances dd)
                shown (last dd0)
                player-calc  (init-dungeon dd player)
                monster-calc (doall (map #(init-dungeon dd %) @monsters))
                dun (atom {:dungeon dd :shown shown :res dungeon-res})
                player-fov-first (swap! player assoc :seen (run-fov-player player dun))
                ^SGKeyListener kl (SGKeyListener. true SGKeyListener$CaptureType/DOWN)
                ^SGKeyListener kl-up (SGKeyListener. true SGKeyListener$CaptureType/UP)
                p (pane)
                spn (spin)
                stats (stats-pane)
                msgs (messages-pane)
                p-eh (display (border-panel :center (do (.refresh p) p) :west stats :south msgs))
                pack-eh (pack! f)
                kl-eh (.addKeyListener ^Component f kl)
                kl-up-eh (.addKeyListener ^Component f kl-up)
                tmr (timer (fn [t]
                             (when (.hasNext kl)
                               (let [^KeyEvent e (.next kl)]
                                ; (println "GOT KEYPRESS")
                                   ;(when (not (distinct? (.getKeyCode e) KeyEvent/VK_UP KeyEvent/VK_DOWN KeyEvent/VK_LEFT KeyEvent/VK_RIGHT
                                   ;                       KeyEvent/VK_H KeyEvent/VK_J KeyEvent/VK_K KeyEvent/VK_L))
                          (condp = (.getKeyCode e)
                              KeyEvent/VK_UP    (move-player player monsters dun p kl (- (:pos @player) wide))
                              KeyEvent/VK_DOWN  (move-player player monsters dun p kl (+ (:pos @player) wide))
                              KeyEvent/VK_LEFT  (move-player player monsters dun p kl (- (:pos @player) 1))
                              KeyEvent/VK_RIGHT (move-player player monsters dun p kl (+ (:pos @player) 1))
                              KeyEvent/VK_K   (move-player player monsters dun p kl (- (:pos @player) wide))
                              KeyEvent/VK_J   (move-player player monsters dun p kl (+ (:pos @player) wide))
                              KeyEvent/VK_H   (move-player player monsters dun p kl (- (:pos @player) 1))
                              KeyEvent/VK_L   (move-player player monsters dun p kl (+ (:pos @player) 1))

                              KeyEvent/VK_0   (shoot player monsters dun p kl \0)
                              KeyEvent/VK_1   (shoot player monsters dun p kl \1)
                              KeyEvent/VK_2   (shoot player monsters dun p kl \2)
                              KeyEvent/VK_3   (shoot player monsters dun p kl \3)
                              KeyEvent/VK_4   (shoot player monsters dun p kl \4)
                              KeyEvent/VK_5   (shoot player monsters dun p kl \5)
                              KeyEvent/VK_6   (shoot player monsters dun p kl \6)
                              KeyEvent/VK_7   (shoot player monsters dun p kl \7)
                              KeyEvent/VK_8   (shoot player monsters dun p kl \8)
                              KeyEvent/VK_9   (shoot player monsters dun p kl \9)
                              KeyEvent/VK_A   (shoot player monsters dun p kl \a)
                              KeyEvent/VK_B   (shoot player monsters dun p kl \b)
                              KeyEvent/VK_C   (shoot player monsters dun p kl \c)
                              KeyEvent/VK_D   (shoot player monsters dun p kl \d)
                              KeyEvent/VK_E   (shoot player monsters dun p kl \e)
                              KeyEvent/VK_F   (shoot player monsters dun p kl \f)

                              KeyEvent/VK_W   (let [mm (move-monster @monsters dun p)]
                                                (freshen dun p)
                                                (doseq [flee-row (partition wide mm)]
                                                  (println (apply str (map #(format " %-4d" (int %)) flee-row))))
                                                (println "\n Player pos: " (:pos @player)
                                                          "  Player X: " (mod (:pos @player) wide)
                                                          "  Player Y: " (quot (:pos @player) wide)))
                              nil)))
                         (when (.hasNext kl-up)
                           (.flush kl)
                           (.flush kl-up)))
                           :delay 110)
                ]
            (freshen dun p)
            ))
;  )
;  (show-dungeon)
  )
