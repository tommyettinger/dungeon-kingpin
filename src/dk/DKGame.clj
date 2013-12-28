(ns dk.DKGame
  (:use dk.core)
  (:require [clojure.pprint :as p]
            [clojure.java.io :as io])
  (:import [com.badlogic.gdx Game Screen Gdx Files Input Input$Keys InputProcessor]
           [com.badlogic.gdx.math Rectangle Vector3]
           [com.badlogic.gdx.scenes.scene2d Stage]
           [com.badlogic.gdx.scenes.scene2d.ui Label Label$LabelStyle Image]
           [com.badlogic.gdx.graphics Color GL10 OrthographicCamera Texture]
           [com.badlogic.gdx.graphics.g2d BitmapFont TextureAtlas TextureAtlas$AtlasSprite TextureAtlas$AtlasRegion SpriteBatch Sprite]
           [squidpony.squidcolor SColor SColorFactory]
           [java.io FileWriter])
  ;(:require (dk [dkscreen :as dkscreen]))
  )
(set! *warn-on-reflection* true)

(gen-class
 :name dk.DKGame
 :extends com.badlogic.gdx.Game)

(defmethod print-dup (Class/forName "[D") [a out] (.write ^java.io.FileWriter out (str "#=" `(double-array ~(vec a)))))
(defmethod print-dup (Class/forName "[C") [a out] (.write ^java.io.FileWriter out (str "#=" `(char-array ~(vec a)))))
(defmethod print-dup (Class/forName "[Z") [a out] (.write ^java.io.FileWriter out (str "#=" `(boolean-array ~(vec a)))))

(defn restore-arr [vv]
  (let [arr (make-array Float/TYPE (count vv) (count (nth vv 0)))]
    (doseq [a2 (range (count vv))] (aset ^"[[F" arr a2 (float-array (nth vv a2)))) arr))
(defmethod print-dup (Class/forName "[[F") [a out] (.write ^java.io.FileWriter out (str "#=" `(restore-arr ~(mapv (partial mapv double) a)))))
(declare sv) (declare sh) ; straight vert/horiz
(declare gr) (declare da) ; ground/dark
(declare ul) (declare ur) ; up left/right (pointing these ways)
(declare dl) (declare dr) ; down left/right
(declare cw) ; crosswall
(declare Tu) (declare Td) (declare Tl) (declare Tr) ; T-shaped up/down/left/right


(defn -create [^Game this]
   ;(.setContinuousRendering Gdx/graphics false)
   ;(.requestRendering Gdx/graphics)
   (def screen-width  (atom 1200.0))
   (def screen-height (atom 640.0))
   (def ^BitmapFont mandrill-16 (BitmapFont. (.internal ^Files Gdx/files "Mandrill-16-mono.fnt") false))
   (.setColor ^BitmapFont mandrill-16 0.0 1.0 0.3 1.0)
   (def ^BitmapFont mandrill-16-red (BitmapFont. (.internal ^Files Gdx/files "Mandrill-16-mono.fnt") false))
   (.setColor ^BitmapFont mandrill-16-red 1.0 0.1 0.1 1.0)
   (def ^BitmapFont mandrill-16-black (BitmapFont. (.internal ^Files Gdx/files "Mandrill-16-mono.fnt") false))
   (.setColor ^BitmapFont mandrill-16-black 0.0 0.0 0.0 1.0)
   (def health (mapv #(doto (Sprite. (Texture. (str "health/" % ".png")) 20 20) (.setColor 0.0 1.0 0.3 0.5)) (range 21)))
   ;(doseq [sp (range 21)] (.setColor ^Sprite (nth health sp) 0.0 1.0 0.3 1.0))
   (def ^Sprite health-red (Sprite. (Texture. "health-red.png") 20 20))
   ;█
   (def ^TextureAtlas packed (TextureAtlas. (.internal ^Files Gdx/files "slashem-packed/pack.atlas")))
   (def ^SpriteBatch batch (SpriteBatch.))
   (defn ^TextureAtlas$AtlasSprite grab [sprite] (TextureAtlas$AtlasSprite. ^TextureAtlas$AtlasRegion (.findRegion ^TextureAtlas packed sprite)))

   (def dragon (grab "monster-dragon-ixoth")) ; monster-eye or sphere-beholder
   (def monster-tiles [(grab "monster-dragon-ixoth")
                       (grab "monster-eye or sphere-beholder")
                       (grab "monster-cat or other feline-displacer beast")
                       (grab "monster-ant or other insect-migo queen")
                       (grab "monster-golem-diamond golem")
                       (grab "monster-major demon-ice devil")
                       (grab "monster-ogre-shadow ogre")
                       (grab "monster-pudding or ooze-giant shoggoth")
                       (grab "monster-humanoid-scathe seer")
                       (grab "monster-dog or other canine-mist wolf")
                       (grab "monster-major demon-demogorgon")])
   (def nucky (grab "monster-human or elf-poison magus"))
   (def sv (grab "cmap-wall-vertical"))
   (def sh (grab "cmap-wall-horizontal"))
   (def gr (grab "cmap-floor of a room"))
   (def da (grab "cmap-dark part of a room"))
   (def ul (grab "cmap-wall-bottom right corner"))
   (def ur (grab "cmap-wall-bottom left corner"))
   (def dl (grab "cmap-wall-top right corner"))
   (def dr (grab "cmap-wall-top left corner"))
   (def cw (grab "cmap-wall-crosswall"))
   (def Tu (grab "cmap-wall-tee up"))
   (def Td (grab "cmap-wall-tee down"))
   (def Tl (grab "cmap-wall-tee left"))
   (def Tr (grab "cmap-wall-tee right"))
   (def stairs-up (grab "cmap-staircase up"))
   (def stairs-down (grab "cmap-staircase down"))
   (def water  (grab "cmap-water")) ; ~
   (def ice    (grab "cmap-ice")) ; ^
   (def stones (grab "object-venoms-splash of venom-blinding venom")) ; &
   (def statue (grab "object-large stones-statue")) ; $
   (def leaf   (grab "object-food-eucalyptus leaf")) ; %



 (defn clean-bones [^doubles dd ^chars shown]
  (map-indexed (fn [i t] (if (= t wall)
    (let [left  (if (> (mod i wide) 0)           (and (not= (aget dd (- i 1)) dark)    (= (aget dd (- i 1)) wall))    false)
          right (if (< (mod i wide) (dec wide))  (and (not= (aget dd (+ i 1)) dark)    (= (aget dd (+ i 1)) wall))    false)
          top   (if (> (quot i wide) 0)          (and (not= (aget dd (- i wide)) dark) (= (aget dd (- i wide)) wall)) false)
          down  (if (< (quot i wide) (dec high)) (and (not= (aget dd (+ i wide)) dark) (= (aget dd (+ i wide)) wall)) false)

          downleft  (if (and (< (quot i wide) (dec high)) (> (mod i wide) 0))           (and (not= (aget dd (+ i -1 wide)) dark)    (= (aget dd (+ i -1 wide)) wall))    false)
          downright (if (and (< (quot i wide) (dec high)) (< (mod i wide) (dec wide)))  (and (not= (aget dd (+ i  1 wide)) dark)    (= (aget dd (+ i  1 wide)) wall))    false)
          upleft    (if (and (> (quot i wide) 0) (> (mod i wide) 0))                    (and (not= (aget dd (- i  1 wide)) dark)    (= (aget dd (- i  1 wide)) wall))    false)
          upright   (if (and (> (quot i wide) 0) (< (mod i wide) (dec wide)))           (and (not= (aget dd (- i -1 wide)) dark)    (= (aget dd (- i -1 wide)) wall))    false)]
          (cond
           left (cond
                    right (cond
                              top (if (not down) (if (and upleft upright) sh Tu) (cond
                                                                                  (and upleft upright downleft downright) da
                                                                                  (and (not upleft) upright downleft downright) ul
                                                                                  (and upleft (not upright) downleft downright) ur
                                                                                  (and upleft upright (not downleft) downright) dl
                                                                                  (and upleft upright downleft (not downright)) dr

                                                                                  (and (not upleft) (not upright) downleft downright) Tu
                                                                                  (and upleft (not upright) downleft (not downright)) Tr
                                                                                  (and upleft upright (not downleft) (not downright)) Td
                                                                                  (and (not upleft) upright (not downleft) downright) Tl
                                                                                  :else cw))
                              down (if (and downleft downright) sh Td)
                              :else sh)
                    top (if down (if (and upleft downleft) sv Tl) ul)
                    down dl
                    :else sh
                    )
          right (cond
                    top (if down (if (and downright upright) sv Tr) ur)
                    down dr
                    :else sh)
          (or top down) sv
          :else sh
          ))

          (condp = t
                10001.0 stairs-up
                10002.0 stairs-down
            (condp = (aget shown i)
                   \~ water
                   \^ ice
                   \$ statue
                   \+ ice ; stones
                   \% leaf
                   gr))
        )) (vec dd)))

   (if (.exists (io/file "Savefile.edn"))
     (let [
         sav (read-string (slurp "Savefile.edn"))
         sav-wide (nth sav 0)
         sav-high (nth sav 1)
         ;dd0 (prepare-bones)
         dd1 (nth sav 2)
         shown (nth sav 3)]
         (reset! res (dungeon-resistances dd1))
         (reset! cleared-levels (nth sav 4))
         (reset! dlevel (nth sav 5))
         (reset! player (nth sav 6))
         (reset! monsters (mapv atom (nth sav 7)))
         (io/delete-file "Savefile.edn" true)
         (def dd dd1)
         (def dun (atom {:dungeon dd :shown shown}))
         (def colors (mapv (fn [_] (Color. (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) 1.0)) (range (* wide high))))
         (def cleaned (atom(clean-bones dd shown)))

;         (init-dungeon dd player)
;         (swap! player assoc :seen (run-fov-player player dun))
;         (doall (map #(init-dungeon dd %) @monsters))
;         (doall (map #(swap! % assoc :tile (rand-int (count monster-tiles))) @monsters))
         (def monster-hash (atom(into {} (map (fn [entry] [(:pos @entry) entry]) @monsters))))
         )
     ; [wide high (:dungeon @dun) (:shown @dun) @cleared-levels @dlevel @player (mapv deref @monsters)]
      (let [
         dd0 (prepare-bones)
         dd1 (first dd0)
         dungeon-res (dungeon-resistances dd1)
         shown (last dd0)]
         (reset! res (dungeon-resistances dd1))
         (def dd dd1)
         (def dun (atom {:dungeon dd :shown shown}))
         (def colors (mapv (fn [_] (Color. (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) 1.0)) (range (* wide high))))
         (def cleaned (atom(clean-bones dd shown)))

         (init-dungeon dd player)
         (swap! player assoc :seen (run-fov-player player dun))
         (doall (map #(init-dungeon dd %) @monsters))
         (doall (map #(swap! % assoc :tile (rand-int (count monster-tiles))) @monsters))
         (def monster-hash (atom(into {} (map (fn [entry] [(:pos @entry) entry]) @monsters))))
         ))
   (defn update-fov [dun]
         (swap! player assoc :seen (run-fov-player player dun))
         (doall (map-indexed #(let [x (mod %1 wide) y (quot %1 wide)] (Color/rgb888ToColor %2 (.getRGB ^SColor (if (> (aget (:seen @player) x y) 0)
                                                                           (SColorFactory/blend SColor/BLACK SColor/CREAM (aget (:seen @player) x y))
                                                                           (if
                                                                             (aget ^"[Z" (:full-seen @player) (+ x (* wide y)))
                                                                               SColor/GRAY
                                                                               SColor/BLACK
                                                                             ))))) colors))
     colors)
   (update-fov dun)
   (def ^OrthographicCamera camera (OrthographicCamera.))
   (.setToOrtho ^OrthographicCamera camera false @screen-width @screen-height)
   (.set ^Vector3 (.position ^OrthographicCamera camera) (+ (* 32.0 (mod (:pos @player) wide)) (* 16.0 (- wide (quot (:pos @player) wide)))) (- @screen-height 64.0 (* 32.0 (quot (:pos @player) wide))) 0.0)

   (def glViewport (atom (Rectangle. 0.0 0.0 @screen-width @screen-height)))


(defn refresher []
  (let [gl (.getGL10 Gdx/graphics)
        seen (:seen @player)]
	(.glClear gl GL10/GL_COLOR_BUFFER_BIT);
  (.glViewport gl (int (.x ^Rectangle @glViewport)) (int (.y ^Rectangle @glViewport)) (int (.width ^Rectangle @glViewport)), (int (.height ^Rectangle @glViewport)))
  (.update ^OrthographicCamera camera)
  (.apply ^OrthographicCamera camera gl)

  (.setProjectionMatrix ^SpriteBatch batch (.combined ^OrthographicCamera camera))
  (.begin ^SpriteBatch batch)
  (doseq [y (range high)]
  (doseq [x (range wide)]
    (let [is-seen (aget seen x y) idx (+ (* y wide) x) c (nth @cleaned idx)] ; x (mod idx wide) y (quot idx wide)
     (when
        (and (< (Math/abs (- (+ (* 32.0 (mod (:pos @player) wide)) (* 16.0 (- wide (quot (:pos @player) wide))))
                             (+ (* 32.0 x) (* 16 (- wide y))))) (+ 48 (/ @screen-width 2)))
             (< (Math/abs (- (- @screen-height 64.0 (* 32.0 (quot (:pos @player) wide)))
                             (- @screen-height 64 (* 32 y)))) (+ 64 (/ @screen-height 2)))
             )
      ;  (<= (+ (Math/abs ^int (- (mod (:pos @player) wide) x)) (Math/abs ^int (- (quot (:pos @player) wide) y))) 13)
        (.setColor ^SpriteBatch batch ^Color (nth colors idx)) ; ^TextureAtlas$AtlasSprite %2
        (if (or (> is-seen 0) (aget ^"[Z" (:full-seen @player) (+ x (* wide y)))) (when (not= (nth (:dungeon @dun) idx) wall)
                                                                              (.draw ^SpriteBatch batch (if (or (= c ice) (= c water)) ^TextureAtlas$AtlasSprite c ^TextureAtlas$AtlasSprite gr)
                                                                                  (+ (* 32.0 x) (* 16 (- wide y)))
                                                                                  (- @screen-height 64.0 (* 32 y)))
                                                                                        ))
      )
      )
    )
  (doseq [x (range wide)]
    (let [is-seen (aget seen x y) idx (+ (* y wide) x) c (nth @cleaned idx)] ; x (mod idx wide) y (quot idx wide)
      (when
        (and (< (Math/abs (- (+ (* 32.0 (mod (:pos @player) wide)) (* 16.0 (- wide (quot (:pos @player) wide))))
                             (+ (* 32.0 x) (* 16 (- wide y))))) (+ 24 (/ @screen-width 2)))
             (< (Math/abs (- (- @screen-height 64.0 (* 32.0 (quot (:pos @player) wide)))
                             (- @screen-height 64.0 (* 32 y)))) (+ 64 (/ @screen-height 2)))
             )
      ;  (<= (+ (Math/abs ^int (- (mod (:pos @player) wide) x)) (Math/abs ^int (- (quot (:pos @player) wide) y))) 13)
        (.setColor ^SpriteBatch batch ^Color (nth colors idx))
        ; ^TextureAtlas$AtlasSprite %2

        (if (or (> is-seen 0) (aget ^"[Z" (:full-seen @player) (+ x (* wide y)))) (do (when (or (>= (nth (:dungeon @dun) idx) wall) (= c statue) (= c stones) (= c leaf))

                                                                            (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite (nth @cleaned idx)
                                                                                (+ (* 32.0 x) (* 16 (- wide y)))
                                                                                (- @screen-height 64.0 (* 32 y))))))
        ;█ ∫
        (when (and (> is-seen 0) (contains? @monster-hash idx))
          (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite (nth monster-tiles (:tile @(get @monster-hash idx))) (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height 64.0 (* 32 y)))
          (.setColor ^SpriteBatch batch 1.0 1.0 1.0 1.0)
          (.draw ^SpriteBatch batch ^Sprite health-red (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32.0 y)))
          (.setColor ^SpriteBatch batch 0.5 1.0 0.0 1.0)
          (.draw ^SpriteBatch batch ^Sprite (nth health (quot 20 (/ 8 (:hp @(get @monster-hash idx))))) (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32.0 y)))
          ;(.draw ^BitmapFont mandrill-16-red ^SpriteBatch batch (str (:hp @(get @monster-hash idx))) (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32 y)))
          )
        (when (and (= idx (:pos @player)) (> (:hp @player) 0))
          (.setColor ^SpriteBatch batch ^Color (nth colors idx))
          (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite nucky (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height 64 (* 32.0 y)))
          (.setColor ^SpriteBatch batch 1.0 1.0 1.0 1.0)
          (.draw ^SpriteBatch batch ^Sprite health-red (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32.0 y)) 20.0 20.0)
          (.setColor ^SpriteBatch batch 0.1 1.0 0.4 1.0)
          (.draw ^SpriteBatch batch ^Sprite (nth health (quot 20 (/ 99 (:hp @player)))) (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32.0 y)))
          ;region.setV(originalRegion.getV() + health * (originalRegion.getV2() - originalRegion.getV()));
          ;(.setV ^Sprite health (float (- 1.0 hp))) ; (float (- 1.0 hp)) ; (Sprite healthMeter).setV(1.0 - (1.0 / (99 / currentHP)));  (SpriteBatch batch).draw(healthMeter, healthX, healthY, 20.0, 20.0 * (currentHP / 99);
          ; (SpriteBatch batch).draw(healthMeter, healthX, healthY, 20.0, 20.0 * (currentHP / 99)
          ;(.draw ^BitmapFont mandrill-16 ^SpriteBatch batch (str (:hp @player)) (+ (* 32.0 x) (* 16 (- wide y))) (- @screen-height (* 32 y)))
          )
      )
    )
    )
    )
  (.end ^SpriteBatch batch)
  ;(.requestRendering Gdx/graphics)
))

(defn shift-player [pc mons dd newpos]
  (if
        (and (apply distinct? (conj (map (fn [atm] (:pos @atm)) @mons) newpos))
             (or (= (aget ^doubles (:dungeon @dd) newpos) floor) (= (aget ^doubles (:dungeon @dd) newpos) 10001.0) (= (aget ^doubles (:dungeon @dd) newpos) 10002.0)))
        (do
          (swap! pc assoc :pos newpos)
          (condp = (aget ^doubles (:dungeon @dd) newpos)
            floor (do
                    (move-monster @mons dd monster-hash)
                    (refresher))
            10001.0 (do
                      (if (= @dlevel 0)
                            (println (str "YOU ESCAPED.  You explored "
                               (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                               " squares."))
                            (do (ascend pc mons dd)
                              (reset! cleaned (clean-bones (:dungeon @dd) (:shown @dd)))
                              (reset! monster-hash (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))
                              (update-fov dd)
                              (refresher)
                              (println
                                (str "YOU ASCEND TO FLOOR " (inc @dlevel) "...  You explored "
                                  (count (filter true?
                                            (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                                  " squares.")))
                    ))
            10002.0 (do
                      (descend pc mons dd)
                      (reset! cleaned (clean-bones (:dungeon @dd) (:shown @dd)))
                      (reset! monster-hash (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))
                      (update-fov dd)
                      (refresher)
                      (println
                        (str "YOU DESCEND TO FLOOR " (inc @dlevel) "...  You explored "
                          (count (filter true?
                                         (vec (concat (flatten (map #(vec (:full-seen (val %))) (dissoc @cleared-levels @dlevel))) (vec (:full-seen @pc))))))
                           " squares.")))
            (println "Something's wrong."))
          )
        (when (= (aget ^doubles (:dungeon @dd) newpos) floor)

          (doseq [mon @mons] (when (= (:pos @mon) newpos)
                                   (damage-monster mon dd monster-hash)))
          (move-monster @mons dd monster-hash)))
  (update-fov dd)
          ;(refresher)
  )
  (defn center-camera []
        (.set ^Vector3 (.position ^OrthographicCamera camera) (+ (* 32.0 (mod (:pos @player) wide)) (* 16.0 (- wide (quot (:pos @player) wide)))) (- @screen-height 64.0 (* 32.0 (quot (:pos @player) wide))) 0.0))
  (.setInputProcessor Gdx/input (reify InputProcessor

   (keyDown [this keycode] (do (condp = keycode
      Input$Keys/UP    (do (shift-player player monsters dun (- (:pos @player) wide)) (center-camera))
		  Input$Keys/DOWN  (do (shift-player player monsters dun (+ (:pos @player) wide)) (center-camera))
      Input$Keys/LEFT  (do (shift-player player monsters dun (- (:pos @player) 1)) (center-camera))
      Input$Keys/RIGHT (do (shift-player player monsters dun (+ (:pos @player) 1)) (center-camera))
      Input$Keys/NUM_0 (do (damage-player player dun  1) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_1 (do (damage-player player dun 11) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_2 (do (damage-player player dun 22) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_3 (do (damage-player player dun 33) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_4 (do (damage-player player dun 44) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_5 (do (damage-player player dun 55) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_6 (do (damage-player player dun 66) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_7 (do (damage-player player dun 77) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_8 (do (damage-player player dun 88) (refresher) (update-fov dun) (center-camera))
      Input$Keys/NUM_9 (do (damage-player player dun 99) (refresher) (update-fov dun) (center-camera))
                                 true)
            true))
   (keyUp [this keycode] false)
   (keyTyped [this keycode] false)

   (touchDown [this x, y, pointer, button] false)
   (touchUp   [this x, y, pointer, button] false)
   (touchDragged [this x, y, pointer] false)
   (mouseMoved [this x, y] false)
   (scrolled [this amount] false)
))
  ;(refresher)
)
(comment (defn handle-input [^OrthographicCamera cam dun]
		(cond
		  (.isKeyPressed Gdx/input Input$Keys/UP)    (shift-player player monsters dun (- (:pos @player) wide))
		  (.isKeyPressed Gdx/input Input$Keys/DOWN)  (shift-player player monsters dun (+ (:pos @player) wide))
      (.isKeyPressed Gdx/input Input$Keys/LEFT)  (shift-player player monsters dun (- (:pos @player) 1))
      (.isKeyPressed Gdx/input Input$Keys/RIGHT) (shift-player player monsters dun (+ (:pos @player) 1))
      ;(.isKeyPressed Gdx/input Input$Keys/P)     (dorun (map #(print (.toString %) "  ") colors))
    )
))


(defn -render [^Float delta]
;  (handle-input ^OrthographicCamera camera dun)
  (refresher)
        ;(refresher)
  )
(defn -resize [^Game this ^Integer width ^Integer height]
  (reset! screen-width (float width))
  (reset! screen-height (float height))
  (.setToOrtho ^OrthographicCamera camera false @screen-width @screen-height)
  (reset! glViewport (Rectangle. 0.0 0.0 (float width) (float height)))
  (.setDisplayMode Gdx/graphics width height false)
  (refresher)
  (center-camera))

(defn -dispose [^Game this]
  (if (> (:hp @player) 0)
      (binding [*print-dup* true] (spit "Savefile.edn" (pr-str [wide high (:dungeon @dun) (:shown @dun) @cleared-levels @dlevel @player (mapv deref @monsters)])))
      (io/delete-file "Savefile.edn" true)))

;  (doto stage
 ;       (.act delta)
  ;      (.draw))





