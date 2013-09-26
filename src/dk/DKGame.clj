(ns dk.DKGame
  (:use dk.core)
  (:import (com.badlogic.gdx Game Screen Gdx Files Input Input$Keys)
           (com.badlogic.gdx.math Rectangle Vector3)
           (com.badlogic.gdx.scenes.scene2d Stage)
           (com.badlogic.gdx.scenes.scene2d.ui Label Label$LabelStyle Image)
           (com.badlogic.gdx.graphics Color GL10 OrthographicCamera)
           (com.badlogic.gdx.graphics.g2d BitmapFont TextureAtlas TextureAtlas$AtlasSprite TextureAtlas$AtlasRegion SpriteBatch))
  ;(:require (dk [dkscreen :as dkscreen]))
  )
(set! *warn-on-reflection* true)

(gen-class
 :name dk.DKGame
 :extends com.badlogic.gdx.Game)



(declare sv) (declare sh) ; straight vert/horiz
(declare gr) (declare da) ; ground/dark
(declare ul) (declare ur) ; up left/right (pointing these ways)
(declare dl) (declare dr) ; down left/right
(declare cw) ; crosswall
(declare Tu) (declare Td) (declare Tl) (declare Tr) ; T-shaped up/down/left/right

(defn handle-input [^OrthographicCamera cam]

		(when (.isKeyPressed Gdx/input Input$Keys/LEFT)
			;(when (> (.x (.position cam)) 0.0)
				(.translate cam -4.0 0.0 0.0))
		(when (.isKeyPressed Gdx/input Input$Keys/RIGHT)
			;(when (< (.x (.position cam)) 1024.0)
				(.translate cam 4.0 0.0 0.0))
		(when (.isKeyPressed Gdx/input Input$Keys/DOWN)
				(.translate cam 0.0 -4.0 0.0))
		(when (.isKeyPressed Gdx/input Input$Keys/UP)
				(.translate cam 0.0 4.0 0.0))
		)

(defn -create [^Game this]
   (def ^OrthographicCamera camera (OrthographicCamera.))
   (.setToOrtho ^OrthographicCamera camera false 1200.0 640.0 )
   (.set ^Vector3 (.position ^OrthographicCamera camera) 2600.0 -1600.0 0.0);

   (def glViewport (Rectangle. 0.0 0.0 1200.0 640.0))
  ; (.setScreen this (dkscreen/screen))

;   (def stage (Stage.))
   (def ^TextureAtlas packed (TextureAtlas. (.internal ^Files Gdx/files "slashem-sepia/pack.atlas")))
   (def ^SpriteBatch batch (SpriteBatch.))
   (defn ^TextureAtlas$AtlasSprite grab [sprite] (TextureAtlas$AtlasSprite. ^TextureAtlas$AtlasRegion (.findRegion ^TextureAtlas packed sprite)))

   (def dragon (grab "monster-dragon-ixoth"))
   (def nucky (grab "monster-human or elf-chief yeoman warder"))
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

 (defn clean-bones [^doubles dd]
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
          :else gr
          ))
      gr)) (vec dd)))

  (comment
   (def normal-dungeon {:sv (grab "cmap-wall-vertical") :sh (grab "cmap-wall-horizontal") :gr (grab "cmap-floor of a room") :da (grab "cmap-dark part of a room")
                        :ul (grab "cmap-wall-bottom right corner") :ur (grab "cmap-wall-bottom left corner") :dl (grab "cmap-wall-top right corner") :dr (grab "cmap-wall-top left corner")
                        :cw (grab "cmap-wall-crosswall")
                        :Tu (grab "cmap-wall-tee up") :Td (grab "cmap-wall-tee down") :Tl (grab "cmap-wall-tee left") :Tr (grab "cmap-wall-tee right")}))

   (let [;{:keys [sv sh gr da ul ur dl dr cw Tu Td Tl Tr]} normal-dungeon
         dd0 (prepare-bones)]
         (def dd (first dd0))
         (def colors (mapv (fn [eh] (Color. (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) (- 1.0 (* 0.0007 (rand-int 50))) 1.0)) (range (* wide high))))
         (def cleaned (clean-bones dd))
         (def player-calc  (init-dungeon dd player))
         (def monster-calc (doall (map #(init-dungeon dd %) @monsters)))
         (def monster-hash (into {} (map (fn [entry] [(:pos @entry) entry]) @monsters)))
         )

   (comment (let [font (BitmapFont.)
            style (Label$LabelStyle. font (Color. 1.0 1.0 1.0 1.0))
            label (Label. "Hello Clojure!" style)
            sprite (Image. (.createSprite packed "monster-angelic being-solar"))]
            (.addActor stage label)
            (.addActor stage sprite)
            (.translate sprite 240.0 128.0)))
  )

(defn -render [^Float delta]
  (handle-input ^OrthographicCamera camera)
  (let [gl (.getGL10 Gdx/graphics)]
	(.glClear gl GL10/GL_COLOR_BUFFER_BIT);

;  (.glClearColor gl 0.0, 0.0, 0.0, 1.0)
  (.glViewport gl (int (.x ^Rectangle glViewport)) (int (.y ^Rectangle glViewport)) (int (.width ^Rectangle glViewport)), (int (.height ^Rectangle glViewport)))
  (.update ^OrthographicCamera camera)
  (.apply ^OrthographicCamera camera gl)

  (.setProjectionMatrix ^SpriteBatch batch (.combined ^OrthographicCamera camera))
  ;(let
   ; [{:keys [sv sh gr da ul ur dl dr cw Tu Td Tl Tr]} normal-dungeon]
  (.begin ^SpriteBatch batch)
  (dorun (map-indexed #(do (.setColor ^SpriteBatch batch ^Color (nth colors %1)) ; ^TextureAtlas$AtlasSprite %2
                         (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite %2 (+ (* 32.0 (mod %1 wide)) (* 16 (- wide (quot %1 wide)))) (- 640.0 64 (* 32 (quot %1 wide))))
                         (.setColor ^SpriteBatch batch ^Color Color/WHITE)
                         (when (contains? monster-hash %1) (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite dragon (+ (* 32.0 (mod %1 wide)) (* 16 (- wide (quot %1 wide)))) (- 640.0 64 (* 32 (quot %1 wide)))))
                         (when (= %1 (:pos @player)) (.draw ^SpriteBatch batch ^TextureAtlas$AtlasSprite nucky (+ (* 32.0 (mod %1 wide)) (* 16 (- wide (quot %1 wide)))) (- 640.0 64 (* 32 (quot %1 wide)))))
                         ) cleaned)) ; (float 48.0) (float 64.0)
  (.end ^SpriteBatch batch)))



;  (doto stage
 ;       (.act delta)
  ;      (.draw))

