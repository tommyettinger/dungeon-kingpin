(ns dk.dkscreen
  (:import (com.badlogic.gdx Gdx Screen Files)
           (com.badlogic.gdx.scenes.scene2d Stage)
           (com.badlogic.gdx.scenes.scene2d.ui Label Label$LabelStyle Image)
           (com.badlogic.gdx.graphics Color)
           (com.badlogic.gdx.graphics.g2d BitmapFont TextureAtlas)))

(declare ^Stage stage)

(defn screen []
  (proxy [Screen] []
    (show []
      (def stage (Stage.))
      (def packed (TextureAtlas. (.internal Gdx/files "slashem-packed/pack.atlas")))
      (let [font (BitmapFont.)
            style (Label$LabelStyle. font (Color. 1.0 1.0 1.0 1.0))
            label (Label. "Hello Clojure!" style)
            sprite (Image. (.createSprite packed "monster-angelic being-solar"))]
            (.addActor stage label)
            (.addActor stage sprite)
            (.translate sprite 240.0 128.0)))

    (render [delta]
      (doto stage
        (.act delta)
        (.draw)))

    (dispose[])
    (hide [])
    (pause [])
    (resize [w h])
    (resume [])))

