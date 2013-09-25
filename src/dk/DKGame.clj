(ns dk.DKGame
  (:import (com.badlogic.gdx Game Screen Gdx))
  (:require (dk [dkscreen :as dkscreen])))

(gen-class
 :name dk.DKGame
 :extends com.badlogic.gdx.Game)

(defn -create [^Game this]
  (.setScreen this (dkscreen/screen)))
