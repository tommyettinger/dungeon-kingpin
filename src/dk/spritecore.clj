(ns dk.spritecore
  (:import (com.badlogic.gdx.backends.lwjgl LwjglApplication))
  (:gen-class))
(set! *warn-on-reflection* true)

(defn App[] (LwjglApplication. (dk.DKGame. ) "Dungeon Kingpin" 1200 640 false))

(defn -main [& args] (App))


