(ns dk.spritecore
  (:import (com.badlogic.gdx.backends.lwjgl LwjglApplication)))
(set! *warn-on-reflection* true)

(defn App[]
  (LwjglApplication. (dk.DKGame. ) "Dungeon Kingpin" 800 480 true))

(defn -main [& args] (App))

