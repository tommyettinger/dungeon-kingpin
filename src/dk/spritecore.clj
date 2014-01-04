(ns dk.spritecore
  (:import (com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration))
  (:gen-class))
(set! *warn-on-reflection* true)

;(defn App[] (LwjglApplication. (dk.DKGame. ) "Dungeon Kingpin" 1200 640 false))
(defn App[] (LwjglApplication. (dk.DKGame. ) (let [cfg (LwjglApplicationConfiguration.)]
                                               (set! (.title cfg) "Dungeon Kingpin")
                                               (set! (.width cfg) 1200)
                                               (set! (.height cfg) 640)
                                               (set! (.useGL20 cfg) false)
                                               (set! (.foregroundFPS cfg) 15)
                                               (set! (.backgroundFPS cfg) 1)
                                               cfg)))

(defn -main [& args] (App))




