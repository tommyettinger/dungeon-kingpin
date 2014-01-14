(ns dk.spritecore
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication LwjglApplicationConfiguration]
           [java.util ArrayList])
  (:gen-class))
(set! *warn-on-reflection* true)

;(defn App[] (LwjglApplication. (dk.DKGame. ) "Dungeon Kingpin" 1200 640 false))
(defn App[] (LwjglApplication. (dk.DKGame. ) (let [cfg (LwjglApplicationConfiguration.)]
                                               (set! (.title cfg) "Dungeon Kingpin")
                                               (set! (.width cfg) 1200)
                                               (set! (.height cfg) 640)
                                               (set! (.useGL20 cfg) false)
                                               (set! (.foregroundFPS cfg) 60)
                                               (set! (.backgroundFPS cfg) 1)
                                               cfg)))

(defn -main [& args] ;(if (seq args)
  (App)
     ;                  (.waitFor (.start (ProcessBuilder. (ArrayList.
     ;         ["java" "-server" "-jar" "dungeon.jar" "ActuallyRun"])))))
  )
