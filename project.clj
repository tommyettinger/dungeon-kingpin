(defproject dk "0.0.1"
  :description "2D Tactical RPG in Clojure"
  :url "https://github.com/tommyettinger/dungeon-kingpin"
  :license {:name "GPL License v2"
            :url "http://opensource.org/licenses/GPL-2.0"}
  ;:resource-paths ["devlib" "resources"]
  :repositories [["libgdx" "https://oss.sonatype.org/content/repositories/snapshots/"]]
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [com.badlogicgames.gdx/gdx "1.0-SNAPSHOT"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.0-SNAPSHOT"]
		             [com.badlogicgames.gdx/gdx-platform "1.0-SNAPSHOT" :classifier "natives-desktop"]
                 [hiphip-aot "0.1.2"]
               ;  [seesaw "1.4.1"]
                 [com.squid/cuttlebone "1.95.1"]
                 ]
  :jvm-opts ^:replace ["-Xshare:off" "-server"]
  :uberjar-name "dungeon.jar"
;  :aot [dk.herringbone dk.weapons dk.combat dk.core]
  :aot [dk.herringbone dk.core dk.DKGame dk.dkscreen dk.spritecore]
  :main dk.spritecore
 )






