(defproject dk "0.0.1"
  :description "2D Tactical RPG in Clojure"
  :url "https://github.com/tommyettinger/dungeon-kingpin"
  :license {:name "GPL License v2"
            :url "http://opensource.org/licenses/GPL-2.0"}
  :dependencies [
                 [org.clojure/clojure "1.5.1"]
                 [hiphip-aot "0.1.1"]
                 [seesaw "1.4.1"]
                 [com.squidpony/squidlib "1.95.1"]]
  :jvm-opts ^:replace []
  :aot [dk.herringbone dk.weapons dk.combat dk.core]
  :main dk.core
 )
