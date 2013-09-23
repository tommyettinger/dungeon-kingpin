(ns dk.combat
  (:use dk.weapons))

(def dungeon (atom {}))
(def wide (atom 62))
(def monsters (atom {}))
(def heroes (atom {}))
(def items (atom {}))
(def wall (atom 9999.0))

(defn init-combat [dun w mon hero item wl]
  (reset! dungeon @dun)
  (reset! wide @w)
  (reset! monsters @mon)
  (reset! heroes @hero)
  (reset! items @item)
  (reset! wall @wl))

(def stats {
:armor {:stat "Armor", :description "Your equipment's durability; your clothing's ability to hide your movement; how attentive you are to what you wear.", :effect "Flaw Defense", :effect-meaning "Having your weaknesses exposed less often."}
:brawn {:stat "Brawn", :description "Your physical strength; how much you can carry; your familiarity with how to damage objects or people.", :effect "Damage Offense", :effect-meaning "Dealing more damage."}
:caution {:stat "Caution", :description "Your wisdom; your judgment of how people's actions will play out; how careful you are in dangerous situations.", :effect "Boost Defense", :effect-meaning "Raising one ally's defense."}
:deception {:stat "Deception", :description "Your cunning; how well you can confuse or lie to people; your ability to sneak or hide.", :effect "Trick Offense", :effect-meaning "Tricking enemies to make them lose bonuses."}
:endurance {:stat "Endurance", :description "Your bodily toughness; your general healthiness; how well you pick yourself up after getting hurt.", :effect "Damage Defense", :effect-meaning "Taking less damage."}
:finesse {:stat "Finesse", :description "Your manual dexterity; how sure-footed you are; your physical performance on delicate tasks.", :effect "Accuracy Offense", :effect-meaning "Hitting more often."}
:insight {:stat "Insight", :description "Your social awareness; your ability to understand or analyze the behavior of people; how well you can sense lies.", :effect "Trick Defense", :effect-meaning "Resisting the loss of bonuses from tricks."}
:knowledge {:stat "Knowledge", :description "Your academic intelligence; how well you can figure out puzzles; your skill with machines and devices.", :effect "Boost Offense", :effect-meaning "Raising one ally's offense."}
:persuasion {:stat "Persuasion", :description "Your ability to make friends; your perceived likability; how well you can inspire people to do their best.", :effect "Group Offense", :effect-meaning "Making nearby allies have slightly higher offense."}
:quickness {:stat "Quickness", :description "Your speed and agility; your reaction time; how fast you can run to or from danger.", :effect "Accuracy Defense", :effect-meaning "Being hit less often."}
:senses {:stat "Senses", :description "Your quality of sight and hearing; your ability to notice hidden things; how alert you are.", :effect "Flaw Offense", :effect-meaning "Exposing weaknesses in enemies more often."}
:tenacity {:stat "Tenacity", :description "Your willpower; your refusal to change course against your wishes; how intimidating you can be.", :effect "Group Defense", :effect-meaning "Making nearby allies have slightly higher defense."}
})

; Armor    Endurance   Finesse     Quickness   Senses   Knowledge   Brawn    Finesse     Brawn     Brawn     Finesse     Deception   Finesse     Tenacity    Finesse       Tenacity
; Block    Counter     Disarm      Trip        Aim      Guide       Pin      Bounce      Grab      Push      Pull        Distract    Bonk        Threat      Distant Bonk  Distant Threat
(def maneuvers {
:block {:maneuver "Block" :stat :armor :opposed :finesse :trigger :guard :difficulty 7 :effect :blocking}
:counter {:maneuver "Counter" :stat :endurance :opposed :quickness :trigger :guard :difficulty 9 :effect :riposte}
:disarm {:maneuver "Disarm" :stat :finesse :opposed :armor :trigger :major :difficulty 9 :effect :disarmed}
:trip {:maneuver "Trip" :stat :quickness :opposed :finesse :trigger :major :difficulty 6 :effect :prone}
:aim {:maneuver "Aim" :stat :senses :opposed :deception :trigger :minor :difficulty 4 :effect :locked}
:guide {:maneuver "Guide" :stat :knowledge :opposed :finesse :trigger :minor :difficulty 7 :effect :exposed}
:pin {:maneuver "Pin" :stat :brawn :opposed :armor :trigger :attack :difficulty 10 :effect :pinned}
:bounce {:maneuver "Bounce" :stat :finesse :opposed :quickness :trigger :attack :difficulty 10 :effect :repeat}
:grab {:maneuver "Grab" :stat :brawn :opposed :brawn :trigger :major :difficulty 6 :effect :pinned}
:push {:maneuver "Push" :stat :brawn :opposed :endurance :trigger :minor :difficulty 5 :effect :knockback}
:pull {:maneuver "Pull" :stat :finesse :opposed :endurance :trigger :minor :difficulty 5 :effect :knockfront}
:distract {:maneuver "Distract" :stat :deception :opposed :insight :trigger :major :difficulty 4 :effect :confused}
:bonk {:maneuver "Bonk" :stat :finesse :opposed :endurance :trigger :major :difficulty 9 :effect :unconscious}
:threat {:maneuver "Threat" :stat :tenacity :opposed :tenacity :trigger :major :difficulty 9 :effect :surrender}
:distant-bonk {:maneuver "Distant Bonk" :stat :finesse :opposed :endurance :trigger :major :difficulty 10 :effect :unconscious}
:distant-threat {:maneuver "Distant Threat" :stat :tenacity :opposed :tenacity :trigger :major :difficulty 10 :effect :surrender}
})
(defn move [pos nswe]
  nil
;  (condp = nswe
;    :n (if (and
;            (>= (- pos wide) 0)
;            (not= (aget ^doubles (:dungeon @dungeon) (- pos wide)) @wall))
;         )
;    )
  )
(defn spawn-item [item pos] nil)
(defn kill [enemy] nil)
(defn attack [user enemy] nil)
(def status-effects {
:blocking (fn [user enemy] (swap! user update-in [:temp :damage-defense] (partial + 5)))
:riposte (fn [user enemy] (attack user enemy))
:disarmed (fn [user enemy] (swap! enemy #(when (:uses-weapons %) (do (spawn-item (:held-weapon %) (:pos %)) (assoc % :held-weapon nil)))))
:locked (fn [user enemy] (swap! user update-in [:temp :flaw-offense] (partial + 3)))
:prone (fn [user enemy] (swap! enemy update-in [:temp :damage-defense] (partial - 5)))
:exposed (fn [user enemy] (swap! enemy update-in [:temp :flaw-defense] (partial - 5)))
:pinned (fn [user enemy] (swap! enemy update-in [:temp :accuracy-defense] (partial - 5)))
:confused (fn [user enemy] (swap! enemy update-in [:temp :flaw-offense] (partial - 3)))
:repeat (fn [user enemy] (attack user enemy))
:knockback (fn [user enemy] (swap! enemy assoc :pos (move (:pos @enemy)
                                                          (first (last (sort-by #(second %1) [
                                                                      [:n (- (quot (:pos @user) @wide) (quot (:pos @enemy) @wide))]
                                                                      [:s (- (quot (:pos @enemy) @wide) (quot (:pos @user) @wide))]
                                                                      [:w (- (mod (:pos @user) @wide) (mod (:pos @enemy) @wide))]
                                                                      [:e (- (mod (:pos @enemy) @wide) (mod (:pos @user) @wide))]]))))))
:knockfront (fn [user enemy] (swap! enemy assoc :pos (move (:pos @enemy)
                                                           (first (first (sort-by #(second %1) [
                                                                      [:n (- (quot (:pos @user) @wide) (quot (:pos @enemy) @wide))]
                                                                      [:s (- (quot (:pos @enemy) @wide) (quot (:pos @user) @wide))]
                                                                      [:w (- (mod (:pos @user) @wide) (mod (:pos @enemy) @wide))]
                                                                      [:e (- (mod (:pos @enemy) @wide) (mod (:pos @user) @wide))]]))))))
:unconscious (fn [user enemy] (kill enemy))
:surrender (fn [user enemy] (swap! enemy assoc :ai :flee))
                     })
(defn maneuver-roll [mnvr user enemy] (> (+
                                            (get-in @user [:stats ((maneuvers :mnvr) :stat)])
                                            1
                                            (rand-int 10))
                                         (+ ((maneuvers :mnvr) :difficulty) (get-in @enemy [:stats ((maneuvers :mnvr) :opposed)]))))







