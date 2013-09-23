(ns dk.herringbone)

(def filler [
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
   "##########"
             ])

(def horiz-testing [
  [
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   "%%%%%%%%%%%%%%%%%%%%"
   ]
  [
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   "$$$$$$$$$$$$$$$$$$$$"
   ]
  [
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   "~~~~~~~~~~~~~~~~~~~~"
   ]
  [
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   "++++++++++++++++++++"
   ]
            ])
(def horiz [
  ["##......%####.....##"
   "###.......###.....##"
   "..##......###%##...."
   "...#####..##........"
   "...#####..##........"
   "...#####..##....##.."
   "....#%....##..#%##.."
   "..........##....####"
   "##.....#####.....###"
   "##.....#####......##"]

  ["##..##%######.....##"
   "##..#$~$##%##.....%#"
   "##..#~~~#...#......."
   "##..#~~~#...####...."
   "##..#$~$#...#~~#...."
   "##..%~~~%...%~~%...."
   "...................."
   "..................##"
   "####..######......##"
   "####..######..######"]

  ["##......##%##.....##"
   "#%......#$~$#.....##"
   "........#~~~#......."
   "........#~~~#......."
   "...#..###~~~####..##"
   "...#..###$~~%###..##"
   "...#..###~~~$$~~~~##"
   "...#..###~~~~~~~~~##"
   "####..###$~~~~~$~$##"
   "####..######..######"]

  ["##..####%####.....##"
   "##..###$~~$%....####"
   "##....#$~~~........."
   "##....#~~~~........."
   "####..#$~~$#..###%##"
   "###%..######..%$~~$#"
   "......#........~~~$#"
   "......#........~~~~#"
   "####..######..#$~~$#"
   "####..######..######"]

  ["##..##%###%###%#..##"
   "##................##"
   ".##................."
   "..######..######...."
   "...##%##..#%####...."
   ".............####..."
   "...............%##.."
   "......#####.....####"
   "####..######.....###"
   "####..######......##"]

  ["##......##........##"
   "##......##%#......%#"
   "##..##%##~~~~~~#...."
   "##..~~~$#~~~~~~#...."
   "##..~~~$#~~###%#...."
   "##..#%###~~#........"
   "........#~~#........"
   "........#$$#..##...#"
   "#####..#####..##...#"
   "##.....#####..######"]

  ["##..####%#######..##"
   "#%..#$~$~~$~~$##..##"
   "....#~~~~~~~~##....."
   "....#~~~~~~~##......"
   "....#$~~~~$##%...###"
   "....###..####...####"
   "...............#####"
   "..............######"
   "####..######..######"
   "####..######..######"]

  ["##..#####%######..##"
   "##..###$~~~$%###..##"
   "##..##$.....~$##...."
   "##..##$......~##...."
   "##..###~.....$##..##"
   "#%...###~~~~$###..##"
   "......##..###%##..##"
   ".......#..........##"
   "##.....#..........##"
   "##.....#####..######"]

  ["###..########.....##"
   "#..................#"
   "...................."
   ".....########......."
   "...#############...."
   "...%###########%...."
   ".....%######%......."
   "...................#"
   "#..................#"
   "####..######......##"]

  ["##......#%###.....##"
   "###....##~+##......#"
   "###.....~~+##......."
   "##......~~+##......."
   "##...########......."
   "##..%####%###......."
   "......##$~~##......#"
   "......##$~~~.......#"
   "####..##$~~~.......#"
   "####..######..######"]

  ["##..####%#%##.....##"
   "#...##~$~~~$#......#"
   "....##$~~~~~#......."
   "....##~~~~~~........"
   "....##$~~~~~.....###"
   "....####~~~~########"
   "....%###$~~~%...####"
   "......##~~~~....####"
   "#.....##$~~~......##"
   "####..######......##"]

  ["##......#%###.....##"
   "###........##......#"
   "####.......##..#...."
   "########...##..##..."
   "######%#...##..#####"
   "##%###....##...%####"
   "...##....##%....####"
   "........###......###"
   "#......###........##"
   "##.....#####......##"]

  ["#%..#########%##..##"
   "#....########......#"
   "......%####%........"
   ".......####........."
   "........##....###..."
   "..##....##....####.."
   "..##....##....#####."
   ".###...####...######"
   "####...####...######"
   "####..######..######"]

  ["##......####%###..##"
   "#.......##$~~~$#...#"
   "#.......##$~~~~#...."
   "#.......##~~~~~....."
   "#..#######~~~~~....."
   "#..##%####..%###...."
   "..............##...."
   "..............######"
   "####..######..######"
   "####..######..######"]

  ["##......########..##"
   "###.....######%#...#"
   ".###...######......."
   "..#%..######........"
   "......####.....#####"
   "......####...#%#++##"
   "..##..%##.........+#"
   ".###...##..........#"
   "###....##.........+#"
   "##.....#####......##"]

  ["##..############..##"
   "##..###%#####%##..%#"
   "#$~~~~$~$###........"
   "#~~~~~~~~###........"
   "#$~~~~~~$###..######"
   "###%#...####..######"
   ".........###..######"
   ".........###..######"
   "##.......###..######"
   "##.....#####..######"]

  ["#%..####%###%.....##"
   "#+..###............%"
   "#...+%#............."
   ".....+#..#####%....."
   "......#..#+........."
   "......#..#+........."
   "......#..%##%##...#."
   "......#.......#...##"
   "#+....#.......#...##"
   "####..######..######"]

  ["#%......##%##.....%#"
   "#...........##.....#"
   "#...........###....."
   "#....#####..###....."
   "#....####%..###....."
   "#....###+~~~###....."
   ".....###~~~~###....."
   ".....%##++~+#%.....#"
   "#.....#######......#"
   "##.....#####......##"]

  ["#%..#%#######.....%#"
   "#.......+####......#"
   "#........+###......."
   "..........##%......."
   "....###...##+......#"
   "...####...##+......#"
   "....#%#...##.......#"
   ".........+##.......#"
   "#.......+###.......#"
   "##.....#####..######"]

  ["##......#####.....##"
   "##......%###%......#"
   "##+................."
   "##++................"
   "#####%######..%#####"
   "##$~~~~~.~~~~~~++###"
   "..~~~~~~.~~~~~~~~###"
   "..~~~~~~#~~~~~~~~###"
   "##$~~~~$#+~~~~~++###"
   "####..######..######"]

  ["##..######%#####..##"
   "##..##.........%..%#"
   "#%..##.............."
   "....##..####........"
   "....%#..####........"
   "........####+......."
   "........####....++.."
   "....%#######..######"
   "##.....#####..######"
   "##.....#####..######"]

  ["##......%#######..##"
   "##......~~~~$$##..%#"
   "#####...~~~~~$##...."
   "####%...~~~~$$##...."
   "#.......#######%...."
   "#..................."
   "...................."
   ".....%######......##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##..############..##"
   "#%..%####%######..%#"
   "......#$$~$#........"
   "......#$~~$#........"
   "......#~~~~#..%#####"
   "......#~~~~#....++.."
   "......#~~~~%........"
   "......#$~~~...#....."
   "####..#$$~~...#....."
   "####..######..#....."]

  ["##......########..##"
   "##......#######%..%#"
   "##.....+#..........."
   "##....++#..........."
   "##..############..##"
   "#%..%######%###%..##"
   "..................##"
   "..................##"
   "####..######.....+##"
   "####..######......##"]

  ["###..#######%.....##"
   "#.......####$~~~~~~%"
   "#........###$.....~."
   "...#......##~.~~~.~."
   "...#..#...##~.~.~.~."
   "...#.....###~.~.~.~."
   ".........###~.~~~.~."
   ".....#..####~.....$#"
   "#......#####~~~$~$$#"
   "##.....#####..######"]

  ["#%......#####.....##"
   "#........#%..~~~~..%"
   "#........#..~~~~~~.."
   "#......###.~~##~~#~."
   "#..%#####..~%#~~~#~."
   "#.....#....~~#%~~%~."
   "......%....~##~~~~~."
   ".........#..~~~~~~.#"
   "#........#...~~~~..#"
   "####..######......##"]

  ["##..%#######%.....##"
   "#%................%#"
   "...................."
   ".....##..%###......."
   "....##......########"
   "...##..........#####"
   "..##............%###"
   "..#....##........###"
   "##%....#####......##"
   "##.....#####......##"]

  ["##......###%#.....##"
   "##......#.........%#"
   "##...####..........."
   "##....%########....."
   "##............#...##"
   "#%............#..###"
   "..........##..%..%##"
   "..........##......##"
   "####..######......##"
   "####..######......##"]

  ["##......########..##"
   "#.......#####......#"
   "#....%...###........"
   ".........##........."
   ".........##...##...."
   "...%.....##...#....."
   ".....#...##........."
   "........###....#...#"
   "#......#####.......#"
   "####..######......##"]

  ["#%..###########%..%#"
   "#~~~~~#~~~~~.......#"
   "#~~~~~#~~~~~........"
   "#~~~~~%~~~~~#......."
   "#~~~~~.~~~~~#......."
   "#~~~~~.~~~~~#......."
   "...#########%......."
   "...#...............#"
   "####...............#"
   "####..######..######"]

  ["##..############..##"
   "#%..######%#####..%#"
   "....####~~~~~###...."
   "....#%##~~~~~###...."
   ".......#~~~~~###..##"
   ".......#~~~~~##%..##"
   ".......#~~~~~.....##"
   ".......#~~~~~.....##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##......###%####..##"
   "##......#$$~$$##..%#"
   "##......#$~~~$##...."
   "###..###%~~~~~##...."
   "###..#$$~~~~~~##..##"
   "##%..#$~~~~~~~##..##"
   ".....%###~~~~~#%..##"
   ".......##$~~~~....##"
   "##.....##$$~~~....##"
   "##.....#####..######"]

  ["##..########%.....##"
   "##..####+.........%#"
   "#%..####+..........."
   "....####...#%###...."
   "....####...~~~$#...."
   "....%###...~~~$#...."
   "......##...####%...."
   "......##+.........##"
   "####..##+.........##"
   "####..######......##"]

  ["##......####%.....##"
   "##......#.........%#"
   "##......#..........."
   "##+....+#..####....."
   "###....##..####+...."
   "##%#..%##..##%#....."
   "...........#........"
   "...........#.......#"
   "####..######..######"
   "####..######..######"]

  ["#%..#####%###%....##"
   "#~~~~~$~$~~#......##"
   ".~~~~~~~~~$#........"
   ".~~%~~~~%~$#........"
   ".~~~~~~~~~~#..#%####"
   ".~~~~~+~~~$#.......#"
   ".~~%~~~~%~~#.......#"
   ".~~~~~~~~~~#.......#"
   "#$$~~~~~$$~#.......#"
   "####..######..######"]

  ["##..###%##%##.....##"
   "##..#+~++#++#.....%#"
   "##..#+~~~#..#......."
   "##..#~~~~#..#......."
   "##..#+~~~#..#......#"
   "#%..#%#..#..#......#"
   ".........#..%####..#"
   ".........#.........#"
   "##.......#.........#"
   "##.....#####..######"]

  ["##......########..##"
   "##......#####%##..%#"
   "##%##...%##++~~~~~~~"
   "..++#.....#+~~~~~~~~"
   "...+#.....#~~~~~~~~~"
   "....####..#+~~~~~~~~"
   "....##%#..#++~~~~~~~"
   "..........#####%..##"
   "##........##......##"
   "##.....#####......##"]

  ["##......########..##"
   "##......%######%..##"
   "##+........#........"
   "##++.......#........"
   "#########..#..#....."
   "##%%##%##..#..#+...."
   "...........#..#++..."
   "...........#..##%###"
   "####..######......##"
   "####..######......##"]

  ["##..############..##"
   "#%..#%##%%####%#..%#"
   "#~~~~~++~+~+~++#...."
   ".~~~~~~~~~~~~~+#...."
   ".~~#........#~~#..##"
   ".~~..........~~...##"
   ".~~#........#~~...##"
   ".~~~~~~~~~~~~~~#..##"
   "#++~~~~+~++~~~+#..##"
   "####..######..######"]

  ["##..###########%..##"
   "##..#####%........%#"
   "##..####+..........."
   "##..####+...##%##..."
   "##..######..~~~$#..#"
   "#%..#%####..~~~$#..#"
   "......##+...#~$$#..#"
   "......##+...##%##..#"
   "####..####.........#"
   "####..#####.......##"]

  ["##......#####.....##"
   "###.....#####.....##"
   "..##.....###.....##."
   "...##...........##.."
   "....###........##..."
   ".....####..#####...."
   "......###..####....."
   "...................#"
   "##................##"
   "##.....#####..######"]

  ["##..#########.....##"
   "#$~~~~~~~~~$#.....##"
   "#$~~~~~~~~~$#......."
   "###..########..#...."
   "#.....#+~~~....#...."
   "#.....#~~~~....#...."
   "...#..#~~~~#........"
   "...#..#+~~~#......##"
   "####..#+~++#......##"
   "##....######......##"]

  ["##......#####.....##"
   "##..............####"
   "................#..."
   "......###########..."
   "..##...............#"
   "..##...............#"
   "......##############"
   "...................#"
   "#..................#"
   "####..#######..#####"]

  ["###..########.....##"
   "#....#+............#"
   "#....#+............."
   "#..#############...."
   "#..#...............#"
   "#..#...............#"
   "......##########...#"
   "......#+...........#"
   "#++#..#+...........#"
   "####..######......##"]

  ["##......########..##"
   "...................."
   "...................."
   "..###############..."
   "..##................"
   "..##................"
   "..##..###########..."
   "..##..#+...........#"
   "####..#+...........#"
   "####..######..######"]

  ["###..###########..##"
   "###..###..........##"
   "###..###............"
   "###..###..##...#...."
   "###..###..##+......."
   "###..###..##+......."
   "..........##........"
   "..........##..#...##"
   "##.....#####......##"
   "##.....#####......##"]

  ["##..############..##"
   "##....~~~~~~~++#..##"
   "......~~~~~~~~+#...."
   "....##~~~~#~~~~#...."
   "...###~~~~~~~~+#..##"
   "...###+~~~~~+~+#..##"
   "...#####...#####..##"
   "...#.......#......##"
   "####.......#......##"
   "####..######..######"]

  ["##......###%####..##"
   "##.~~~~.#.....##..%#"
   "##.~##~.#.....##...."
   "##.~+.~.%..#..##...."
   "##.~.+~....#..##..##"
   "#%.~%%~....#..##..##"
   "...~~~~.#..#..##..##"
   "........#..#..#%..##"
   "####....#..#......##"
   "####..######......##"]

  ["##......###%####..##"
   "##..#####..~~++#..%#"
   "##..%......~~~+#...."
   "##.........#~~~%...."
   "##......#~~#~~~~...."
   "#%..####%~~#+~~~...."
   "......#$~~~#++~#...."
   "......#$~~~#####..##"
   "####..#$~$$#......##"
   "####..######......##"]

  ["##......#####.....##"
   "#.......#++#%.....%#"
   "#...####%~~#........"
   "#...%~~+~~+#........"
   "#....~~~~~~#..##...#"
   "%....~~~~~+#..#...##"
   "....#~+~~~~#.....###"
   "....####~~+#....####"
   "##.....#~++#...#####"
   "##.....#####..######"]

  ["##......%####.....##"
   "##........#.......%#"
   "##........#....#...."
   "####%..#..#..##%...."
   "##++~~~#..#..#+....."
   "#%+~~~~#..%..#+....."
   "..~~~~~#.....#+....."
   "..~~~~~#.....#+...##"
   "#####..######%##..##"
   "##.....#####......##"]

  ["#%......##%#####..##"
   "#+......#.....#...%#"
   "#.......#.....#....."
   "#...#...#..#..#....."
   "#...#+..#..#..#....#"
   "#...#...%..#..%#%..#"
   "....#+.....#..~~~~~#"
   "....#+.....#..~~~~~#"
   "##..%###+.+#..~~~$$#"
   "##.....#####..######"]

  ["#%.....#####%###..##"
   "#......#++#.......%#"
   "#......#~~#........."
   "#....##%~~#........."
   "#....#+~~~~..#######"
   "#....#+~~~~...#+.+##"
   ".....#+~~~#...#+.+##"
   ".....%#~~~#...%...##"
   "##.......+#.......##"
   "##.......###......##"]

  ["###..########.....##"
   "#%...###%####%....%#"
   "#....#.............."
   "#...##.............."
   "#..###..#..#..##..##"
   "#..%##..#++#..##..##"
   "....+...%###..%#..##"
   "..................##"
   "##................##"
   "##.....#####..######"]

  ["#%~~~~~######%##..##"
   "#+~~~~~+#......#..%#"
   "#~~~~~~~%..........."
   "#+~~~~~~...#........"
   "##+~~~~~...#++.#...."
   "#####%##########...."
   "..............##...."
   "..............#%..##"
   "####..######......##"
   "####..######......##"]

  ["##..#%#######.....%#"
   "#......#####%......#"
   ".........###........"
   ".........###....#..."
   "...##.....%.....##.."
   "....%##.........##.."
   "......##........###."
   ".......##......#####"
   "##.....####....#####"
   "##.....#####..######"]

  ["##......%####.....##"
   "##......~~++#.....%#"
   "#####%..~~~+##......"
   "........~~++##......"
   "........###%####%..."
   "...#%####$$~~~......"
   "......++#$~~~~......"
   ".......+#~~~~~######"
   "##.....+#$$~~~######"
   "##.....#####..######"]

  ["##......%####%##..##"
   "##..........~~$#..%#"
   "##+.........~~$#...."
   "##.....+.+.#~~$#...."
   "##+...######~~$#..##"
   "#%....#+++~####%..##"
   "......#~~~~%......##"
   "......#~~~~.......##"
   "####..#+++~.......##"
   "####..######......##"]

  ["##......###%##....##"
   "#.......#$$~$#....##"
   "#...%#..#$~~~##%..%."
   "#....######~~#......"
   "#....#++~~#~~......."
   "##%..#+~~~#~~......."
   ".....###~~%#..####.."
   ".......#~~......+###"
   "##%....#~~......+###"
   "##.....#####..######"]

  ["##......###%####..##"
   "##.......~~~~~$#..%#"
   "##%##....~~~~~$#...."
   "....%...#####$$#...."
   "........##++####..##"
   "........##+....#..##"
   "....#####%##...#..##"
   "..........+#...%..##"
   "##........+#......##"
   "##.....#####......##"]

  ["#%.....#%###%#....%#"
   "#.......+#+........#"
   "#+.~~~~~.%.~~~~~...."
   "%..~~~~~...~~~~~...."
   "...~~~~~...~~~~~...."
   "...~~~~~...~~~~~.%##"
   "..................+#"
   "......#....#......+#"
   "#+....#++++#......+#"
   "###...######.....###"]

  ["##......%#######..##"
   "##................##"
   ".###..............%."
   ".%###%#%#%#%#%##...."
   "..............+#...."
   "...............%...."
   ".#.....~~~~........."
   ".#+...#~~~~#......##"
   "##+...#++++#.....###"
   "####..######......##"]

  ["#%..#####%%#####..%#"
   "#..................%"
   "#..................."
   "#..###%#####..%##..."
   "#..##+~~~~~~~~+##..#"
   "...##+~~~~~~~~+##..#"
   "...%##..#%#####%%..#"
   "...................#"
   "#..................#"
   "####..######..######"]

  ["##..%###########..##"
   "##....##########...%"
   "#......###..###%...."
   "##.....#......#....."
   "###................#"
   "#%.................#"
   "...................#"
   "......#....#..#...##"
   "#..#..##..##..##.###"
   "####..######..######"]

 ])

 (def vert [["####..####"
   "####..%#%#"
   "#%#%......"
   ".........."
   ".........."
   "..###%%..."
   "..#.+....."
   "..#......."
   "###+.....#"
   "######+..#"
   "######..+#"
   "##%###...#"
   "....##+.+#"
   "....###%##"
   "##..###%##"
   "##..#%...."
   "##........"
   "##......##"
   "######..##"
   "######..##"]

  ["##.....###"
   "###%..%###"
   "###....###"
   "###....###"
   "###%..####"
   "......%###"
   ".........."
   "###......."
   "####%#..%#"
   "###~$~~~~#"
   "###~~~~~$#"
   "###$~~~~~#"
   "..#~~$~~$#"
   "..#%##%###"
   "........##"
   ".........."
   "..#......."
   "###....###"
   "###....###"
   "###.....##"]

  ["#......###"
   "##.....###"
   "###..##%##"
   ".%#..#$~$#"
   ".....#$~~#"
   ".....%~~~#"
   ".........."
   "..#......."
   "###..%####"
   "###......#"
   "###......#"
   "#%#####..#"
   "...####..#"
   "...####..."
   "#...###..."
   "#...###..."
   "##..##%..."
   "##........"
   "##.......#"
   "######..##"]

  ["####..####"
   "####..####"
   "####..##.."
   "####..##.."
   "###%..%#.."
   ".........."
   "..~~~~...."
   "##~~~~#..."
   "##+~~+####"
   "##~~~~####"
   "##+~~+####"
   "##%####%##"
   "...##~$~$#"
   "...%#~~~~~"
   ".....~~~~~"
   ".....~~~~~"
   "...##~~~~~"
   "#..##~~~~~"
   "#..##$~~$#"
   "######~~##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#######..."
   "##%##$~~~~"
   "....#~~~~~"
   "....#$~~~~"
   "##..#%#..."
   "##.......#"
   "##.......#"
   "##.......#"
   "##%####..#"
   ".....##..#"
   ".....##..#"
   "#..####..#"
   "#..#+%#..."
   "#..#..+..."
   "#........#"
   "#.......+#"
   "###.....##"]

  ["#%.....###"
   "#......###"
   "#....#####"
   "....######"
   "...#######"
   "..###%####"
   "..###....."
   "...##....."
   "....%..###"
   "#......###"
   "##.....###"
   "###....###"
   "..%%...###"
   "..%#####.."
   ".......#.."
   ".......%.."
   "..##......"
   "####......"
   "####...###"
   "###.....##"]

  ["####..####"
   "###%..####"
   "#........."
   ".........."
   "....##...."
   "...#####.."
   "....####.."
   ".....####."
   "##...%####"
   "###...####"
   "###....###"
   "####....##"
   "..###....#"
   "...###...."
   "#...##...."
   "#....%...."
   "##........"
   "##........"
   "###......#"
   "###.....##"]

  ["####...###"
   "####.....#"
   "#####....."
   "######...."
   "#%####...."
   "...###...."
   "....##...."
   "#...##...."
   "##...%...#"
   "##......##"
   "##.....###"
   "#...#..%##"
   "....#~~~$#"
   "...##~~~~~"
   "####$~~~~~"
   "###~~~~#~~"
   "##$~$~~~~~"
   "###~~~~~~~"
   "####$~~~$#"
   "######~~##"]

  ["####..####"
   "####..####"
   "###%..####"
   ".......###"
   ".......###"
   "...##..###"
   "...##....."
   "...%#....."
   "###%######"
   "###%######"
   "#......###"
   "#......%##"
   "...##....#"
   "...##....#"
   "#######..#"
   "####%....."
   "####......"
   "###......#"
   "###......#"
   "###.....##"]

  ["##.....###"
   "##......##"
   "#####...##"
   "#####...##"
   "####%...##"
   "........##"
   ".........."
   "##+###...."
   "#...######"
   "#......###"
   "#......%##"
   "#..#.....#"
   "...##....#"
   "...###...."
   "...####..."
   "...####..."
   "....%#...."
   "#........."
   "##.......#"
   "######..##"]

  ["####..####"
   "####.....#"
   "###......."
   "#%...##..."
   "#...##%..."
   "...###+..."
   "..###..~.."
   "####.+~~+."
   "#######~##"
   "######+~##"
   "######~###"
   "######~###"
   "..##.~~~.."
   "..##..~..."
   ".~~~~~...."
   ".~~~~~#..."
   ".~~~~~#..."
   "#~~~~~%##."
   "#~~~~~..##"
   "###.....##"]

  ["##.....###"
   "#......###"
   "#.....####"
   "......####"
   ".....#####"
   ".....%####"
   "..#...+..."
   "..#......."
   "##%.....##"
   "#.+.....##"
   "#....#####"
   "###+######"
   "....######"
   "......%###"
   "#.......##"
   "#..##....."
   "#...###..."
   "#.....%###"
   "##......##"
   "###.....##"]

  ["####..####"
   "####..#%##"
   "##....~~$~"
   "##....~~~~"
   "##..##$~~~"
   "....##~~~~"
   "....##$~~~"
   "##..##~~$~"
   "##..###%##"
   "##..##+~~#"
   "##..##~~+#"
   "##..##~~~#"
   ".#..##+~##"
   ".#..##...."
   ".%..#%...."
   ".........."
   ".........."
   "######...."
   "######..##"
   "######..##"]

  ["##.....###"
   "##......##"
   "##......##"
   ".#......##"
   ".%..##..##"
   "....##..##"
   "....##...."
   "....##...."
   "###%######"
   "###~##~$$#"
   "##~~~#$~$#"
   "##+~+#~~~#"
   "..~~~#$~~#"
   "..~~+#~~~."
   "##~~~#~~~."
   "##..#%~~~."
   "##....~~~."
   "##....~~~."
   "##......##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....###"
   "###...####"
   "###%..####"
   "......####"
   "......#..."
   "#.....#..."
   "#..##%#..#"
   "#........#"
   "#........#"
   "######%###"
   "........##"
   "........##"
   "##%###..##"
   "#~~$~#...."
   "#$~~~#...."
   "#~~~~...##"
   "#~$~~...##"
   "######..##"]

  ["####..####"
   "####..####"
   "####...###"
   "..##....%."
   "...##....."
   "....%#...."
   ".........."
   ".........."
   "##.....#+#"
   "########.#"
   "##.......#"
   "#......###"
   ".......#.#"
   "...#...#.."
   "...#...#.."
   "...#...%.."
   "...#......"
   "###%......"
   "###......."
   "###......."]

  ["####..####"
   "####..####"
   "####..####"
   "####..####"
   "##%#..#%##"
   ".........."
   ".........."
   ".........."
   "#####%####"
   "#####%####"
   "###%%%####"
   "###%######"
   ".....#####"
   "......####"
   "###....%##"
   "####......"
   "#####....."
   "######..##"
   "######..##"
   "######..##"]

  ["##.....%##"
   "#........#"
   "#...~~...."
   "...~##~..."
   "..~####~.."
   "..~####~.."
   "...~%#~..."
   "....~~...."
   "##......##"
   "###....###"
   "###+######"
   "##..######"
   "...#######"
   "...####%.."
   "...####..."
   "....%#...."
   ".........."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "####..####"
   "####..%#.."
   "####......"
   "###%......"
   ".........."
   ".........."
   "##.######."
   "##...#####"
   "####...###"
   "##%.....##"
   "##......##"
   ".....#..##"
   "....##..##"
   "#~~%##..%#"
   "#~~~~#...."
   "#$~~$#...."
   "#~~~~#..##"
   "#$~~$#..##"
   "######..##"]

  ["####..####"
   "####..####"
   "####..####"
   "####..####"
   "##%#..####"
   "......#%##"
   ".........."
   "##%#......"
   "##.#######"
   "##%#######"
   "##..###.%#"
   "#...##...#"
   "....##...#"
   "....##...."
   "....##...."
   "....##...."
   ".........."
   "#........."
   "#........#"
   "###.....##"]

  ["##.....###"
   "#......#%#"
   "#......#.."
   "#......#.."
   "###%#..#.."
   ".........."
   ".........."
   "###+#....."
   "##..######"
   "##...#####"
   "#$$%######"
   "#~~~######"
   ".~~~~#%###"
   ".~~~~...##"
   ".~~~~...##"
   ".~~~~#...."
   ".~~~$#...."
   "#~~~~#..##"
   "#~$~$#..##"
   "######..##"]

  ["####..#%##"
   "#........#"
   "#........#"
   "....%##..#"
   "......#..#"
   "......#..#"
   "......#..."
   "......#..."
   "#..#######"
   "#..###%###"
   "#..#.....#"
   "#..#.....#"
   "...#.....#"
   "...#.....#"
   "#..%..#..."
   "#.....#..."
   "#.....#..."
   "#.....%###"
   "#.......##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....#.."
   "#####..#.."
   "##%##..%.."
   ".........."
   ".........."
   "######...."
   "######...#"
   "#$$~~~~~.#"
   "#~~~##~~~#"
   "#$~$######"
   "~~~~%##%##"
   "~~~~$~~$~~"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "#$~~~~~~~~"
   "#~$~~~~~~#"
   "###.....##"]

  ["##.....%##"
   "##.......#"
   "####.....#"
   "...##....#"
   "....##...#"
   ".....##..#"
   "......#..."
   "......#..."
   "##....#..#"
   "###......#"
   "###......#"
   "#%....#..#"
   "......#..#"
   ".....##..#"
   ".....#%..#"
   "....##...."
   "...###...."
   "#####....#"
   "#####....#"
   "######..##"]

  ["##%#..#%##"
   "#+~~~~~~+#"
   "#~~~~~~~+#"
   "#~~##%#~~#"
   "#~~#~~#~~#"
   ".~~#$~#~~#"
   ".~~#~~.~~."
   "#~~#~~.~~."
   "#~~#$~#~~#"
   "#~~#~~#~~#"
   "#~~#~~#~~#"
   "#~~%~$#~~#"
   ".~~.~~#~~#"
   ".~~.~~#~~#"
   "#~~#~$#~~#"
   "#~~#~~#~~."
   "#~~#%##~~."
   "#+~~~~~~+#"
   "#~+~~~~~+#"
   "###.....##"]

  ["####..####"
   "####..####"
   "####..#..."
   "...#..#..."
   "...#..#..."
   "...#..#..."
   "......#..."
   "......#..."
   "#..####..#"
   "#..~~....#"
   "#..~~....#"
   "###~~#####"
   "...~~#####"
   "...~~##..."
   "###~~##..."
   "###~~##..."
   "###~~##..."
   "###~~....."
   "###~~....#"
   "######..##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#........#"
   "####.....#"
   "...##....#"
   "....###..."
   "#........."
   "#........#"
   "####.....#"
   "####+#####"
   "#........#"
   ".........#"
   "..##..#..."
   "..##..#..."
   "......#..."
   "......#..."
   "#.##..###."
   "###.....##"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "###...##.."
   ".#%...%#.."
   "..~~~~~..."
   "..~~~~~..."
   "..~~~~~..."
   "..~~~~~..."
   "##~+~+~###"
   "##~~######"
   "##%~######"
   "#~~~....##"
   ".~~~....##"
   ".~~~##..##"
   "#~~~##..%#"
   "#~~$##...."
   "#$~~##...."
   "#$~$##..##"
   "######..##"
   "######..##"]

  ["####..####"
   "###...%###"
   "##.....###"
   "#.......##"
   "....#....#"
   "...###~..."
   "..##~~~~.."
   "####~###.."
   "####~~####"
   "#####~####"
   "##%##~~~##"
   "#$~$$##~##"
   ".~~~~##%##"
   ".~~~~##..."
   "#~~~$#...."
   "#~~~$#...."
   "#..###...."
   "#........."
   "#........#"
   "######..##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#........."
   "###%##...."
   ".....#...."
   ".....#...."
   "#........."
   "#........#"
   "#%########"
   "#%########"
   "#%########"
   "....###%##"
   "....##...."
   "....##...."
   "....##...."
   ".....%...."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "#.....####"
   "#.....####"
   "#..#######"
   "#..#%#####"
   ".....##%##"
   ".....##..."
   "###..##..."
   "###..##..#"
   "###..##..#"
   "###..##..#"
   "#%#..##..#"
   ".....##..#"
   ".....##..#"
   "#..###%..#"
   "#........."
   "#........."
   "######..##"
   "######..##"
   "######..##"]

  ["##%#..####"
   "#.....#..#"
   "#.....#..."
   "....##%..."
   "....#....."
   "....#....."
   ".........."
   ".........."
   "#...#....#"
   "#####%####"
   "###%%%####"
   "###%###%##"
   ".~$~~$~~$#"
   ".~~~~~~~~."
   ".~~~~~~$~."
   ".~~~$~~~~."
   ".~~~~~~~~."
   "#$~~~~~~~."
   "#~$~~~~~$#"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##..######"
   "##..#%#%##"
   "##~~$~$~$#"
   "..~.....~#"
   "..~.~~~.~."
   "##~.~#~.~."
   "##~.~~~.~#"
   "##~.~~~.$#"
   "##~.~~~.$#"
   "##~.~~~.$#"
   "..~.~~~.$#"
   "..~.~~~.$#"
   "##~.~#~.~#"
   "##$.~~~.~."
   "##$.....~."
   "##~$$~~~$#"
   "######..##"
   "######..##"]

  ["####..#%##"
   "####.....#"
   "####......"
   ".#####...."
   "..####...."
   "...###...."
   "...###...."
   "...#%#...."
   "#........#"
   "#........#"
   "###+######"
   "###%######"
   ".....#####"
   ".....#...."
   "###..#...."
   "#........."
   "#........."
   "#........."
   "#........#"
   "###.....##"]

  ["####..####"
   "####...%##"
   "##%##...##"
   "...###..##"
   "....##..##"
   "....##..##"
   "....##...."
   "....##...."
   "#...#....#"
   "#.......##"
   "#......###"
   "###...####"
   "..+..#####"
   "..#####%##"
   "...###...#"
   "....##...."
   ".....%...."
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....%##"
   "#........#"
   "#........."
   "#..##....."
   "#..###...."
   "...####..."
   "...#####.."
   "#%%%#####."
   "#.......##"
   "#........#"
   "##~~%#...#"
   "#~~~$##..#"
   "~~~~~##..#"
   "~~~~~$#..#"
   "~~~~~~#..#"
   "~~~~~$#..."
   "~~~~~##..."
   "#$~~$#%..#"
   "##$$##...#"
   "######..##"]

  ["##%...##%#"
   "##....#..#"
   "#....##..."
   "#...##~..."
   "#..##$~..."
   "...##~~..."
   "...##$~..."
   "#..%###..."
   "#........#"
   "#........#"
   "#####+####"
   "#####%#%##"
   ".........#"
   ".........#"
   "#..#~~#..#"
   "#..#~~#..."
   "#..####..."
   "#........#"
   "#........#"
   "###.....##"]

  ["##%#..####"
   "#........#"
   "#........#"
   "...##%#..#"
   "...##....#"
   "...##....#"
   "...##....."
   "...##....."
   "#..##....#"
   "####%....#"
   "###%%#...#"
   "###%######"
   ".....###%#"
   "......##.."
   "###...##.."
   "#.##..%#.."
   "#........."
   "#........."
   "#........#"
   "###.....##"]

  ["##.....###"
   "###....###"
   "####..##.."
   "####..##.."
   "####..%#.."
   ".........."
   ".........."
   "###......."
   "####..####"
   "####.~####"
   "####~.####"
   "#%##~~####"
   "...#~~##%#"
   "...#~~#..."
   "...#~~#..."
   "...#......"
   ".........."
   "#........."
   "#.......##"
   "######..##"]

  ["####..####"
   "####.....#"
   "####.....#"
   ".####%#..#"
   "..##.....#"
   "...#.....#"
   "...%..#..."
   "......#..."
   "#.....####"
   "#.....####"
   "###%######"
   "#........#"
   ".........#"
   ".........#"
   "##..###..#"
   "##..%##..."
   "#....##..."
   "#.....####"
   "#......###"
   "###.....##"]

  ["##.....###"
   "##.....###"
   "##.....###"
   "###...####"
   "#%##..####"
   "......##%#"
   "......#..."
   "#..#%##..."
   "#........#"
   "#........#"
   "###+######"
   "###.######"
   ".....#####"
   "......%..."
   "##........"
   "####......"
   "#####....."
   "#####....."
   "######...#"
   "######..##"]

  ["##.....###"
   "#......###"
   "#....####."
   "#...##%#.."
   "#...#....."
   ".........."
   ".........."
   "###%......"
   "###%#%####"
   "##%%%%%###"
   "##%#%#%###"
   "#$~$~$~$~#"
   "~~~~~~~~$#"
   "~~~~~~~~~~"
   "~~~~~~~~~~"
   "~~~~~#~~~~"
   "~~~~~~~~~~"
   "#$~~~~~~~~"
   "#~$~~~~~$#"
   "###.....##"]

  ["#%##..#%##"
   "#..#.....#"
   "#..#.....#"
   "...#.....#"
   ".........#"
   ".........#"
   "...####..."
   "...###%..."
   "#..#~$~~~#"
   "#..#$~~~~#"
   "#..#$~~~~#"
   "#..#~$~~~#"
   "...####..#"
   "...###%..#"
   "#####+~~~#"
   "#####~~~~."
   "#####~~~~."
   "#####+~~~#"
   "#####+~~~#"
   "######..##"]

  ["##%#..####"
   "#........#"
   "#........."
   "#........."
   "#..#####.."
   "...#####.."
   "...#####.."
   "#+######.."
   "#....%####"
   "#........#"
   "#........#"
   "#..###...#"
   "...####..#"
   "....###..."
   "....###..."
   "....##%..."
   "...###...."
   "######...."
   "######...#"
   "######..##"]

  ["##.....###"
   "#......###"
   "#......###"
   "#####..###"
   "##%##..###"
   ".~~+#..%##"
   ".~~+#....."
   "#~~~#....."
   "#.~~##%..#"
   "#.~~.~~~~#"
   "#.~~.~~~~#"
   "#+###~~~~#"
   "...##~~~$#"
   "...##$$$~#"
   "#..#######"
   "#..####..."
   "#..#%##..."
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....%##"
   "#.......##"
   "#.......##"
   "....##..%#"
   "....##...#"
   "....###..#"
   "...####..."
   "..#####..."
   "#%########"
   "#.%#######"
   "#..#######"
   "#...###..#"
   "....##...#"
   ".....#...."
   ".....%...."
   ".........."
   ".........."
   "###......."
   "######...#"
   "######..##"]

  ["####..####"
   "####..##%#"
   "####..#..."
   "####..#..."
   "##%#..#..."
   "...#..#..."
   "...#..#..."
   "#..#..#..."
   "#..%..%..#"
   "#........#"
   "#........#"
   "##.#..####"
   "...#######"
   "....#%##.."
   "#.....##.."
   "##.....%.."
   "###......."
   "####......"
   "#####....#"
   "######..##"]

  ["##%...####"
   "##....####"
   "##...#####"
   "#....#####"
   ".......%##"
   "........##"
   "...##....."
   "#######..."
   "########.#"
   "########%#"
   "###%#$$~~#"
   "##~~~~~~~#"
   "..~~~~~~~#"
   "..~~~~~~~#"
   "##$~$~#..#"
   "#####%#..."
   "####......"
   "####.....#"
   "###......#"
   "###.....##"]

  ["##%#..##%#"
   "#........#"
   "#........."
   "#.....#..."
   "##%#..#..."
   "......#..."
   "......#..."
   "#.....#..."
   "#.....#..#"
   "##.#######"
   "##%####%##"
   "#$~$#$~~$#"
   ".~.~#~..~#"
   ".~.~#~..~#"
   "#~.~#~..~#"
   "#~.~%~..~."
   "#~.~~~..~."
   "#~......~#"
   "#$~~~~~~~#"
   "######..##"]

  ["##.....###"
   "##...##%##"
   "###..#$~$#"
   "..#..#~~~#"
   "..#..#~~~#"
   "..%..%~~~%"
   ".........."
   ".........."
   "#........#"
   "###+######"
   "###%######"
   "#.......##"
   "........##"
   "...##...#."
   "...##....."
   "...##....."
   "...##....."
   "####%..##."
   "###.....##"
   "###.....##"]

  ["####..#%##"
   "...#~~~~~#"
   "...#~~~~~#"
   "...#+~~~~#"
   "...#~+~~~#"
   "...##%#..#"
   ".........."
   ".........."
   "#%%%%#####"
   "###%######"
   "#~$~~~~$~#"
   "#$~~~~~~$#"
   ".~~~~#~~~#"
   ".~~~$#$~$#"
   "#~~~$#####"
   "#~~~~#%..."
   "#$~~~....."
   "#$~~~....#"
   "#~$$~....#"
   "######..##"]

  ["####..%###"
   "#...~~~~+#"
   "#...~~~~+#"
   "#..#~~~~~#"
   "#..#~+~~~#"
   "...####%##"
   ".........."
   "#........."
   "#..###%..#"
   "#..##$~~~#"
   "#..##$~~~#"
   "#%###~~~~#"
   "...##~~~~#"
   "...##~$~$#"
   "...#######"
   ".........."
   ".........."
   "#####%..##"
   "###.....##"
   "###.....##"]

  ["##.....###"
   "##.....%##"
   "##........"
   ".#........"
   ".#........"
   ".###%##..."
   ".........."
   ".........."
   "#..#..#..#"
   "#..#..#..#"
   "#..####..#"
   "#..###%..#"
   "...##....."
   "...##....."
   "##%##...#."
   "#$$~#...#."
   "#~~~#...#."
   "#~~~....##"
   "#~~~....##"
   "######..##"]

  ["#%##..####"
   "#........#"
   "#........."
   "#..#%##..."
   "#..#~~#..."
   "...#~~#..."
   "...#~~#..."
   "####~~#..."
   "#...~~%###"
   "#...~~~~$#"
   "#..#~~~~$#"
   "#..#~~~~$#"
   "...#~~~###"
   "...#~~~#.."
   "...#~$$#.."
   "...##%##.."
   ".........."
   "#........."
   "#.......##"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#..#%##..#"
   "#..#..#..#"
   "...#..#..#"
   "...#......"
   "#..#......"
   "#..#..#%##"
   "#..#...~+#"
   "#..#...~~#"
   "#..#...~~#"
   "...#..#~~#"
   "...#..#~+#"
   "##%#..%###"
   "#$~~~~~~.."
   "#~~~~~~~.."
   "#~~~~~~~##"
   "#$~~~~~~##"
   "###.....##"]

  ["##%#..####"
   "#..#.....#"
   "#..#......"
   "#..#......"
   "#..####..."
   "......#..."
   "......#..."
   "#..##%#..."
   "#........#"
   "#........#"
   "######.###"
   "######%###"
   ".........#"
   ".........."
   "###%##...."
   "#~$~~~...."
   "#$~~~~...."
   "#$~~~~...."
   "#~$~~~...#"
   "######..##"]

  ["###%..####"
   "#$~~~~~$~#"
   "#~~~~~~~$#"
   ".~~~~~~~~#"
   ".~~$~$~$~#"
   "...##%####"
   ".........."
   ".........."
   "#######%##"
   "#........#"
   "#........#"
   "#..####..#"
   "......#..#"
   "......#..#"
   "......#..#"
   "......#..."
   "......#..."
   "####..%###"
   "###.....##"
   "###.....##"]

  ["##.....%##"
   "#........#"
   "#........#"
   "...##....#"
   "....#....#"
   "....#..###"
   "....#....."
   "....#....."
   "##%##~~###"
   "##+##~~++#"
   "#$$$#~~~~#"
   "#~~~#+~~~#"
   ".~~$#+~~+#"
   ".~~~######"
   "#~~~#%####"
   "#~~~~~~~.."
   "#$~~~~~~.."
   "#$~~~~~~##"
   "#~$$~~~~##"
   "######..##"]

  ["#%##..####"
   "#........#"
   "#........#"
   "#..##%#..#"
   "#..#$~~~~#"
   "...#~~~~~#"
   "...#$~~~~."
   "####~~~~~."
   "#..#$~~~~#"
   "#..##..%##"
   "#..#+~~~+#"
   "#..#+~~~+#"
   "...#+~~~+#"
   "...#+~~~+#"
   "#..##%..##"
   "#..#......"
   "#..#......"
   "#........#"
   "#........#"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........."
   "...##%#..."
   "...#~~#..."
   "...#~~#..."
   "...#~~#..."
   "...#~~#..."
   "#..%~~%..#"
   "#........#"
   "#$$$..$$$#"
   "######%###"
   "....#$+$.#"
   "....#$...."
   ".#..#$...."
   ".#..#$...."
   ".#..%....."
   "##........"
   "##.......#"
   "######..##"]

  ["#%##..%###"
   "#...~~~~~#"
   "#...~~~~+#"
   "#..#~~~~~#"
   "#..#~+~+~#"
   "...#######"
   ".........."
   "#........."
   "#..###%###"
   "#..#~$~$~#"
   "#..#$~~~$#"
   "#..%~~~~$#"
   "....~~~~$#"
   "....~~~~~#"
   "#..#$~~~~#"
   "#..#~$~~~."
   "#..#%##..."
   "#.....####"
   "#.......##"
   "###.....##"]

  ["##.....###"
   "#........#"
   "#........#"
   "#........#"
   "##%#%#%..#"
   ".~~$~$~~~#"
   ".~......~."
   "#~......~."
   "#~......~#"
   "#$......$#"
   "#~$~$$~$~#"
   "###%####%#"
   ".~+~+#...#"
   ".~~~+#...."
   "#~~~+#...."
   "#~~~~#...."
   "#~~~~#...."
   "#+~~~....."
   "#++~~...##"
   "######..##"]

  ["####..####"
   "####..##%#"
   "####..#..."
   "...#..#..."
   "...#..#..."
   "...%..%..."
   ".........."
   ".........."
   "#####%####"
   "##$$~~$~$#"
   "##~.....$#"
   "##~.....$#"
   ".#~.....~#"
   ".#~~~~$$~#"
   ".#..#%####"
   ".........."
   ".........."
   "##.......#"
   "##.......#"
   "###.....##"]

  ["##%#..####"
   "#........#"
   "#........#"
   "#..####..#"
   "#.....#..#"
   "......#..#"
   "......#..."
   "#.....#..."
   "##.####.##"
   "##%#%##%##"
   "#$~$~$$~$#"
   "#~......$#"
   ".~.~~~~.~#"
   ".~.~~~~.~."
   "#$.~~~~.~."
   "#$.~~~~.~."
   "#$.~~~~.~."
   "#$......~."
   "#~$$$~~~$#"
   "######..##"]

 ])
