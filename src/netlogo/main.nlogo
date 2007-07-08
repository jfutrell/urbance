breed [ roadlinks link ]
breed [ nodes node ]
breed [ buildingblocks buildingblock]

turtles-own [ explored? ]
globals
[
  component-size          ;; number of roadnodes explored so far in the current component
  giant-component-size    ;; number of roadnodes in the giant component
  giant-start-node        ;; node from where we started exploring the giant component
  contour-data
  buildable-data
  heightlevel
]

;; Set up as the name implies sets up the scenario
;; It calls mainly the load-countour-data function which sets up the contour and paints
;; the apporpriate patches green
;; make-nodes , find-all-components and color-giant-component functions are related to
;; the road network generation
to setup
  ca
  setxyz 150 150 100
  set heightlevel 1
  load-contour-data
  set-default-shape turtles "circle"
  make-nodes
  find-all-components
  color-giant-component
end

;; Clear is a simple function which is similar to the setup function but unlike it does
;; not load a contour. This is helpful as a button during the development process
;; and also considering the fact that various observations could be made on a plain
;; terrain as well

to clear
  ca
  setxyz 150 150 100
  set heightlevel 1
  set-default-shape turtles "circle"
  make-nodes
  find-all-components
  color-giant-component
  ask patches with [pzcor = 0 ] [set pcolor green + (random-float 2 ) - 1]
end

;; load-contour-data uses the standard netlogo primitives for loading a file and reading
;; the data. As of now contour data is stored as [ x y z] which basically gives
;; the height of various locations (x,y)

to load-contour-data
  let file user-new-file

  ifelse ( file != false )
  [
    set contour-data []
    file-open file

    while [ not file-at-end? ]
      [ set contour-data sentence contour-data (list (list file-read file-read file-read)) ]

    user-message "Loading template file specified"
    file-close
    no-display
    foreach contour-data [ draw-contour first ? item 1 ? last ? ]
    ask patches with [pzcor = 0 ] [set pcolor green + (random-float 2 ) - 1]
    display
    user-message "Contour loading complete!"
  ]
  [ user-message "No contour file specified. Loading plain terrain"
    ask patches with [pzcor = 0 ] [set pcolor green + (random-float 2 ) - 1] ]
end

;; eventually to be replaced with the bezier curves and surfaces  this is a primitive and simple spiraling contour drawing algorithm

to draw-contour [cx cy cz]
       if ( cz >= 1 and cz < max-pycor and cx < max-pxcor - 2 and cy < max-pycor - 2 and cx > min-pxcor + 2 and cy > min-pycor + 2 )
       [
       let planstep  1
       let zstep  1
       set pcolor-of patch cx cy cz lime + (random-float 2 ) - 1
       if (( pcolor-of patch (cx - planstep) cy (cz + zstep ))  = 0 ) [ draw-contour cx - planstep cy cz - zstep ]
       if (( pcolor-of patch (cx - planstep) (cy - planstep) (cz + zstep)) = 0 ) [ draw-contour cx - planstep cy - planstep cz - zstep ]
       if (( pcolor-of patch (cx - planstep) (cy + planstep) (cz + zstep) ) = 0 ) [ draw-contour cx - planstep cy + planstep cz - zstep ]
       if (( pcolor-of patch cx (cy - planstep) (cz + zstep)) = 0 ) [ draw-contour cx cy - planstep cz - zstep ]
       if (( pcolor-of patch cx (cy + planstep) (cz + zstep)) = 0 ) [ draw-contour cx cy + planstep cz - zstep ]
       if (( pcolor-of patch (cx + planstep) (cy + planstep) (cz + zstep)) = 0 ) [ draw-contour cx + planstep cy + planstep cz - zstep ]
       if (( pcolor-of patch (cx + planstep) cy (cz + zstep)) = 0 ) [ draw-contour cx + planstep cy cz - zstep ]
       if (( pcolor-of patch (cx + planstep) (cy - planstep) (cz + zstep)) = 0 ) [ draw-contour cx + planstep cy - planstep cz - zstep ]     
       ]
end


;; nodes in this program context stands for the transportation network
   
to make-nodes
  create-nodes num-nodes
  [
    setxy random-xcor random-ycor
    set zcor 0
  ]
end

;; typical go function
;; some functions to be changed : randheighter

to go
  ;; we don't want the display to update while we're in the middle
  ;; of updating stuff, so we use no-display/display to freeze/unfreeze
  while [ (2 * count roadlinks ) <= connectedness * ( (count nodes) * (count nodes - 1) ) ]
  [
  no-display
  add-edge
  find-all-components
  color-giant-component
  ask roadlinks [ set color color-of __end1 ]  ;; recolor all edges
  ]
  display
  drawroadpatch
  user-message "Road Network Generation complete"
  ;randheighter
  ;user-message "Random Height Building Generation complete"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Network Exploration ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to find all the connected components in the network, their sizes and starting roadnodes
to find-all-components
  ask nodes [ set explored? false ]
  ;; keep exploring till all roadnodes get explored
  loop
  [
    ;; pick a node that has not yet been explored
    let start one-of nodes with [ not explored? ]
    if start = nobody [ stop ]
    ;; reset the number of roadnodes found to 0
    ;; this variable is updated each time we explore an
    ;; unexplored node.
    set component-size 0
    ;; at this stage, we recolor everything to light gray
    ask start [ explore (gray + 2) ]
    ;; the explore procedure updates the component-size variable.
    ;; so check, have we found a new giant component?
    if component-size > giant-component-size
    [
      set giant-component-size component-size
      set giant-start-node start
    ]
  ]
end

;; Finds all roadnodes reachable from this node (and recolors them)
to explore [new-color]  ;; node procedure
  if explored? [ stop ]
  set explored? true
  set component-size component-size + 1
  ;; color the node
  set color new-color
  ask __link-neighbors [ explore new-color ]
end

;; color the giant component red
to color-giant-component
  ask nodes [ set explored? false ]
  ask giant-start-node [ explore red ]
end

to add-edge
  let node1 one-of nodes
  let node2 one-of nodes
  ask node1 [
    ifelse __link-neighbor? node2 or node1 = node2
    [ add-edge ]
    [ __create-link-with node2 ]
  ]
    
end

;; Drawing the Road
;; identifies the link elements / agents and traverses along them marking the region black as of now. This leaves us
;; with the buildable green patch

to drawroadpatch

  ask roadlinks [ 
      repeat size / 2 [ fd 1
                        set pcolor black
                        ;let x xcor
                        ;let y ycor
                        ;ask patches with [ pxcor = x and pycor = y and pcolor != black ] [ set pcolor black]
                        lt 90
                        fd 1
                        set pcolor black
                        ;set x xcor
                        ;set y ycor
                        ;ask patches with [ pxcor = x and pycor = y and pcolor != black ] [ set pcolor black]
                        bk 1
                        rt 90
                      ]
      bk size / 2
      repeat size / 2 [ bk 1
                        set pcolor black
                        ;let x xcor
                        ;let y ycor
                        ;ask patches with [ pxcor = x and pycor = y and pcolor != black ] [ set pcolor black]
                        lt 90
                        fd 1
                        set pcolor black
                        ;set x xcor
                        ;set y ycor
                        ;ask patches with [ pxcor = x and pycor = y and pcolor != black ] [ set pcolor black]
                        bk 1
                        rt 90
                      ]
      jump size / 2
      ]   
end   

;; Bezier Exploits
;; Still to be finished and optimized are the bezierblend functions which would
;; eventually take the place of the countour drawing function


to-report bezierblend [ k mu n]
  let nn n
  let kn k
  let nkn n - k
  let blend 1.0
  
  while [ nn >= 1]
  [
    set blend  blend * nn
    set nn  nn - 1
    if ( kn > 1) 
      [ set blend  blend / kn
        set kn  kn - 1
      ]
    if ( nkn > 1)
      [ set blend  blend / nkn
        set nkn nkn - 1
      ]
  ]
  if ( k > 0) [ set blend  blend * ( mu ^ k )]
  if ( n - k > 0 ) [ set blend  blend * ( ( 1 - mu ) ^ ( n - k)) ]
  report blend
end

to bezierpoint
  foreach contour-data 
    [ 
      draw-contour first ? item 1 ? last ?
    ]
end


;; random height playing
;; temporary testing function for the vertical growth

to randheighter

  ;ask patches with [ pzcor = 0 ]
  ;            [ if (pcolor != black and random 3 = 1) [
  ;              let tempvar 1
  ;              repeat random 14 [ 
  ;                           ask patch pxcor pycor ( pzcor + tempvar ) [ set pcolor blue ]
  ;                           set tempvar tempvar + 1
  ;                           ]
  ;              ]
  ;           ]
  make-buildingblocks
  grow-buildingblocks
end

;; generates random building blocks into the growable patch left after the roadpatches
;; have been drawn Further modifications: Non random positioning. Affinity to nodes

to make-buildingblocks

  repeat 300 [ 
              let inputx 0
              let inputy 0
              let makeblocks 0
              ask patch-at random-xcor random-ycor 0 [
              if ( pcolor != black) [
                                    set inputx  pxcor
                                    set inputy  pycor
                                    set makeblocks 1
                                    ]
              ]
            if ( makeblocks = 1 )[ make-block inputx inputy ]
            ]
end

to make-block [inputx inputy]
  create-buildingblocks 1 [ setxy inputx inputy
                            set zcor 1
                            set shape "square"
                            set size 1
                            set color blue
                            set pcolor red]
end

;; Growth of the building blocks in the horizontal plane
to grow-buildingblocks
  ask buildingblocks [ set pcolor red]
  ask patches  with [ pzcor = 1 and pcolor = red ]
  [ paint-agents neighbors ]
  ;foreach ( patches with [ pcolor = blue ])
  ;  [ make-block pxcor pycor ]
    
end

;; Grows till all the green patch which is simply connected is filled

to grow-till-limit
  loop [
  let beforegrowth  count patches with [ pzcor = 1 and pcolor = red ]
  grow-buildingblocks
  if ( beforegrowth = count patches with [ pzcor = 1 and pcolor = red ] )
    [
      user-message "Maximum growth reached"
      stop ]
  ]
end

to paint-agents [agents]
  ask agents with [pzcor = 1 and pcolor-of  patch-at 0  0 -1 != black ][ set pcolor red ]
end

;; clears all the patches but this does not remove the block agents as such

to clear-buildingblocks
  ask patches with [ pzcor = 1 and pcolor = red ] [ set pcolor black ]
end

;; Grow-percentage function: Allowing the user to specify the amount / extent of growth
;; in the horizontal plane
to grow-percentage
end

;; Growth in the vertical plane : Eventual form would be of artificial life-sque style

to raise-blocks
  ask patches with [ pcolor = red and pzcor = heightlevel ] 
          [ 
          let rn 0;
          set rn count neighbors with [pcolor = red ]
          if ( rn > 4 )
              [ ifelse ( rn = 5 )
                        [ if( pzcor != 1 ) [ ask patch-at 0 0 -1 [set pcolor black]
                          set pcolor black ]
                        ]
                        [ ifelse ( rn = 6 )
                              [ ask patch-at 0 0 1  [set pcolor red ] ]
                              [ ifelse ( rn = 7 )
                                   [ ask patch-at 0 0 1 [set pcolor red] ]
                                   []
                              ]
                        ]
              ]
          ]
  set heightlevel heightlevel + 1
end
@#$#@#$#@
GRAPHICS-WINDOW
91
247
310
487
100
100
1.04
1
10
1
1
1
0
1
1
1
-100
100
-100
100
0
15
1

CC-WINDOW
5
501
607
596
Command Center
0

BUTTON
14
20
78
53
Setup
setup
NIL
1
T
OBSERVER
T
NIL

BUTTON
170
20
233
53
Go
go
NIL
1
T
OBSERVER
T
NIL

SLIDER
14
69
186
102
num-nodes
num-nodes
10
100
36
1
1
NIL

SLIDER
13
119
200
152
connectedness
connectedness
0.04
0.3
0.3
0.01
1
NIL

BUTTON
265
22
393
55
Grow blocks once
grow-buildingblocks
NIL
1
T
OBSERVER
T
NIL

BUTTON
395
72
485
105
Clear blocks
clear-buildingblocks
NIL
1
T
OBSERVER
T
NIL

BUTTON
266
71
380
104
Grow maximum
grow-till-limit
NIL
1
T
OBSERVER
T
NIL

SLIDER
265
119
437
152
growth-percentage
growth-percentage
0
100
50
1
1
NIL

BUTTON
266
165
422
198
Growth by percentage
grow-percentage
NIL
1
T
OBSERVER
T
NIL

BUTTON
500
25
598
58
Raise blocks
raise-blocks
NIL
1
T
OBSERVER
T
NIL

BUTTON
91
21
154
54
Clear
clear
NIL
1
T
OBSERVER
T
NIL

@#$#@#$#@
WHAT IS IT?
-----------
Urbance is an agent-based approach to simulating urban systems which would thus help understand evolutionary traits in an urban scenario.


HOW IT WORKS
------------



HOW TO USE IT
-------------


THINGS TO NOTICE
----------------


THINGS TO TRY
-------------


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------



CREDITS AND REFERENCES
----------------------
Under Development by Dawn Thomas mentored by Dr. Rick L. Riolo of Centre for Study of  Complex Systems funded by Google Inc. under the Google Summer of Code program ( 2007 )
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 3D Preview 4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="SimpleTest" repetitions="1" runMetricsEveryTick="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="connectedness">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-nodes">
      <value value="36"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="growth-percentage">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
