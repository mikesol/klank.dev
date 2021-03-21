# klank-weblib

Functions to help build webapps with `klank-studio`.

Specifically, and most importantly, it contains the functions `fetchAssets`, `playKlank`, `stopKlank`, which can be used to build a Halogen webapp using `purescript-audio-behaviors`. These functions intend to accomplish two things:

1. Be maximally polymorphic for to accommodate halogen setup (different actions, different slots, etc).
1. Give all required information to `purescript-audio-behaviors` to correctly render (ie microphones, canvases, etc).
