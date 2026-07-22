(ns chip8.keyboard)

(def start-keys (vec (repeat 16 false))) ;all keys off

(def mapping ;key to value mapping
  {"1" 0x1, "2" 0x2, "3" 0x3, "4" 0xC,
   "q" 0x4, "w" 0x5, "e" 0x6, "r" 0xD,
   "a" 0x7, "s" 0x8, "d" 0x9, "f" 0xE,
   "z" 0xA, "x" 0x0, "c" 0xB, "v" 0xF})

;;TODO: implement reading from the cli/screen
(defn key-cli
  "Updates the keyboard map of the cpu with the currently pressed keys. [CURRENTLY TOGGLES RANDOM KEYS]"
  [state]
  (update-in state [:keys (second (rand-nth (vec mapping)))] not))

(defn key-swing
  "Updates the keyboard map of the cpu with the currently pressed keys. [CURRENTLY TOGGLES RANDOM KEYS]"
  [state jframe]
  [state jframe])

;;TODO: implement key-up and key-down functions returning which key was detected, playing favourites with the closest in the mapping
