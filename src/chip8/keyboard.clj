(ns chip8.keyboard
  (:require [chip8.input.terminal :as term]
            [chip8.input.jframe :as swing]))

(def start-keys (vec (repeat 16 false))) ;all keys off

(def mapping ;key to value mapping
  {\1 0x1, \2 0x2, \3 0x3, \4 0xC,
   \q 0x4, \w 0x5, \e 0x6, \r 0xD,
   \a 0x7, \s 0x8, \d 0x9, \f 0xE,
   \z 0xA, \x 0x0, \c 0xB, \v 0xF})

(defn update-keyboard
  "Updates the keyboard map of the cpu with the currently pressed keys."
  [state] ;it doesn't really "update" it, it just kind of rewrites it entirely from scratch
  (let [keycode (term/get-key) ;the actual key being pressed
        val (when keycode (mapping (char keycode))) ;the value of the keycode, if any
        empty (vec (repeat 16 false))] ;the empty vector to write onto
    (assoc state :keys (if val ;if there IS a value
                         (assoc empty val true) ;set :keys to a vector with that key true
                         empty)))) ;else it's just entirely empty

;;TODO: implement key-up and key-down functions returning which key was detected, playing favourites with the closest in the mapping
;;;TODO: implement automatic term/swing switching based on a setting
