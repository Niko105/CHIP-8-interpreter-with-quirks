(ns chip8.render.cli (:require [chip8.screen :as screen]))

(defn render-ascii-chip8
  "Renders the chip8 screen (64x32) in ascii characters, does not support high resolution (SCHIP)."
  [screen]
  (newline)
  (println "----------------------------------------------------------------")
  (dotimes [y 32]
    (dotimes [x 64]
      (if (screen/get-xy screen x y)
        (print "█")
        (print " ")))
    (newline))
  (println "----------------------------------------------------------------")
  (print "\033[H") ;home cursor (overwrite frame)
  (flush))
