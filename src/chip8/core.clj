(ns chip8.core
  (:require [chip8.cpu :as cpu]
            [chip8.render.render :as render])
  (:import [java.nio.file Files Paths]))

(def main-frequency 1000) ;main clock frequency (CHIP8 1000Hz, SCHIP ~5000Hz)
(def time-per-tick (int (* (/ 1 main-frequency) 1000))) ;time taken on each tick (roughly)
(def seconds-to-run 60) ;how many seconds the sim should run for
(def runtime (* main-frequency seconds-to-run)) ;how many ticks to run the sim for (108660000 gives rule 090 on 1dcell.ch8)
(def fps 60) ;the fps the render thread runs at
(def ticks-per-frame (int (/ main-frequency fps))) ;time taken on each frame (roughly)
(def rom-path "programs/slipperyslope.ch8") ;rom to load and its path

(defn read-rom-into-memory
  "Returns a new memory state with the ROM loaded in."
  [memory file-path]
  (let [raw-bytes (Files/readAllBytes (Paths/get file-path (into-array String []))) ;read every byte of the file
        unsigned-rom-bytes (mapv #(bit-and % 0xFF) raw-bytes) ;turn them unsigned 0-255, mapv is map() (not lazy)
        rom-size (count unsigned-rom-bytes) ;see how large the rom is
        program-start cpu/program-start
        memory-size cpu/memory-size]

    (if (> rom-size (- memory-size program-start))
      (throw (IllegalArgumentException. "ROM too large."))
      (let [updated-mem (reduce-kv (fn [mem i byte] ;accumulator, index, value; a foreach on crack
                                     (assoc mem (+ program-start i) byte)) ;replace mem with byte at (+ 0x200 i)
                                   memory ;starting accumulator
                                   unsigned-rom-bytes)] ;array to iterate over
        updated-mem)))) ;send the updated memory out

(defn load-rom
  "Loads the specified ROM and outputs a cpu state with the ROM in memory."
  [rom-file-path]
  (let [rom (read-rom-into-memory (:memory cpu/start-state) rom-file-path)]
    (assoc cpu/start-state :memory rom))) ;returns a new state with the rom loaded

(def state (atom (load-rom rom-path))) ;mutable state for stepping

(defn -main []
  (print "\033[H\033[2J") ;clear screen first
  (flush)
  #_(println @state) ;@ cause it's an atom and has to be dereferenced
  (dotimes [i runtime]
    (cpu/tick! state)
    #_(println @state)
    (when (zero? (mod i ticks-per-frame)) ;runs at ~(/ main-frequency fps), 60Hz by default
      (render/render-ascii-chip8 (:screen @state))
      (cpu/tick-timers! state))
    (when (cpu/beep? state) (print "\u0007")) ;terminal bell sound
    (Thread/sleep time-per-tick)))
