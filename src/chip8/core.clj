(ns chip8.core
  (:require [chip8.cpu :as cpu]
            [chip8.render.render :as render])
  (:import [java.nio.file Files Paths]))

(defn- ms-from-frequency
  "Helper function to calculate the period in milliseconds given a frequency. Truncated to int."
  [frequency]
  (int (* (/ 1 frequency) 1000)))

(def main-frequency 1000) ;main clock frequency (CHIP8 1000Hz, SCHIP ~5000Hz)
(def time-per-tick (ms-from-frequency main-frequency)) ;time taken on each tick (roughly)
(def seconds-to-run 60) ;how many seconds the sim should run for
(def runtime (* main-frequency seconds-to-run)) ;how many ticks to run the sim for (108660000 gives rule 090 on 1dcell.ch8)
(def fps 60) ;the fps the render thread runs at
(def time-per-frame (ms-from-frequency fps)) ;time taken on each frame (roughly)
(def instructions-per-frame 500) ;how many instructions to run every frame
(def rom-path "programs/knumberknower.ch8") ;rom to load and its path

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
  (println @state) ;@ cause it's an atom and has to be dereferenced
  (dotimes [i runtime]
    (cpu/tick! state)
    #_(println @state)
    (when (= 0 (mod i instructions-per-frame)) (render/render-ascii-chip8 (:screen @state)) (Thread/sleep time-per-frame)) ;not in sync yet, timings aren't decoupled
    (Thread/sleep time-per-tick)))
