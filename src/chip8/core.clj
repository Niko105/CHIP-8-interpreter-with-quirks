(ns chip8.core (:require [chip8.cpu :as cpu]))
(import '[java.nio.file Files Paths])

(defn read-rom-into-memory
  "Returns a new memory state with the ROM loaded in."
  [state file-path]
  (let [raw-bytes (Files/readAllBytes (Paths/get file-path (into-array String []))) ;read every byte of the file
        unsigned-rom-bytes (mapv #(bit-and % 0xFF) raw-bytes) ;turn them unsigned 0-255, mapv is map() (not lazy)
        rom-size (count unsigned-rom-bytes)] ;see how large the rom is

    (if (> rom-size (- 4096 0x200))
      (throw (IllegalArgumentException. "ROM too large."))
      (let [current-mem (:memory state) ;get the memory for the cpu's state
            updated-mem (reduce-kv (fn [mem i byte] ;accumulator, index, value; a foreach on crack
                                     (assoc mem (+ 0x200 i) byte)) ;replace mem with byte at (+ 0x200 i)
                                   current-mem ;starting accumulator
                                   unsigned-rom-bytes)] ;array to iterate over
        updated-mem)))) ;send the updated memory out

(defn load-rom
  "Loads the specified ROM and outputs a cpu state with the ROM in memory."
  [rom-file-path]
  (let [rom (read-rom-into-memory cpu/start-state rom-file-path)]
    (assoc cpu/start-state :memory rom))) ;returns a new state with the rom loaded

(def rom "programs/1dcell.ch8") ;name and path of the rom
(def state (atom (load-rom rom))) ;mutable state for stepping (to redo, cpu has changed significantly)

(defn -main []
  (print @state)) ;@ cause it's an atom and has to be dereferenced
