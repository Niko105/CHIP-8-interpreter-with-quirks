(ns chip8.cpu
  (:require [chip8.screen :as screen]
            [chip8.keyboard :as keys]
            [chip8.instructions :as instr]))
;; [ ] 1.9: CHIP-8 Virtual Machine & Interpreter (Clojure)
;;    [ ] Model the entire CPU state as a single immutable map structure.
;;       [✔] 16-bit program counter (PC)
;;       [✔] sixteen 8-bit registers (V0-VF)
;;       [✔] VF register flags
;;       [✔] 16-bit index register (I)
;;       [✔] 16-bit 16-byte stack with 8-bit stack pointer (SP) for functions
;;       [✔] 8-bit delay timer
;;       [✔] 8-bit sound timer
;;          [ ] decrease at 1 tick per 60Hz, when above 0 sound plays a beep
;;       [✔] 64x32 bit frame buffer
;;       [✔] 4096 bytes of addressable memory, program starts at 0x200, 0x000 to 0x1FF has the interpreter, 0x000 to 0x080 for fonts
;;       [ ] input (halt until key release)
;;    [ ] Implement a pure Fetch-Decode-Execute pipeline for all 35 opcodes. every instruction is on an even PC index.
;;       [✔] shift shifts Vy and copies to Vx / Vx is shfited in-place switch
;;       [✔] I++ after read or write of registes / I is unmodified switch
;;       [✔] 0nnn ignored
;;       [✔] 00E0 clear screen
;;       [✔] 00EE return subroutine (from stack)
;;       [✔] 1nnn unconditional jump
;;       [✔] 2nnn call subroutine (jump, but save PC to stack)
;;       [✔] 3xkk skip next instruction if Vx == kk (eq)
;;       [✔] 4xkk skip next instruction if Vx != kk (neq)
;;       [✔] 5xy0 skip next instruction if Vx == Vy (eq reg)
;;       [✔] 6xkk Vx = kk
;;       [✔] 7xkk Vx = Vx + kk
;;       [✔] 8xy0 Vx = Vy
;;       [✔] 8xy1 Vx = Vx | Vy
;;       [✔] 8xy2 Vx = Vx & Vy
;;       [✔] 8xy3 Vx = Vx ^ Vy
;;       [✔] 8xy4 Vx = Vx + Vy, VF = carry (0 or 1)
;;       [✔] 8xy5 Vx = Vx - Vy, VF = NOT borrow (1 or 0)
;;       [✔] 8xy6 Vx = Vx SHR 1, VF = LSB Vx
;;       [✔] 8xy7 Vx = Vy - Vx, VF = NOT borrow (1 or 0)
;;       [✔] 8xyE Vx = Vx SHL 1, VF = MSB Vx
;;       [✔] 9xy0 skip next instruction if Vx != Vy (neq reg)
;;       [✔] Annn I = nnn
;;       [✔] Bnnn JMP nnn + V0
;;       [✔] Bxnn JMP nn + Vx (legacy)
;;       [✔] Cxkk Vx = random AND kk
;;       [-] Dxyn display n-byte sprite starting at I at (Vx, Vy), VF = collision (0 or 1)
;;       [-] Ex9E skip next instruction if key == Vx is pressed
;;       [-] ExA1 skip next instruction if key != Vx is pressed
;;       [✔] Fx07 Vx = delay timer value
;;       [-] Fx0A Vx = value of key pressed (halt until key pressed and released)
;;       [✔] Fx15 delay timer value = Vx
;;       [✔] Fx18 sound timer value = Vx
;;       [✔] Fx1E I = I + Vx
;;       [✔] Fx29 I = sprite for Vx value (in the font)
;;       [✔] Fx33 store BCD of Vx in I, I+1, and I+2
;;       [✔] Fx55 store registers from V0 to Vx in memory starting from I
;;       [✔] Fx65 read registers from V0 to Vx from memory starting from I
;;    [ ] Render the 64x32 monochrome display matrix (separate buffer) using a functional loop.
;;       [ ] jesus christ
;;       [ ] wrap horiz clip vert
;;       [ ] 60Hz per frame btw

(def font-sprites
  [0xF0 0x90 0x90 0x90 0xF0 ;; 0
   0x20 0x60 0x20 0x20 0x70 ;; 1
   0xF0 0x10 0xF0 0x80 0xF0 ;; 2
   0xF0 0x10 0xF0 0x10 0xF0 ;; 3
   0x90 0x90 0xF0 0x10 0x10 ;; 4
   0xF0 0x80 0xF0 0x10 0xF0 ;; 5
   0xF0 0x80 0xF0 0x90 0xF0 ;; 6
   0xF0 0x10 0x20 0x40 0x40 ;; 7
   0xF0 0x90 0xF0 0x90 0xF0 ;; 8
   0xF0 0x90 0xF0 0x10 0xF0 ;; 9
   0xF0 0x90 0xF0 0x90 0x90 ;; A
   0xE0 0x90 0xE0 0x90 0xE0 ;; B
   0xF0 0x80 0x80 0x80 0xF0 ;; C
   0xE0 0x90 0x90 0x90 0xE0 ;; D
   0xF0 0x80 0xF0 0x80 0xF0 ;; E
   0xF0 0x80 0xF0 0x80 0x80]);; F

(def program-start 0x200) ;program start
(def initial-registers (vec (repeatedly 16 #(rand-int 256)))) ;initial registers are random
(def initial-memory
  (into font-sprites (repeatedly 4016 #(rand-int 256)))) ;font sprites, then random
(def initial-stack (vec (repeatedly 16 #(rand-int 65536)))) ;initial stack contents are random

(def start-state
  {:pc program-start
   :registers initial-registers
   :memory initial-memory
   :I 0
   :stack initial-stack
   :SP 0
   :delay 0
   :sound 0
   :screen screen/start-screen
   :keys keys/start-keys
   :quirks {:logic-clears-VF? false
            :shift-uses-Vy? false
            :JMO-uses-Vx? false
            :clip-top-sprites? true ;not implemented (DRW in general)
            :dump-and-load-restore-I? true}})

;;00E0
;;00EE
;;0nnn
;;1nnn
;;2nnn
;;3xkk
;;4xkk
;;5xy0
;;6xkk
;;7xkk
;;8xy0
;;8xy1
;;8xy2
;;8xy3
;;8xy4
;;8xy5
;;8xy6
;;8xy7
;;8xyE
;;9xy0
;;Annn
;;Bnnn
;;Bxnn
;;Cxkk
;;Dxyn
;;Ex9E
;;ExA1
;;Fx07
;;Fx0A
;;Fx15
;;Fx18
;;Fx1E
;;Fx29
;;Fx33
;;Fx55
;;Fx65

(defn fetch
  "Fetches an instruction to execute from RAM at the current PC."
  [current-state]
  (let [memory (:memory current-state)
        pc (:PC current-state)
        opcode (bit-or (bit-shift-left (get memory pc) 8) (get memory (inc pc))) ;combine 2 8-bit into 1 16-bit
        fetched-state (assoc current-state :PC (+ pc 2))] ;increase pc by two
    [fetched-state opcode]))

(defn execute
  "Executes a given instruction and returns a new cpu state."
  [fetched-state opcode]
  ((case opcode
     0x00E0 (instr/CLS fetched-state)
     0x00EE (instr/RET fetched-state))))

(defn tick! ;swap! is the only way to mutate things, and atoms are the only mutable bit, ! marks a function as mutating
  "Ticks the VM once."
  [state]
  (swap! state (fn [current-state] ;first argument is the atom, by value, always, that's swap!
                 (let [[stepped-state opcode] (fetch current-state)] ;you can do multiple-value-bind for free in clj
                   (execute stepped-state opcode))))) ;step once
