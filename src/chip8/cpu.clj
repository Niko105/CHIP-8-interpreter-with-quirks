(ns chip8.cpu
  (:require [chip8.screen :as screen]
            [chip8.keyboard :as keys]
            [chip8.cpu.instructions :as instr]))
;; [ ] 1.9: CHIP-8 Virtual Machine & Interpreter (Clojure)
;;    [ ] Model the entire CPU state as a single immutable map structure.
;;       [✔] 16-bit program counter (PC)
;;       [✔] sixteen 8-bit registers (V0-VF)
;;       [✔] VF register flags
;;       [✔] 16-bit index register (I)
;;       [✔] 16-bit 16-byte stack with 8-bit stack pointer (SP) for functions
;;       [✔] 8-bit delay timer
;;       [✔] 8-bit sound timer
;;          [✔] decrease at 1 tick per 60Hz, when above 0 sound plays a beep
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
;;       [✔] Dxyn display n-byte sprite starting at I at (Vx, Vy), VF = collision (0 or 1)
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
;;       [ ] Extra opcodes for SCHIP compatibility (7)
;;    [✔] Render the 64x32 monochrome display matrix (separate buffer) using a functional loop.
;;       [✔] jesus christ
;;       [✔] wrap horiz clip vert
;;       [✔] 60Hz per frame btw

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
(def memory-size 4096)
(def font-size (count font-sprites))
(def initial-registers (vec (repeat 16 0))) ;16 8-bit registers
(def initial-memory
  (into font-sprites (repeatedly (- memory-size font-size) #(rand-int 255)))) ;font sprites, then random data (uninitialised), 4096 bytes of noise
(def initial-stack (vec (repeat 16 0))) ;16 16-bit registers

(def start-state
  {:PC program-start
   :registers initial-registers
   :memory initial-memory
   :I 0
   :stack initial-stack
   :SP 0
   :delay 0
   :sound 0
   :screen screen/start-screen
   :high-res false ;for SCHIP
   :keys keys/start-keys
   :quirks {:logic-clears-VF? false
            :shift-uses-Vy? false
            :JMO-uses-Vx? false
            :clip-top-sprites? true
            :clip-side-sprites? true
            :dump-and-load-restore-I? true
            :I-overflow-is-tracked? false}})

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
  (let [primary (bit-shift-right opcode 12)
        Vx (bit-and (bit-shift-right opcode 8) 0x0F)
        Vy (bit-and (bit-shift-right opcode 4) 0x0F)
        nnn (bit-and opcode 0x0FFF)
        kk (bit-and opcode 0x00FF)
        n (bit-and opcode 0x000F)]
    (case primary
      0x0 (case kk
            0xE0 (instr/CLS fetched-state)
            0xEE (instr/RET fetched-state)
            (instr/SYS fetched-state))
      0x1 (instr/JMP fetched-state nnn)
      0x2 (instr/CALL fetched-state nnn)
      0x3 (instr/EQ fetched-state Vx kk)
      0x4 (instr/NEQ fetched-state Vx kk)
      0x5 (instr/EQR fetched-state Vx Vy)
      0x6 (instr/SET fetched-state Vx kk)
      0x7 (instr/ADDI fetched-state Vx kk)
      0x8 (case n
            0x0 (instr/LXY fetched-state Vx Vy)
            0x1 (instr/OR fetched-state Vx Vy)
            0x2 (instr/AND fetched-state Vx Vy)
            0x3 (instr/XOR fetched-state Vx Vy)
            0x4 (instr/ADD fetched-state Vx Vy)
            0x5 (instr/SUB fetched-state Vx Vy)
            0x6 (instr/SHR fetched-state Vx Vy)
            0x7 (instr/SUBN fetched-state Vx Vy)
            0xE (instr/SHL fetched-state Vx Vy))
      0x9 (instr/NEQR fetched-state Vx Vy)
      0xA (instr/LDI fetched-state nnn)
      0xB (instr/JMO fetched-state nnn Vx) ;will pick one
      0xC (instr/RND fetched-state Vx kk)
      0xD (instr/DRW fetched-state Vx Vy n) ;actually x and y
      0xE (case kk
            0x9E (instr/KEY fetched-state Vx)
            0xA1 (instr/KEN fetched-state Vx))
      0xF (case kk
            0x07 (instr/LXD fetched-state Vx)
            0x0A (instr/WKP fetched-state Vx)
            0x15 (instr/LDX fetched-state Vx)
            0x18 (instr/LSX fetched-state Vx)
            0x1E (instr/AXI fetched-state Vx)
            0x29 (instr/LFI fetched-state Vx)
            0x33 (instr/BCD fetched-state Vx)
            0x55 (instr/SRM fetched-state Vx)
            0x65 (instr/LRM fetched-state Vx)))))

(defn beep? "Simple predicate to see if the beeper should beep." [state] (pos? (:sound @state)))

(defn tick-timers!
  "Helper functions to tick the timers down by one, should run once every 60Hz. Returns a new cpu state."
  [state]
  (swap! state (fn [current-state]
                 (-> current-state
                     (update :delay #(if (pos? %) (dec %) %)) ;updates delay by applying a function to it, better than assoc since it's a direct function
                     (update :sound #(if (pos? %) (dec %) %)))))) ;%/%1 is first argument, %2 is second, %3 is third, %& is rest

(defn tick! ;swap! is the only way to mutate things, and atoms are the only mutable bit, ! marks a function as mutating
  "Ticks the VM once."
  [state]
  (swap! state (fn [current-state] ;first argument is the atom, by value, always, that's swap!
                 (let [[fetched-state opcode] (fetch current-state)] ;you can do multiple-value-bind for free in clj]
                   (execute fetched-state opcode))))) ;step once
