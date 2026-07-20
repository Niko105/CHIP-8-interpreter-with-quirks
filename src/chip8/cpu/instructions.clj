(ns chip8.cpu.instructions)

(defn- stack-push
  "Pushes an element into the stack, returns the new state."
  [state element]
  (if (< (:SP state) 16)
    (assoc state
           :stack (assoc (:stack state) (:SP state) element)
           :SP (inc (:SP state)))
    (throw (ex-info "Stack out of bounds" (select-keys state [:SP :stack])))))

(defn- stack-pop
  "Pops an element off the stack, returns the new element and new state."
  [state]
  (if (> (:SP state) 0)
    (let [sp (dec (:SP state))]
      [(get (:stack state) sp) (assoc state :SP sp)])
    (throw (ex-info "Stack out of bounds" (select-keys state [:SP :stack])))))

(defn- bind-8
  "Binds a number to 0..255 through AND. For most registers."
  [num]
  (bit-and num 0xFF))

(defn- bind-16
  "Binds a number to 0..4096 through AND. For the I and PC register."
  [num]
  (bit-and num 0xFFFF))

(defn SYS ;0nnn
  "Ignored on modern systems."
  [state]
  state) ;just return state

(defn CLS ;00E0
  "CLears the Screen."
  [state]
  (assoc state :screen (repeat 2048 0))) ;blank the screen

(defn RET ;00EE
  "RETurn subroutine."
  [state]
  (let [[addr stack-state] (stack-pop state)] ;pop address from stack
    (assoc stack-state :PC addr))) ;update pc

(defn JMP ;1nnn
  "JuMPs to an adddress unconditionally."
  [state addr]
  (assoc state :PC addr)) ;set pc

(defn CALL ;2nnn
  "CALLs a subroutine."
  [state addr]
  (assoc (stack-push state (:PC state)) :PC addr)) ;push pc to stack and set pc

(defn EQ ;3xkk
  "Skips one instruction if Vx is EQual to kk."
  [state Vx kk]
  (if (= (get (:registers state) Vx) kk)
    (assoc state :PC (+ (:PC state) 2))
    state))

(defn NEQ ;4xkk
  "Skips one instruction if Vx is Not EQual to kk."
  [state Vx kk]
  (if (not= (get (:registers state) Vx) kk)
    (assoc state :PC (+ (:PC state) 2))
    state))

(defn EQR ;5xy0
  "Skips one instruction if Vx is EQual to another Register."
  [state Vx Vy]
  (if (= (get (:registers state) Vx) (get (:registers state) Vy))
    (assoc state :PC (+ (:PC state) 2))
    state))

(defn SET ;6xkk
  "SETs a register to kk."
  [state Vx kk]
  (assoc-in state [:registers Vx] kk)) ;needs assoc-in since :registers is nested

(defn ADDI ;7xkk
  "ADDs Immediate to Vx."
  [state Vx kk]
  (assoc-in state [:registers Vx] (bind-8 (+ (get (:registers state) Vx) kk)))) ;using AND so it's bounded 0..255 and wraps

(defn LXY ;8xy0
  "Load VX with VY."
  [state Vx Vy]
  (assoc-in state [:registers Vx] (get (:registers state) Vy)))

(defn- op-registers
  "Function to build an operaton between registers and storing in Vx."
  [state Vx Vy op]
  (assoc-in state [:registers Vx] (op (get (:registers state) Vx) (get (:registers state) Vy))))

(defn- clear-VF-maybe-if-the-flag-is-set
  "Looks at the :logic-clears-VF? flag and clears VF if it's true."
  [state]
  (if (get-in state [:quirks :logic-clears-VF?])
    (assoc-in state [:registers 0xF] 0)
    state))

(defn OR ;8xy1
  "Bitwise OR between two registers."
  [state Vx Vy]
  (clear-VF-maybe-if-the-flag-is-set (op-registers state Vx Vy bit-or)))

(defn AND ;8xy2
  "Bitwise AND between two registers."
  [state Vx Vy]
  (clear-VF-maybe-if-the-flag-is-set (op-registers state Vx Vy bit-and)))

(defn XOR ;8xy3
  "Bitwise XOR between two registers."
  [state Vx Vy]
  (clear-VF-maybe-if-the-flag-is-set (op-registers state Vx Vy bit-xor)))

(defn ADD ;8xy4
  "ADDs two registers and stores the result in Vx. Sets VF to 1 if carry, else 0."
  [state Vx Vy]
  (let [added-state (op-registers state Vx Vy +) ;perform the sum
        sum (get (:registers added-state) Vx) ;get the sum's value
        carry? (> sum 0xFF) ;does it carry?
        safe-sum (bind-8 sum)] ;bound sum
    (-> added-state ;threading added-state through two assocs
        (assoc-in [:registers Vx] safe-sum) ;first set the sum so it's bounded
        (assoc-in [:registers 0xF] (if carry? 1 0))))) ;then update VF (index 0xF)

(defn SUB ;8xy5
  "SUBtracts two registers and stores the result in Vx. Sets VF to 0 if borrow, else 1."
  [state Vx Vy]
  (let [added-state (op-registers state Vx Vy -)
        sub (get (:registers added-state) Vx)
        borrorw? (< sub 0)
        safe-sub (bind-8 sub)]
    (-> added-state
        (assoc-in [:registers Vx] safe-sub)
        (assoc-in [:registers 0xF] (if borrorw? 0 1)))))

(defn SHR ;8xy6
  "SHifts one register Right by one depending on the emulator configuration. Sets VF to the LSB of Vx."
  [state Vx Vy]
  (let [source (if (get-in state [:quirks :shift-uses-Vy?]) Vy Vx)
        value (get-in state [:registers source])]
    (-> state
        (assoc-in [:registers 0xF] (if (bit-test value 0) 1 0))
        (assoc-in [:registers Vx] (bind-8 (bit-shift-right value 1))))))

(defn SUBN ;8xy7
  "SUBtracts two registers and stores the result in Vx. Sets VF to 0 if borrow, else 1. This opcode performs Vy-Vx."
  [state Vx Vy]
  (let [added-state (op-registers state Vx Vy #(- %2 %1)) ;swap the operands
        sub (get (:registers added-state) Vx)
        borrorw? (< sub 0)
        safe-sub (bind-8 sub)]
    (-> added-state
        (assoc-in [:registers Vx] safe-sub)
        (assoc-in [:registers 0xF] (if borrorw? 0 1)))))

(defn SHL ;8xyE
  "SHifts one register Left by one depending on the emulator configuration. Sets VF to the MSB of Vx."
  [state Vx Vy]
  (let [source (if (get-in state [:quirks :shift-uses-Vy?]) Vy Vx)
        value (get-in state [:registers source])]
    (-> state
        (assoc-in [:registers 0xF] (if (bit-test value 7) 1 0))
        (assoc-in [:registers Vx] (bind-8 (bit-shift-left value 1))))))

(defn NEQR ;9xy0
  "Skips one instruction if Vx is EQual to another Register."
  [state Vx Vy]
  (if (not= (get (:registers state) Vx) (get (:registers state) Vy))
    (assoc state :PC (+ (:PC state) 2))
    state))

(defn LDI ;Annn
  "LoaDs the I register with an address."
  [state addr]
  (assoc state :I addr))

(defn JMO ;Bnnn/Bxnn
  "JuMps to nnn + Offset, the offset being V0/Vx depending on config."
  [state nnn Vx]
  (let [reg (if (get-in state [:quirks :JMO-uses-Vx?]) Vx 0)]
    (assoc state :PC (bind-16 (+ nnn (get-in state [:registers reg]))))))

(defn RND ;Cxkk
  "Generates a RaNDom byte and performs bitwise AND using kk, stores in Vx."
  [state Vx kk]
  (assoc-in state [:registers Vx] (bit-and (rand-int 256) kk)))

;;TODO: find the will to make this function
(defn DRW ;Dxyn
  "DRaW on the display an n-byte sprite starting at I at (Vx, Vy). VF is used as a collision flag (0 or 1)."
  [state Vx Vy n]
  (print "Unimplemented"))

;;TODO: do these functions
(defn KEY ;Ex9E
  "Skip next instruction if the KEY with value Vx is pressed. (non blocking)"
  [state Vx]
  (print "Unimplemented"))

(defn KEN ;ExA1
  "Skip next instruction if the KE with value Vx is Not pressed. (non blocking)"
  [state Vx]
  (print "Unimplemented"))

(defn LXD ;Fx07
  "Loads VX with the Delay timer's value."
  [state Vx]
  (assoc-in state [:registers Vx] (:delay state)))

;;TODO: do this
(defn WKP ;Fx0A
  "Waits for a Key with value Vx to be Pressed and released. (blocking)"
  [state Vx]
  ())

(defn LDX ;Fx15
  "Loads the Delay timer with VX."
  [state Vx]
  (assoc state :delay (get-in state [:registers Vx])))

(defn LSX ;Fx18
  "Loads the Sound timer with VX."
  [state Vx]
  (assoc state :sound (get-in state [:registers Vx])))

(defn AXI ;Fx1E
  "Adds VX to I, stores in I."
  [state Vx]
  (assoc state :I (bind-16 (+ (:I state) (get (:registers state) Vx)))))

(defn LFI ;Fx29
  "Loads the Font sprite for the Vx character in I."
  [state Vx]
  (assoc state :I (* 5 (get-in state [:registers Vx])))) ;sprites are 5 bytes each, starting at ram 0x000, also ignore out of bounds errors per spec

(defn BCD ;Fx33
  "Stores the BCD representation of Vx into I, I+1, and I+2."
  [state Vx]
  (let [v (get-in state [:registers Vx]) ;can't use state with ->
        i (:I state)]
    (-> state
        (assoc-in [:memory i] (quot v 100)) ;hundreds
        (assoc-in [:memory (+ i 1)] (quot (rem v 100) 10)) ;tens
        (assoc-in [:memory (+ i 2)] (rem v 10))))) ;ones

(defn SRM ;Fx55
  "Stores Registers V0--Vx into Memory starting from I."
  [state Vx]
  (let [i (:I state)
        regs (range (inc Vx))
        reduced-state (reduce (fn [s r] ;apply a function to a list, cause there's NO MUTATING LOOPS, reduce is still sequential tho
                                (assoc-in s [:memory (+ i r)] (get-in s [:registers r]))) ;so i guess there are
                              state regs)] ;they're just pretty well hidden in the compilation
    (if (get-in state [:quirks :dump-and-load-restore-I?]) (assoc reduced-state :I i) reduced-state)))

(defn LRM ;Fx65
  "Loads Registers V0--Vx from Memory starting from I."
  [state Vx]
  (let [i (:I state)
        regs (range (inc Vx))
        reduced-state (reduce (fn [s r]
                                (assoc-in s [:registers r] (get-in s [:memory (+ i r)])))
                              state regs)]
    (if (get-in state [:quirks :dump-and-load-restore-I?]) (assoc reduced-state :I i) reduced-state)))
