(ns chip8.screen)

(def start-screen (vec (repeat 2048 false))) ;64x32

(def blank start-screen) ;blank screen for CLS, alias for start-screen

(defn drw-xy
  "Toggles a pixel at (x,y) if the input is 1, returns the new screen and if a pixel was erased."
  [screen x y bit]
  (if (or (> x 63) (> y 31) (neg? x) (neg? y))
    [screen false] ;if out of bounds, ignore the pixel, always
    (if (= 0x1 bit)
      (let [index (+ (* y 64) x)
            current-pixel? (get screen index)
            new-screen (assoc screen index (not current-pixel?))]
        [new-screen current-pixel?])
      [screen false])))

(defn get-xy
  "Toggles a pixel at (x,y) if the input is 1, returns the new screen and if a pixel was erased."
  [screen x y]
  (when (or (> x 63) (> y 31) (neg? x) (neg? y)) (throw (ex-info "X or Y values out of bounds, clip/wrap before asking to 0..63 and 0..31." {:x x :y y})))
  (let [index (+ (* y 64) x)]
    (get screen index)))

;;;TODO: normalise this system so (render screen) is in here instead of calling the specific ones
