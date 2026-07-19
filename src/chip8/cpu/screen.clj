(ns chip8.cpu.screen)

(def start-screen (vec (repeatedly 2048 #(rand-int 2))))