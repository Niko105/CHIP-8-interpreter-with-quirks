(ns chip8.input.terminal
  (:import (org.jline.terminal TerminalBuilder)))

(defn- make-terminal [] (-> (TerminalBuilder/builder) ;makes a terminal builder (that's a static method)
                            (.system true) ;(.system (TerminalBuilder/builder) true), TerminalBuilder.builder().system(true)
                            (.build))) ;hooks to the running terminal and exposes it as an object

(def what-once-was (atom nil))
(defonce terminal (make-terminal)) ;one global terminal, don't overwrite if exists
(defonce reader (.reader terminal)) ;terminal's input stream, no need for a writer

(defn enable-raw! []
  (when-not @what-once-was
    (let [attributes (.enterRawMode terminal)] ;sets the terminal into raw mode and returns the original attributes
      (reset! what-once-was attributes))))

(defn disable-raw! []
  (when @what-once-was
    (.setAttributes terminal @what-once-was) ;restores the terminal 
    (reset! what-once-was nil))) ;disables the flag/value

(defn terminal-raw? [] (boolean @what-once-was))

(defn get-key []
  (let [c (.read reader 1)]
    (when (>= c 0) c)))

(defn close-and-restore-terminal! []
  (disable-raw!)
  (.close terminal))
