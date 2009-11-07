(ns boggle-solver
  ;(:require )
  (:use
   clojure.set
   (clojure.contrib str-utils duck-streams seq-utils))
  ;(:import )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boggle Solver                        
;; By James Swift                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Util
; (set! *warn-on-reflection* true)

(defn not-nil? [x]
  (not (nil? x)))

;; Dictionary

(def wordfile "/usr/share/dict/words")

(defn prep-words [x]
  (map #(.toLowerCase #^String %) x))

(defn add-to-trie [trie x]
  (assoc-in trie x {:t true}))

(defn in-trie
  "Searches trie for x and returns a vector of two booleans.
  The first indicates if x is a complete entry.
  The second if there are futher children on that branch"
  [trie x]
  (let [res (get-in trie x)]
    [(:t res false) res]))

(defn make-trie [x]
  (reduce add-to-trie {} x))

(defn create-dict [f]
  (make-trie (prep-words (read-lines f))))

;; Dice and Tray

(defstruct die :position :letters :up)
(def dice-letters [["lrytte"  "vthrwe"  "eghwne"  "seotis"]
		   ["anaeeg"  "idsytt"  "oattow"  "mtoicu"] 
		   ["afpkfs"  "xlderi"  "hcpoas"  "ensieu"]
		   ["yldevr"  "znrnhl"  "nmiqhu"  "obbaoj"]])

(defn shake-dice-tray []
  (for [y (range 4)]
    (for [x (range 4)]
      (let [l ((dice-letters y) x)]
	(struct die [x y] l (nth l (rand-int 6)))))))

(defn get-at [x y board]
  (nth (nth board y) x))

(defn in-bounds? [x y v]
  (if (and  (> x -1)
            (< x (count v))
            (> y -1)
            (< y (count (nth v 0))))
    true))

(defn get-neighbours [place v]
  (let [[x y] place]
    (for [y1 (range (dec y) (+ y 2))
	  x1 (range (dec x) (+ x 2)) 
	  :when (and (not (and (= x1 x) (= y1 y)))
		     (in-bounds? x1 y1 v))]
      (get-at x1 y1 v))))

(defn pprint-board [board]
  (doseq [row board]
    (println
     (for [dice row]
       (:up dice)))))

;; Search Algorithm

(defn crosses-path? [point path]
  (some #(= % point) path))

(defn search-from [die board dict cur-word path]
  (let [cur-word (concat cur-word [(:up die)])
	path (conj path (:position die))]
    (let [[match rem] (in-trie dict cur-word)]
      (conj (if match [(apply str cur-word)])
	    (if (not-empty rem)
	      (for [next-die (get-neighbours (:position die) board) 
		    :when (not (crosses-path? (:position next-die) path))]
		(search-from next-die board dict cur-word path)))))))

(defn get-words [board dict]
  (sort
   (distinct
    (flatten
     (for [row board]
       (for [die row]
	 (search-from die board dict [] [])))))))

;; Example

; (in-ns 'boggle-solver)
; (def dict (create-dict wordfile))
; (def a-board (shake-dice-tray))

; (pprint-board a-board)
; (t e h o)
; (e y t m)
; (a l c n)
; (y n u a)
; nil

; (get-words a-board dict)
; (nil "a" "acle" "act" "aculea" "acyl" "ae" "al" "alca" "ale" "alee" "aln" "alt" "altho" "alto" "aly" "an" "ant" "ante" "any" "aula" "aulae" "aulete" "aunt" "auntly" "ay" "aye" "aylet" "c" "ca" "can" "cant" "cantle" "cantlet" "canto" "canty" "canun" "caul" "clan" "clay" "clean" "clee" "cly" "culet" "cult" "cuna" "cyan" "e" "ea" "ean" "eel" "eely" "eh" "el" "elt" "ethyl" "etna" "ey" "eyalet" "eye" "h" "he" "heel" "het" "hey" "ho" "hot" "hotly" "hyla" "hyle" "l" "la" "laet" "lan" "lay" "lea" "lean" "lee" "leet" "let" "lete" "ley" "lu" "lucan" "lucy" "luna" "lunacy" "lunt" "ly" "lye" "m" "mho" "mo" "mot" "mote" "motet" "motey" "moth" "mothy" "motley" "n" "na" "nae" "nael" "nay" "nth" "nu" "nul" "nun" "nuncle" "nunlet" "o" "oh" "ohm" "om" "t" "te" "tea" "teal" "tean" "tee" "teel" "teet" "teeth" "teethy" "teety" "telt" "telyn" "tete" "tetel" "teth" "th" "the" "thee" "theet" "they" "tho" "thy" "to" "tom" "tye" "tyee" "tyt" "tyto" "u" "uca" "ula" "ule" "ulna" "ulnae" "un" "una" "unact" "unal" "unca" "unclay" "uncle" "unclean" "unlay" "unlet" "unto" "y" "ya" "yale" "yan" "ye" "yea" "yean" "yee" "yeel" "yelt" "yet" "yeth" "yn")


