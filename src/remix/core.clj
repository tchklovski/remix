(ns remix.core
  (:require [clojure.string :as s]))

;; just for fun -- read in a file of words, break them up by eg syllables,
;; and remix the syllables into new words of similar length

;; given a sample of words, create a model which will generate words like them

(defn read-lines [uri]
  (-> uri slurp s/split-lines))

(defn cleanup-lines [lines]
  (let [comment? #(= (first %) \;)]
    (remove comment?
            (map s/trim lines))))

(def wordsets [:hawaiian-places :boys-names :girls-names])

(def word-files
  (let [full-path #(str (first (s/split *file* #"/src/" 2)) "/data/" (name %) ".txt")]
    (into {} (for [ws wordsets] [ws (full-path ws)]))))

(defn read-words
  ([lines]
      (let [first-word #(first (s/split % #"[|,\s]" 2))]
        (map (comp s/lower-case first-word)
             lines))))

(defn words [key]
  (-> key word-files read-lines cleanup-lines read-words))

;; FIXME -- this is far from accurate
;; see http://www.phonicsontheweb.com/syllables.php
(defn split-by-syllables [word]
  ;; consonants: bcdfghjklmnpqrstvwxz
  ;; vowels: aeiouy
  (s/split word #"(?<=[aeiouy])(?=.*[aeiouy])"))

(def wordify #(s/capitalize (apply str %)))

(defn remix
  "\"Remix\" existing items into fanciful new ones"
  [items splitter remixer joiner]
  (joiner (remixer (map splitter items))))

(defn remixers
  "Each value fn, given a collection of collections, create a new one single collection"
  {:uniform-sampler
   (fn [pieces-seq]
     (let [all-pieces (apply concat pieces-seq)]
       (repeatedly (count (rand-nth pieces-seq)) #(rand-nth all-pieces))))

   :positional-sampler
   (fn [pieces-seq]
     (for [k (range 0 (count (rand-nth pieces-seq)))]
       (nth (rand-nth pieces-seq) k nil)))})

(defn remix-words
  [words-in remixer]
  (remix words-in split-by-syllables remixer wordify))

(defn print-n [n f]
  (dorun (map println (repeatedly n f))))

(defn print-remixes [n dataset-name remixer-name]
  (print-n n #(remix-words (words dataset-name)
                           (remixers remixer-name))))

(comment
  (print-remixes 20 :hawaiian-places :positional-sampler)
  (print-remixes 20 :boys-names :positional-sampler)
  (print-remixes 20 :girls-names :positional-sampler))