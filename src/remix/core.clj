(ns remix.core
  (:require [clojure.string :as s]
            [sosueme.io :as io]))

;; An experiment in frequency analysis and generation.
;; Read in a file of words, break them up by eg syllables,
;; and remix the syllables into new words of similar length.

;; ## General file and line reading
(defn read-lines [uri]
  (-> uri io/slurp-cp s/split-lines))

(defn cleanup-lines [lines]
  (let [comment? #(= (first %) \;)]
    (remove comment?
            (map s/trim lines))))

(defn tokenize
  "Return a (possibly empty) lazy sequence of tokens of str."
  [str]
  (sequence (re-seq #"\w+" str)))

(defn- read-words*
  [uri]
  (let [first-word  #(first (tokenize %))
        keep-first-words #(keep first-word %)]
    (-> uri read-lines cleanup-lines keep-first-words)))

(def read-words
  "Reads in first word of every line of uri (a file or a url -- anything that can be slurped)"
  (memoize read-words*))

;; FIXME -- this is far from accurate
;; Here are some resources:
;;
;; - [syllabilization rules](http://www.phonicsontheweb.com/syllables.php)
;; - [syllable dictionary](http://www.languagebits.com/books-and-references/)
;; - [python dict search](http://www.languagebits.com/phonetics-english/fonrye-english-phonetic-syllable-dictionary-search/)
(defn split-by-syllables
  [word]
  ;; TODO: handle prefix, suffix
  ;; eg prefix: (?<=^(?:pre|un|dis|re|mis|im|bi|de)) (?=.*[aeiouy])
  (s/split word #"(?xi)
    (
        # divide before a middle consonant but
        # don't split off final e in eg ice
    (?<=[aeiouy])
     (?=[^aeiouy](?:[aiouy]|e.))|
        # split on multiple consonants right before last one
    (?<=[aeiouy][^aeiouy]{1,4})
     (?=[^aeiouy](?:[aiouy]|e.))
    )"))

(def join-syllables "A joiner for remixing words" s/join)

(defn remix
  "\"Remix\" existing items into fanciful new ones. To remix something, you need:

- input items
- a splitter to map each to a sequence of pieces
- a remixer of the collection of piece sequences
- a joiner to map the remixer output back into the original domain"
  [items splitter remixer joiner]
  (joiner (remixer (map splitter items))))

(defn random-length [pieces-seq]
  (count (rand-nth pieces-seq)))

(def remixers
  "A remixer, given a collection of sequences, \"remixes\" by sampling from the sequences to create a novel sequence which resembles those in the input collection."
  {:uniform-sampler
   ;; Takes random pieces from all items
   ;; Number of pieces is picked from a random input
   (fn [pieces-seq]
     (let [all-pieces (apply concat pieces-seq)
           num-pieces (random-length pieces-seq)]
       (repeatedly num-pieces
                   #(rand-nth all-pieces))))

   :positional-sampler
   ;; Takes a piece from nth position of random items
   ;; good if items in different positions differ.
   ;; Number of pieces is picked from a random input
   ;; If sampling from an item that's too short,
   ;; just allows a nil, which will disappear.
   (fn [pieces-seq]
     (for [k (range 0 (random-length pieces-seq))]
       (nth (rand-nth pieces-seq) k nil)))})

(defn remix-words
  "A remixer for the words domain. Assumes we're remixing proper nouns, so capitalizes the output."
  [words-in remixer]
  (s/capitalize (remix words-in split-by-syllables remixer join-syllables)))

(defn print-n
  "Presentation utility to print the result of running n times the
   no-args function f"
  [n f]
  (dorun (map println (repeatedly n f))))

;; ## Sample datasets
(def wordsets [:hawaiian-places :boys-names :girls-names])

;; See /data/README.md for data sources
(def word-files
  (let [full-path #(str "data/" (name %) ".txt")]
    (into {} (for [ws wordsets] [ws (full-path ws)]))))

(defn new-word [dataset-name]
  (remix-words (read-words (dataset-name word-files))
               (:positional-sampler remixers)))

(defn print-remixes
  "Print n remixes of items from dataset-name"
  [n dataset-name remixer-name]
  (print-n n #(remix-words (read-words (dataset-name word-files))
                           (remixer-name remixers))))

(comment
  (new-word :hawaiian-places)
  (new-word :boys-names)
  (new-word :girls-names)
  (print-remixes 20 :hawaiian-places :positional-sampler)
  (print-remixes 20 :boys-names :positional-sampler)
  (print-remixes 20 :girls-names :positional-sampler))