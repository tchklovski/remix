(ns remix.test.core
  (:use :reload [remix.core])
  (:use [clojure.test]))

(deftest test-split-by-syllables
  ;; see eg http://www.phonicsontheweb.com/syllables.php
  (are [in out]
       (= (split-by-syllables in) out)

       ;; splits between volwels only
       "car" ["car"]
       "night" ["night"]
       "sports" ["sports"]
       "beautiful" ["beau" "ti" "ful"]
       ;; but not trailing "e"
       "care" ["care"]
       "hero" ["he" "ro"]
       "balloon" ["bal" "loon"]
       "happen" ["hap" "pen"]
       ;; middle consonant
       "motor"  ["mo" "tor"]
       ;; middle consonant, trailing e
       "police" ["po" "lice"]
       ;;
       ;; KNOWN ISSUES
       ;; (google "define x" to see syllables)
       ;;
       ;; "mary" ["mar" "y"]
       ;;
       ;; prefix and suffix:
       ;; "unionize" ["un" "ion" "ize"]
       ;;
       ;; middle consonant exception b/c 1st has short sound:
       ;; "cabin"  ["cab" "in"]
       ;;
       ;; compound words
       ;; "fireworks" ["fire" "works"]
       ;; "sportscar" ["sports" "car"]
       ))

(deftest test-new-word
  (is (string? (new-word :hawaiian-places))))
