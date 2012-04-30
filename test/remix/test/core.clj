(ns remix.test.core
  (:use [remix.core])
  (:use [clojure.test]))

(deftest test-split-by-syllables
  (are [in out]
       (= (split-by-syllables in) out)

       "car" ["car"]
       "hero" ["he" "ro"]
       ;; known issues (google "define x" to see syllables)
       ;; "mary" ["mar" "y"]
       ;; "police" ["po" "li" "ce"]
       ;; "fireworks" ["fire" "works"]
       ))

(deftest test-new-word
  (is (string? (new-word :hawaiian-places))))
