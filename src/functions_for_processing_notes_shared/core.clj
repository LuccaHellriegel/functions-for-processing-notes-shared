(ns functions-for-processing-notes-shared.core
  (:require [com.rpl.specter :refer :all]
            [clojure.data :refer :all]
            [clojure.java.io :as io]
            [babashka.fs :as fs]
            [clojure.string :as str]))

;; Make sure you define your two paths as duplicates so you aren't messing with files you are working with until you know how this works

(def vaultPath "/Users/roberthaisfield/coding-projects/clojure-projects/functions-for-processing-notes/Personal-Notes-Copy")
(def knowledge-graph-path "/Users/roberthaisfield/coding-projects/clojure-projects/functions-for-processing-notes/knowledge-graphs-ux-copy/Rob/")

(defn find-all-md [path glob-pattern]
  (fs/glob path glob-pattern))

(defn all-markdown-for-path [path]
  (vec (map str (find-all-md path "**.md"))))


(defn find-name-in-name [name1 name2]
  (when (re-find (re-pattern name1) name2) name2))

(find-name-in-name "GIF" "animated GIF for GuidedTrack")

(defn find-search-text-in-list-of-strings [name1 list-of-strings]
  (remove nil? (map #(find-name-in-name name1 %) list-of-strings)))

(defn create-list-of-list-of-file-names [list-of-strings]
  "the first item of each list in the output is the string that is contained in other strings in the list. The rest of a list are the containing strings"
  (map #(find-search-text-in-list-of-strings % list-of-strings) list-of-strings))

(defn create-list-of-list-of-nested-strings [list-of-strings1 list-of-strings2]
  "the first item in a list in the output is the string that is contained in other strings in another list. The rest of a list are the containing strings"
  (map #(find-search-text-in-list-of-strings % list-of-strings2) list-of-strings1))


; Take all of the file name lists with an amount of items greater than one and sort them by shortest to longest
(defn sort-file-list-of-lists-short-to-long [file-list]
  (map #(sort-by count %) (filter #(> (count %) 1) file-list)))

(defn make-data-structure-for-complex-pages [list]
  {(first list) (vec (rest list))})

(defn remove-path-and-extensions-from-file-list [list-of-file-paths]
  (map (comp first fs/split-ext fs/file-name) list-of-file-paths))

; Wait what the heck did I just do? Some of these lists contain similar items where I'm not sure how they happened or what the relationship is.
; In this specific function, I'm creating a list of nested strings on two different note vaults.
; Looking at the output, it might be grabbing notes that contain words within the same names?
; Try running this on two directories and see what happens
(sort-file-list-of-lists-short-to-long
  (create-list-of-list-of-nested-strings
    (remove-path-and-extensions-from-file-list
      (all-markdown-for-path vaultPath))
    (remove-path-and-extensions-from-file-list
      (all-markdown-for-path knowledge-graph-path))))
; Switch order
(sort-file-list-of-lists-short-to-long
  (create-list-of-list-of-nested-strings
    (remove-path-and-extensions-from-file-list
      (all-markdown-for-path knowledge-graph-path))
    (remove-path-and-extensions-from-file-list
      (all-markdown-for-path vaultPath))))

(clojure.data/diff
  (sort-file-list-of-lists-short-to-long
    (create-list-of-list-of-nested-strings
      (remove-path-and-extensions-from-file-list
        (all-markdown-for-path vaultPath))
      (remove-path-and-extensions-from-file-list
        (all-markdown-for-path knowledge-graph-path))))
  ; Switch order
  (sort-file-list-of-lists-short-to-long
    (create-list-of-list-of-nested-strings
      (remove-path-and-extensions-from-file-list
        (all-markdown-for-path knowledge-graph-path))
      (remove-path-and-extensions-from-file-list
        (all-markdown-for-path vaultPath)))))


(defn ultimate-data-structure [path]
  (let [list-of-strings (remove-path-and-extensions-from-file-list
                          (all-markdown-for-path path))]
    (into {} (map make-data-structure-for-complex-pages
                  (sort-file-list-of-lists-short-to-long
                    (create-list-of-list-of-nested-strings
                      list-of-strings
                      list-of-strings))))))

(create-list-of-list-of-file-names
  (remove-path-and-extensions-from-file-list
    (all-markdown-for-path knowledge-graph-path)))

(ultimate-data-structure knowledge-graph-path)


(defn append-to-file
  "Uses spit to append to a file specified with its name as a string, or
   anything else that writer can take as an argument.  s is the string to
   append."
  [file-name s]
  (spit file-name s :append true))

(defn data-structure-to-append-to-big-page [path]
  (transform [MAP-KEYS]
             #(str "[[" % "]]")
             (transform [MAP-VALS ALL]
                        #(str path "/" % ".md")
                        (ultimate-data-structure path))))

(defn append-small-page-wikilink-to-list-of-file-paths [lat1 page-name]
  (cond (empty? lat1)
        nil
        true (do
               (append-to-file
                 (first lat1)
                 (str "\n"
                      "- "
                      page-name))
               (append-small-page-wikilink-to-list-of-file-paths (rest lat1) page-name))))

; use map to get the one list version to work on a list of lists
(defn append-all-small-pages-to-big-pages [path]
  (map append-small-page-wikilink-to-list-of-file-paths
       (select [MAP-VALS] (data-structure-to-append-to-big-page path))
       (select [MAP-KEYS] (data-structure-to-append-to-big-page path))))

;test case, make sure you use a duplicate of your actual directory. You can see the data structure it operates on if you use the following
(comment
  (map append-small-page-wikilink-to-list-of-file-paths
       (select [MAP-VALS] (data-structure-to-append-to-big-page knowledge-graph-path))
       (select [MAP-KEYS] (data-structure-to-append-to-big-page knowledge-graph-path))))
(comment
  (append-all-small-pages-to-big-pages knowledge-graph-path))

(defn data-structure-page-name-page-contents [path]
  (zipmap (all-markdown-for-path path)
          (map fs/read-all-lines (all-markdown-for-path path))))

(comment
  (zipmap (all-markdown-for-path knowledge-graph-path)
          (map fs/read-all-lines (all-markdown-for-path knowledge-graph-path))))

(find-search-text-in-list-of-strings "C-"
                                     (select [MAP-KEYS]
                                             (data-structure-page-name-page-contents "/Users/roberthaisfield/coding-projects/clojure-projects/functions-for-processing-notes/knowledge-graphs-ux-copy")))

(defn search-page-contents-in-directory [search-string path]
  (transform [MAP-VALS]
             vec
             (vec
               (map
                 #(find-search-text-in-list-of-strings
                    search-string
                    %)
                 (select [MAP-VALS] (data-structure-page-name-page-contents path))))))


;test case
(comment
  (search-page-contents-in-directory
    "C-"
    knowledge-graph-path))

(comment
  (defn search-string-contents-of-all-pages-in-path [search-string path]
    (map #(find-search-text-in-list-of-strings
            search-string
            %)
         (map fs/read-all-lines (all-markdown-for-path path)))))

; trying to see the difference between these two functions
; search-page-contents-in-directory
; search-string-contents-of-all-pages-in-path
; the read-all-lines function outputs a list of vectors of strings
; the select function outputs a vector of vectors of strings)
(comment
  (map fs/read-all-lines (all-markdown-for-path knowledge-graph-path)) ;outputs list of vectors of strings
  (select [MAP-VALS] (data-structure-page-name-page-contents knowledge-graph-path)))

; Trying to see which is faster
; Looking at this, it seems like they output a different order, so even though the Specter implementation is a touch slower, it's needed to maintain the logic for data
(comment
  (time (remove empty?
                (search-string-contents-of-all-pages-in-path "social" knowledge-graph-path)))

  (time (remove empty?
                (search-page-contents-in-directory "social" knowledge-graph-path))))

(defn positions
  "grabs the index of something within a collection"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn get-indexes-of-content-search-in-directory-matches [search-string path]
  (positions false?
             (map empty? (search-page-contents-in-directory
                           search-string
                           path))))

(defn get-indexes-of-content-search-in-directory-matches [search-string path]
  (positions false?
             (map empty? (search-page-contents-in-directory
                           search-string
                           path))))

(get-indexes-of-content-search-in-directory-matches
  "GuidedTrack"
  knowledge-graph-path)

(defn get-page-names-of-file-content-search-results [search-string path]
  (map #(nth
          (select [MAP-KEYS] (data-structure-page-name-page-contents path))
          %)
       (get-indexes-of-content-search-in-directory-matches
         search-string
         path)))

; playing with concrete values
(comment
  (map #(nth
          (select [MAP-KEYS] (data-structure-page-name-page-contents knowledge-graph-path))
          %)
       (get-indexes-of-content-search-in-directory-matches
         "GuidedTrack"
         knowledge-graph-path)))

; crappy early implementation of the search function
(comment
  (defn search-for-string-in-contents-and-grab-file-name-as-key [search-string path]
    {(get-page-names-of-file-content-search-results search-string path)
     (remove empty?
             (search-page-contents-in-directory search-string path))}))


(defn search-for-string-in-contents-and-grab-file-name-as-key [search-string path]
  (into {}
        (partition-all 2)
        (interleave
          (get-page-names-of-file-content-search-results search-string
                                                         path)
          (remove empty? (search-page-contents-in-directory search-string
                                                            path)))))

(search-for-string-in-contents-and-grab-file-name-as-key "twitter" knowledge-graph-path)
