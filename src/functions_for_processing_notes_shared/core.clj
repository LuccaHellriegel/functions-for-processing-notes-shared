(ns functions-for-processing-notes-shared.core
  (:require [babashka.fs :as fs]
            [clojure.string :as str :refer [includes? join]])
  (:gen-class))

(defn md-files [path]
  (map str (fs/glob path "**.md")))

(defn search-in-strs [search-term strs]
  (filter #(includes? % search-term) strs))

(defn search-strs-in-strs [search-terms strs]
  (map #(search-in-strs % strs) search-terms))

(defn filter-search-results [search-results]
  (filter #(> (count %) 1) search-results))

(defn add-search-result-to-map [search-result m]
  (let [sorted (sort-by count search-result)
        k (first sorted)
        v (rest sorted)]
    (assoc m k v)))

(defn add-search-results-to-map [search-results]
  (reduce #(add-search-result-to-map %2 %1) {} search-results))

(defn search-results->map [search-results]
  (-> search-results
      (filter-search-results)
      (add-search-results-to-map)))

(defn path->page-name [path]
  (first (fs/split-ext path)))

(defn paths->page-name [paths]
  (map path->page-name paths))

(defn paths->search-result-map [paths]
  (-> paths
      (paths->page-name)
      (#(search-strs-in-strs % %))
      (search-results->map)))

(defn append-to-file [path str]
  (spit path str :append true))

(defn str->wikilink [s]
  (str "[[" s "]]"))

(defn page-name->appendable [page-name]
  (str "\n"
       "- "
       (str->wikilink page-name)))

; this only checks straight wikilinks, aliases etc. are not checked!
; we could replace this with checking at a certain position if we want to have the page-names at a certain position
(defn append-if-not-included [path to-be-appended]
  (when (not (includes? (slurp path) to-be-appended))
    (append-to-file path to-be-appended)))

(defn vec-contains? [vec str]
  (some #(= % str) vec))

(defn append-page-name-to-relevant-files [full-paths relevant-page-names page-name]
  (doseq
   [path full-paths]
    (when
     (vec-contains? relevant-page-names (path->page-name path))
      (append-if-not-included path (page-name->appendable page-name)))))

; could be massively sped up by converting the data structure from {to-be-added-page-name: [compound-pages...]}
; to {compound-page: [to-be-added-page-names...]}, so we need to create the appendables just once and do just one read/write to the file
; but I am running out of time for now
(defn append-page-names-to-compound-pages [path]
  (let [full-paths (md-files path)
        m (paths->search-result-map full-paths)]
    (doseq [[page-name relevant-page-names] m]
      (println (str "Now appending " page-name))
      (append-page-name-to-relevant-files full-paths (vec relevant-page-names) page-name))))

; could be simplified, but running out of time
; difference from the other one is that we go throug the paths
; and can immediately add all the appendables because the data-structure is more suited for this case
(defn append-compound-pages-to-pages [path]
  (let [full-paths (md-files path)
        m (paths->search-result-map full-paths)]
    (doseq
     [path full-paths]
      (let [page-name (path->page-name path)
            page-content (slurp path)
            relevant-page-names (get page-name m)
            filtered-rel-page-names (filter #(not (includes? page-content %)) relevant-page-names)
            appendables (map page-name->appendable filtered-rel-page-names)]
        (when (not-empty appendables)
          (append-to-file (join "" appendables) path))))))

(defn -main [& args]
  (let [path (first args)
        append-page-names (nil? (second args))]
    (println (str "Processing path " path))
    (if append-page-names
      (do
        (println "Appending page-names to compound pages")
        (append-page-names-to-compound-pages path))
      (do
        (println "Appending compound page-names to pages")
        (append-compound-pages-to-pages path)))))