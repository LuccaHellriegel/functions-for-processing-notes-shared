(ns functions-for-processing-notes-shared.core-test
  (:require [clojure.test :refer :all]
            [functions-for-processing-notes-shared.core :refer :all]
            [clojure.string :refer [split includes?]]))

; I am to lazy to figure out Clojure mocking right now
; also too lazy to write Unit Tests, so Integration Test here we go
(def base-path "./test/functions_for_processing_notes_shared/files/")
(def file-paths (map #(str base-path %)
                     ["Title.md" "Title Of Long.md" "Not Affected.md" "Long Title.md" "Long Title Of Long Titles.md"]))

(defn slurp-files []
  (map (fn [%] {:path % :content (slurp %)}) file-paths))
(defn spit-files [file-maps]
  (doseq [m file-maps]
    (spit (:path m) (:content m))))

(def file-maps (slurp-files))

;resets the files after running the tests
(defn fixture [test-run]
  (test-run)
  (spit-files file-maps))
(use-fixtures :each fixture)

(def base-content "Content\nContent\n")
(def base-re (re-pattern base-content))

(def expected-appended-pages {"Not Affected.md" nil
                              "Title.md" nil
                              "Title Of Long.md" ["Title"]
                              "Long Title.md" ["Title"]
                              "Long Title Of Long Titles.md" ["Title" "Long Title" "Title Of Long"]})

(deftest smaller-pages-names-should-be-appended
  (testing
   (-main base-path)
    (let [file-maps (slurp-files)]
      (doseq [m file-maps]
        (let [page-name (path->page-name (:path m))
              appended (second (split (:content m) base-re))
              expected (get page-name expected-appended-pages)]
          (is (or (and (nil? appended) (= appended expected))
                  ;we need to check each expected page name individually because we have no guarantee of ordering
                  (every?  #(includes? appended (page-name->appendable %)) expected))))))))

(deftest page-names-should-not-be-appended-twice
  (testing
   (-main base-path)
    (let [file-maps (slurp-files)]
      (-main base-path)
      (is (= file-maps (slurp-files))))))

(def expected-appended-compound-pages {"Not Affected.md" nil
                                       "Title.md" ["Title Of Long" "Long Title" "Long Title Of Long Titles.md"]
                                       "Title Of Long.md" ["Long Title" "Title Of Long"]
                                       "Long Title.md" ["Long Title" "Title Of Long"]
                                       "Long Title Of Long Titles.md" nil})

(deftest compound-pages-names-should-be-appended
  (testing
   (-main base-path "compound")
    (let [file-maps (slurp-files)]
      (doseq [m file-maps]
        (let [page-name (path->page-name (:path m))
              appended (second (split (:content m) base-re))
              expected (get page-name expected-appended-compound-pages)]
          (is (or (and (nil? appended) (= appended expected))
                  ;we need to check each expected page name individually because we have no guarantee of ordering
                  (every?  #(includes? appended (page-name->appendable %)) expected))))))))

(deftest compound-pages-should-not-be-appended-twice
  (testing
   (-main base-path "compound")
    (let [file-maps (slurp-files)]
      (-main base-path "compound")
      (is (= file-maps (slurp-files))))))