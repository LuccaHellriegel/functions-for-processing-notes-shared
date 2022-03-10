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

(def base-content "Content\nContent\n")
(def base-re (re-pattern base-content))
;resets the files after running the tests
(defn fixture [test-run]
  (test-run)
  (doseq [path file-paths]
    (spit path base-content)))
(use-fixtures :each fixture)

(def expected-appended-pages {"Not Affected" nil
                              "Title" nil
                              "Title Of Long" ["Title"]
                              "Long Title" ["Title"]
                              "Long Title Of Long Titles" ["Title" "Long Title" "Title Of Long"]})

(deftest smaller-pages-names-should-be-appended
  (testing
   (do
     (-main base-path)
     (let [f-maps (slurp-files)]
       (doseq [m f-maps]
         (let [page-name (path->page-name (:path m))
               appended (second (split (:content m) base-re))
               expected (get expected-appended-pages page-name)]
           (if  (nil? appended)
             (is (= appended expected) (str "Page: " page-name ". appended != expected: \n appended: " appended "\n expected: " expected))
             ;we need to check each expected page name individually because we have no guarantee of ordering
             (do (is (not (nil? expected)) (str "Page: " page-name ". expected == nil: \n appended: " appended "\n expected: " expected))
                 (is (every?  #(includes? appended (page-name->appendable %)) expected)
                     (str "Page: " page-name ". appended does not contain all expected: \n appended: " appended "\n expected: " expected))))))))))

(deftest page-names-should-not-be-appended-twice
  (testing
   (do
     (-main base-path)
     (let [file-maps (slurp-files)]
       (-main base-path)
       (is (= file-maps (slurp-files)))))))

(def expected-appended-compound-pages {"Not Affected" nil
                                       "Title" ["Title Of Long" "Long Title" "Long Title Of Long Titles"]
                                       "Title Of Long" ["Long Title Of Long Titles"]
                                       "Long Title" ["Long Title Of Long Titles"]
                                       "Long Title Of Long Titles" nil})

(deftest compound-pages-names-should-be-appended
  (testing
   (do
     (-main base-path "compound")
     (let [file-maps (slurp-files)]
       (doseq [m file-maps]
         (let [page-name (path->page-name (:path m))
               appended (second (split (:content m) base-re))
               expected (get expected-appended-compound-pages page-name)]
           (if (nil? appended)
             (is (= appended expected) (str "Page: " page-name ". appended != expected: \n appended: " appended "\n expected: " expected))
             ;we need to check each expected page name individually because we have no guarantee of ordering
             (do (is (not (nil? expected)) (str "Page: " page-name ". expected == nil: \n appended: " appended "\n expected: " expected))
                 (is (every?  #(includes? appended (page-name->appendable %)) expected)
                     (str "Page: " page-name ". appended does not contain all expected: \n appended: " appended "\n expected: " expected))))))))))

(deftest compound-pages-should-not-be-appended-twice
  (testing
   (do
     (-main base-path "compound")
     (let [file-maps (slurp-files)]
       (-main base-path "compound")
       (is (= file-maps (slurp-files)))))))