(ns xflow.batch-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [xflow.template :as template]
            [clojure.string :as str]
            [xflow.parser-refactor-test]
            [xflow.layout.swimlane-test]
            [xflow.layout.routing-test]))

(deftest batch-render-templates
  (let [templates-dir (io/file "resources/templates")
        output-dir (io/file "output/batch_test")
        files (filter #(str/ends-with? (.getName %) ".edn") (file-seq templates-dir))]

    (when-not (.exists output-dir)
      (.mkdirs output-dir))

    (doseq [file files]
      (let [filename (.getName file)
            template-name (str/replace filename #"\.edn$" "")
            output-file (str output-dir "/" template-name ".svg")]
        (println "Rendering template:" template-name "to" output-file)
        ;; render-template expects keyword or string for template name (without extension)
        (template/render-template template-name output-file)

        (is (.exists (io/file output-file)) (str "Output file should exist: " output-file))
        (is (> (.length (io/file output-file)) 0) (str "Output file should not be empty: " output-file))))))

(defn -main [& args]
  (println "\n=== Running New Unit Tests ===")
  (run-tests 'xflow.parser-refactor-test
             'xflow.layout.swimlane-test
             'xflow.layout.routing-test)

  (println "\n=== Running Batch Render Tests ===")
  (run-tests 'xflow.batch-test))

(-main)