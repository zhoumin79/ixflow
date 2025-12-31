(ns xflow.dsl.parser
  (:require [clojure.string :as str]))

(defn- strip-quotes [s]
  (if (string? s)
    (-> s
        str/trim
        (str/replace #"^\"+|\"+$" ""))
    s))

(defn- parse-props [props-str]
  (if (empty? props-str)
    {}
    (->> (str/split props-str #",")
         (map (fn [p]
                (let [[k v] (str/split p #":" 2)]
                  [(keyword (str/trim k)) (some-> v strip-quotes)])))
         (into {}))))

(defn- parse-line [line context]
  (let [line (str/trim line)]
    (cond
      (empty? line) context
      (str/starts-with? line "//") context ;; Comment

      ;; Title: title ...
      (str/starts-with? line "title ")
      (assoc-in context [:config :title] (strip-quotes (subs line 6)))

      ;; Config: key: value
      (re-matches #"^([a-zA-Z0-9-]+)\s*:\s*(.+)$" line)
      (let [[_ k v] (re-matches #"^([a-zA-Z0-9-]+)\s*:\s*(.+)$" line)]
        ;; Only treat as config if we are NOT inside a pool/block
        (if (empty? (:stack context))
          (assoc-in context [:config (keyword k)] (strip-quotes v))
          context))

      ;; Pool Start: Name [props] {
      (re-matches #"(?i)^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?\s*\{$" line)
      (let [[_ name props-str] (re-matches #"(?i)^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?\s*\{$" line)
            pool {:id name ;; FIX: Add ID to match node structure
                  :name name
                  :props (parse-props props-str)
                  :nodes []
                  :type :pool}]
        (-> context
            (update :stack conj pool)))

      ;; Pool End: }
      (= line "}")
      (let [pool (peek (:stack context))
            stack (pop (:stack context))]
        (if (empty? stack)
          (-> context
              (update :pools conj pool) ;; Top level pool
              (assoc :stack stack)) ;; FIX: Remove from stack!
          (let [parent (peek stack)
                parent (update parent :nodes conj pool) ;; Nested pool
                stack (pop stack)
                stack (conj stack parent)]
            (assoc context :stack stack))))

      ;; Edge: A > B, A -> B, A --> B, A -- B [props] : label
      (re-find #"^(.+?)\s+((?:-*>)|(?:--))\s+(.+?)(\s*\[.*\])?(?:\s*:\s*(.*))?$" line)
      (let [[_ from type-str to props-str label] (re-find #"^(.+?)\s+((?:-*>)|(?:--))\s+(.+?)(\s*\[.*\])?(?:\s*:\s*(.*))?$" line)
            type (if (or (str/includes? type-str "--") (= type-str "--")) :dashed :solid)
            idx (count (:edges context))
            props (if props-str
                    (parse-props (subs (str/trim props-str) 1 (dec (count (str/trim props-str)))))
                    {})
            final-type (or (some-> (:type props) keyword) type)
            relation (some-> (:relation props) keyword)
            edge (cond-> {:id (str "edge-" idx)
                          :from (str/trim (str/replace from #"\"" ""))
                          :to (str/trim (str/replace to #"\"" ""))
                          :type final-type
                          :relation relation
                          :label (some-> label strip-quotes)}
                   (seq props) (merge (dissoc props :type :relation)))]
        (update context :edges conj edge))

      ;; Node: Name [props]
      :else
      (let [[_ name props-str] (re-matches #"^\"?([^\"]+?)\"?\s*(?:\[(.*)\])?$" line)]
        (if name
          (let [node {:id name
                      :props (parse-props props-str)}]
            (if (empty? (:stack context))
              (update context :nodes conj node) ;; Top level node
              (let [pool (peek (:stack context))
                    pool (update pool :nodes conj node)
                    stack (pop (:stack context))
                    stack (conj stack pool)]
                (assoc context :stack stack))))
          ;; Log ignored lines for debugging
          (do
            (when (not (empty? line))
              (println "Ignored line:" line))
            context))))))

(defn parse-dsl [dsl-str]
  (let [lines (str/split-lines dsl-str)
        initial-ctx {:config {} :pools [] :nodes [] :edges [] :stack []}]
    (reduce (fn [ctx line] (parse-line line ctx)) initial-ctx lines)))

(defn flatten-nodes [pools top-level-nodes]
  (let [all-nodes (atom top-level-nodes)]
    (letfn [(process-pool [pool path]
              (let [current-path (conj path pool)
                    nodes (:nodes pool)]
                ;; Add the pool itself as a node
                (swap! all-nodes conj (assoc pool
                                             :swimlane-id (str/join " / " (map :name path))
                                             :swimlane-path (mapv :name path)))
                (doseq [item nodes]
                  (if (= (:type item) :pool)
                    (process-pool item current-path)
                    (swap! all-nodes conj (assoc item
                                                 :swimlane-id (str/join " / " (map :name current-path))
                                                 :swimlane-path (mapv :name current-path)))))))]
      (doseq [pool pools]
        (process-pool pool [])))
    @all-nodes))
