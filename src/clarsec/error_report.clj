(ns clarsec.error-report)

(defn- get-lastline [^String input]
  (let [index (.lastIndexOf input (str \newline))]
    (if (= -1 index)
      input
      (subs input index))))

(defn- get-error-info [initial-input shortest-rest-len]
  (let [index-of-err (- (count initial-input) shortest-rest-len)
        sub (subs initial-input 0 index-of-err)
        cnt (count (filter #{\newline} sub))]
    [(inc cnt) (get-lastline sub)]))

(declare ^:dynamic *shortest-rest-len*
         ^:dynamic *initial-input*)

(defn generate [result]
  (let [info (get-error-info *initial-input* @*shortest-rest-len*)
        line-number (first info)
        line (second info)]
    {:line-number line-number :error-line line}))

(defmacro with-new-report [initial-input & body]
  `(binding [*shortest-rest-len* (atom (count ~initial-input))
             *initial-input* ~initial-input]
     (let [result# (try ~@body
                        (catch Exception e#
                          (merge {:type :failed :exception e#})))]
       (if (= :failed (:type result#))
         (println (merge result# (generate result#)))
         result#))))

(defn update! [parser-result]
  (when (bound? #'*shortest-rest-len*)
    (swap! *shortest-rest-len* min (count (:rest parser-result)))))


