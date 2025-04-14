(defproject btcbalance "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :aot [btcbalance.core]
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [bchain "0.1.0-SNAPSHOT"]
                 [clj-twilio "0.2.0"]
                 [org.clojure/data.json "0.2.4"]
                 [com.draines/postal "2.0.2"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [clj-http "3.12.3"]
                 [cheshire "5.11.0"]
                 [hickory "0.7.1"]]
  :java-source-paths ["java/src"])
