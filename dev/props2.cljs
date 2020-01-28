(ns props2
  (:require [clojure.test :refer [deftest is are testing run-tests]]
            [helix.impl.props2 :refer [translate-props props-bean]]
            [goog.object :as gobj]))

(enable-console-print!)

(defn serialize [v]
  (.stringify js/JSON v))

(defn xf [s]
  (str ">" s "-postfix<"))

(deftest test-translate-props
  (testing "basic static props translation"
    (are [props res] (= (serialize res) (serialize (translate-props props)))
      ; basic static cases
      {:name "v"} #js {:name "v"}
      {:kebab-name "v"} #js {"kebabName" "v"}
      {:class "v"} #js {"className" "v"}
      {:for "v"} #js {"htmlFor" "v"}
      {:style "v"} #js {"style" "v"}
      {:style {:background-color "red"}} #js {"style" #js {"backgroundColor" "red"}}

      ; basic dynamic cases
      {(xf "name") "v"} #js {">namePostfix<" "v"}
      {(xf "kebab-name") "v"} #js {">kebabNamePostfix<" "v"}

      )))

(deftest test-props-bean
  (testing "basic reverse props translation"
    (are [props res] (= res (props-bean props))
      #js {"someValueAndSomething" "v"} {:some-value-and-something "v"}
      #js {"SomeValue" "v"} {:Some-value "v"}
      #js {"className" "v"} {:class "v"}
      #js {"htmlFor" "v"} {:for "v"}
      #js {"aria-some-thing" "v"} {:aria-some-thing "v"}
      #js {"data-some-thing" "v"} {:data-some-thing "v"}
      #js {"style" #js {"backgroundColor" "red"}} {:style {:background-color "red"}}))
  (testing "props-bean should not be recursive except for style"
    (let [nested-val #js {"nestedValue1" #js {"nestedValue2" "v"}}]
      (are [props res] (= res (props-bean props))
        #js {"value" nested-val} {:value nested-val}
        #js {"style" nested-val} {:style {:nested-value1 {:nested-value2 "v"}}}))))

(run-tests)

;(js-debugger)
;(js/console.log (str (macroexpand '(translate-props {(xf "name") "value"}))))
;(js/console.log (translate-props {(xf "name-x-y-z-last") "value"}))

#_(js/console.log (props-bean #js {"className"       "c"
                                   "htmlFor"         "h"
                                   "aria-some-thing" "a"
                                   "data-some-thing" "d"
                                   "style"           #js {"backgroundColor" "red"}}))
