(ns helix.impl.props2-test
  (:require [clojure.test :refer [is deftest testing run-tests]]
            [pjstadig.humane-test-output]
            [helix.impl.props2 :refer [gen-translate-props]]
            [clojure.pprint :refer [pprint]]))

(pjstadig.humane-test-output/activate!)

(def starting-next-id 0)
(def next-id (atom starting-next-id))

(defn mock-gensym
  ([] (mock-gensym ""))
  ([prefix] (symbol (str prefix (swap! next-id inc)))))

(defmacro with-mock-gensym [& body]
  `(with-redefs [clojure.core/gensym mock-gensym]
     (reset! next-id starting-next-id)
     ~@body))

; -- macros -----------------------------------------------------------------------------------------------------------------

(deftest gen-translate-props-test
  (with-mock-gensym
    (testing "basic props translation"
      (is (= '(clojure.core/let
                [key-1 (identity "somecode") key-2 symbol-name]
                (js-obj
                  (helix.impl.props2/dynamic-translate-prop-key key-1)
                  (helix.impl.props2/dynamic-translate-prop-value
                    key-1
                    "symbol-from-code")
                  "className"
                  "class-value"
                  "complexValue"
                  {:should-not-camelize {:should-not-camelize 2}}
                  "htmlFor"
                  "for-value"
                  "kebabNameAndCName"
                  "kebab-value"
                  "name"
                  "value"
                  "style"
                  (js-obj
                    "backgroundColor"
                    (helix.impl.props2/dynamic-translate-style (identity "red"))
                    "color"
                    "color-value")
                  "stringName"
                  "string-name"
                  (helix.impl.props2/dynamic-translate-prop-key key-2)
                  (helix.impl.props2/dynamic-translate-prop-value key-2 "symbol-val")))

             (gen-translate-props '{:name                  "value"
                                    :kebab-name-and-c-name "kebab-value"
                                    :complex-value         {:should-not-camelize {:should-not-camelize 2}}
                                    symbol-name            "symbol-val"
                                    (identity "somecode")  "symbol-from-code"
                                    "string-name"          "string-name"
                                    :class                 "class-value"
                                    :for                   "for-value"
                                    :style                 {:color            "color-value"
                                                            :background-color (identity "red")}}))
          ))))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(comment
  (run-tests)

  (with-mock-gensym
    (pprint (gen-translate-props '{:name                  "value"
                                   :kebab-name-and-c-name "kebab-value"
                                   :complex-value         {:should-not-camelize {:should-not-camelize 2}}
                                   symbol-name            "symbol-val"
                                   (identity "somecode")  "symbol-from-code"
                                   "string-name"          "string-name"
                                   :class                 "class-value"
                                   :for                   "for-value"
                                   :style                 {:color            "color-value"
                                                           :background-color (identity "red")}})))
  )
