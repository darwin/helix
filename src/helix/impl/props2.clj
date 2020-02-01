(ns helix.impl.props2
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]])
  (:import (clojure.lang Named)
           (cljs.tagged_literals JSValue)
           (java.util.regex Pattern)))
;
; see motivation https://github.com/Lokeh/helix/issues/9
;
; translating props to js land:
;   * we should strive to do as much work at compile-time
;
; some conventions:
;   * fn prefixed with "gen-" are code generating
;   * macros should be minimal wrappers around gen-functions
;   * fallback to dynamic cases should be performed by calling "dynamic-" counterparts
;   * to prevent code bloat, do not generate unnecessary dynamic code, implement dynamic- helpers in cljs ns instead
;

; -- constants --------------------------------------------------------------------------------------------------------------

(def ^:const NO_TRANSLATION 0)
(def ^:const STYLE_TRANSLATION 1)

(def kebab-to-camel-re #"-(\w)")
(def special-case-prop-name-re #"^(aria-|data-|class$|for$|style$).*")

; TODO: this is a workaround to generate stable code
; FIXME: this must be resolved somehow
;
; from #clojurescript slack
;darwin: I have a macro which is processing a map, I’m generating cljs code based on visited keys and values.
;        My issue is that I need to generate the code in the same order as individual keys/vals appeared in the source code.
;        Naively iterating over a map value in macro seem to not preserve the order. Any ideas?
;        btw. I’m using reduce over the map’s [k v] (edited)
;alexmiller: maps are not ordered
;            so this isn't going to work with a map as is
;darwin: yep, looks like I’m toasted
;alexmiller: if you look at something like let , the use of a vector for bindings enforces a semantic order (edited)
;darwin: thanks for the suggestion, this is part of an effort to translate react props passed as cljs map to js conventions
;        (at compile time), not sure if this a-vector-instead-of-a-map proposal would be palatable in this case (edited)
;
(defn force-as-sorted-map [m]
  (let [x (into (sorted-map-by #(compare (str %1) (str %2))) m)]
    x))

; -- helpers ----------------------------------------------------------------------------------------------------------------

(defn kebab-to-camel [name]
  (assert name)
  (assert (> (count name) 0))
  (if (= \' (.charAt name 0))
    (subs name 1)
    (string/replace name kebab-to-camel-re #(string/upper-case (second %)))))

(defn coerce-key-to-string [k]
  (cond
    (symbol? k) nil
    (instance? Named k) (name k)
    ; TODO: decide if we want string keys to be excluded from translation, then they should be quoted here
    (string? k) k))

(defn cljs-primitive? [v]
  (or (nil? v)
      (boolean? v)
      (string? v)
      (number? v)
      (float? v)
      (keyword? v)
      (instance? Pattern v)
      ; TODO: review this and maybe add other cases of primitive values shared between clj and cljs
      ))

; -- style translation ------------------------------------------------------------------------------------------------------

(declare gen-translate-style)

(defn gen-translate-style-key [k]
  (if-some [name (coerce-key-to-string k)]
    (kebab-to-camel name)
    `(dynamic-translate-style-key ~k)))

(defn gen-translate-style-value [v]
  (gen-translate-style v))

(defn gen-translate-style-kv [[k v]]
  [(gen-translate-style-key k) (gen-translate-style-value v)])

(defn gen-translate-style [v]
  (cond
    (map? v) `(~'js-obj ~@(mapcat gen-translate-style-kv (force-as-sorted-map v)))
    (vector? v) `(~'array ~@(map gen-translate-style v))
    (cljs-primitive? v) v
    :else `(dynamic-translate-style ~v)))

; -- props translation ------------------------------------------------------------------------------------------------------

(defn key->translation-mode [k]
  (if (= (coerce-key-to-string k) "style")
    STYLE_TRANSLATION
    NO_TRANSLATION))

(defn gen-translate-prop-value [k v]
  (assert (= 0 NO_TRANSLATION))
  (assert (= 1 STYLE_TRANSLATION))
  (case (key->translation-mode k)
    0 v
    1 (gen-translate-style v)))

(defn gen-translate-prop-key-statically [k]
  (if-some [name (coerce-key-to-string k)]
    ; static case
    (if-some [match (re-matches special-case-prop-name-re name)]
      (case (second match)
        ("aria-" "data-") name
        "class" "className"
        "for" "htmlFor"
        "style" "style")
      (kebab-to-camel name))
    ; defer dynamic case codegen
    ))


(defn prop-translation-reducer [[bindings members] [k v]]
  (if-some [translated-static-k (gen-translate-prop-key-statically k)]
    ; static case
    (let [translated-v (gen-translate-prop-value k v)]
      [bindings (conj members translated-static-k translated-v)])
    ; dynamic case
    ; -  note that dynamic key forces use to take dynamic code path for value translation as well
    ;   because key value is determining translation mode (and that will be known during runtime)
    (let [k-sym (gensym "key-")]
      ; we need to hoist the key to prevent double evaluation
      [(conj bindings k-sym k) (conj members
                                     `(dynamic-translate-prop-key ~k-sym)
                                     `(dynamic-translate-prop-value ~k-sym ~v))])))

(defn gen-translate-clean-props-map [m]
  (assert (map? m))
  (let [[bindings kv-members] (reduce prop-translation-reducer [[] []] (force-as-sorted-map m))
        js-map `(~'js-obj ~@kv-members)]
    (if (seq bindings)
      `(let [~@bindings]
         ~js-map)
      js-map)))

(defn extract-spread-special [m]
  (if-some [amp (m '&)]
    [amp (dissoc m '&)]
    (if-some [amp (m :&)]
      [amp (dissoc m :&)])))

(defn gen-translate-props-map [m]
  (if-some [[spread-sym clean-m] (extract-spread-special m)]
    `(js/Object.assign ~(gen-translate-clean-props-map clean-m) (dynamic-translate-props ~spread-sym))
    (gen-translate-clean-props-map m)))

(defn gen-translate-props [v]
  (if (map? v)
    (gen-translate-props-map v)
    `(dynamic-translate-props ~v)))

; -- macros -----------------------------------------------------------------------------------------------------------------

(defmacro translate-props [v]
  (gen-translate-props v))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(comment

  (gen-translate-prop-key "aria-something")
  (gen-translate-prop-key "data-something")
  (gen-translate-prop-key "data1-something")
  (gen-translate-prop-key "class")
  (gen-translate-prop-key "for")
  (gen-translate-prop-key "class1")
  (gen-translate-prop-key "forx")
  (gen-translate-prop-key "kebab-name-last")
  (gen-translate-prop-key "'kebab-name-last")

  (pprint (macroexpand '(translate-props {:name "value"})))

  (pprint (macroexpand '(translate-props {:name "value" & rest})))
  (pprint (macroexpand '(translate-props {:name "value" :& rest})))

  (pprint (macroexpand '(translate-props {:name               "value"
                                          :kebab-name         "kebab-value"
                                          :regex-value        #"a regex"
                                          :complex-value      {:should-not-camelize {:should-not-camelize 2}}
                                          :js-map             (JSValue. {:js "map"})
                                          :js-vector          (JSValue. ["js" "vector"])
                                          symbol-name         "symbol-val"
                                          (str "some" "code") "symbol-from-code"
                                          "string-name"       "string-name"
                                          :class              "class-value"
                                          :for                "for-value"
                                          :style              {:color               "color-value"
                                                               :nesting-should-work {:background-color "val-from-nesting"}
                                                               (str "style" "code") {:background-color "val-from-style-code"}
                                                               :background-color    (str "red")}
                                          }

                                         )))

  (pprint (macroexpand '(translate-props {:style {:background-color "value"
                                                  :nested-map       {:nested-key "nested-val"}}})))

  (pprint (macroexpand '(translate-props (merge spring {:key           index
                                                        :castShadow    true
                                                        :receiveShadow true}))))

  )



