(ns helix.impl.props2
  (:require-macros [helix.impl.props2])
  (:require [goog.object :as gobj]
            [cljs-bean.core :refer [bean]]))

; this is an experimental implementation of props translation

(def ^:const NO_TRANSLATION 0)
(def ^:const STYLE_TRANSLATION 1)

; -- helpers ----------------------------------------------------------------------------------------------------------------

(def kebab-to-camel-re (js/RegExp. "-(\\w)" "g"))
(def camel-to-kebab-re (js/RegExp. "([a-z0-9]|(?=[A-Z]))([A-Z])" "g"))
(def special-case-prop-name-re #"^(aria-|data-|class$|for$|style$).*")
(def special-case-reverse-prop-name-re #"^(aria-|data-|className$|htmlFor$|style$).*")

(defn kebab-to-camel [name]
  (if (= \' (.charAt name 0))
    (subs name 1)
    (.replace name kebab-to-camel-re #(.toUpperCase %2))))

(defn camel-to-kebab [name]
  (if (= \' (.charAt name 0))
    (subs name 1)
    (.toLowerCase (.replace name camel-to-kebab-re "$1-$2"))))

(defn coerce-key-to-string [k]
  (cond
    (satisfies? INamed k) (name k)
    ; TODO: decide if we want string keys to be excluded from translation, then they should be quoted here
    (string? k) k
    :else (.toString k)))

(defn cljs-primitive? [v]
  (or (nil? v)
      (boolean? v)
      (string? v)
      (number? v)
      (float? v)
      (keyword? v)
      (regexp? v)
      ; TODO: review this and maybe add other cases of primitive values shared between clj and cljs
      ))

(declare dynamic-translate-style-key)
(declare dynamic-translate-style)
(declare dynamic-translate-prop-key)
(declare dynamic-translate-prop-value)

(defn translate-style-kv-reducer [o [k v]]
  (let [translated-k (dynamic-translate-style-key k)
        translated-v (dynamic-translate-style v)]
    (gobj/set o translated-k translated-v)
    o))

(defn key->translation-mode [k]
  (if (= "style" (coerce-key-to-string k))
    STYLE_TRANSLATION
    NO_TRANSLATION))

(defn translate-prop-kv-reducer [o [k v]]
  (let [translated-k (dynamic-translate-prop-key k)
        translated-v (dynamic-translate-prop-value k v)]
    (gobj/set o translated-k translated-v)
    o))

(defn dynamic-reverse-translate-prop-key [name]
  (assert (string? name))
  (if-some [match (.match name special-case-reverse-prop-name-re)]
    (case (aget match 1)
      ("aria-" "data-") name
      "className" "class"
      "htmlFor" "for"
      "style" "style")
    (camel-to-kebab name)))

(defn dynamic-reverse-translate-style-key [name]
  (assert (string? name))
  (camel-to-kebab name))

; -- macro API --------------------------------------------------------------------------------------------------------------

(defn dynamic-translate-style-key [k]
  (kebab-to-camel (coerce-key-to-string k)))

(defn dynamic-translate-style [v]
  (cond
    (map? v) (reduce translate-style-kv-reducer (js-obj) v)
    (vector? v) (into-array (mapv dynamic-translate-style v))                                                                 ; TODO: this could be optimized
    (cljs-primitive? v) v
    :else (throw (js/Error. (str "Unsupported style value '" v "' type:" (type->str (type v)))))))

(defn dynamic-translate-prop-key [k]
  (let [name (coerce-key-to-string k)]
    (if-some [match (.match name special-case-prop-name-re)]
      (case (aget match 1)
        ("aria-" "data-") name
        "class" "className"
        "for" "htmlFor"
        "style" "style")
      (kebab-to-camel name))))

(defn dynamic-translate-prop-value [k v]
  (case (key->translation-mode k)
    NO_TRANSLATION v
    STYLE_TRANSLATION (dynamic-translate-style v)))

(defn dynamic-translate-props [v]
  ; TODO: raise user-friendly explanation
  (assert (map? v))
  (reduce translate-prop-kv-reducer (js-obj) v))

; -- bean API ---------------------------------------------------------------------------------------------------------------

(defn style-prop->key [prop]
  (keyword (dynamic-reverse-translate-style-key prop)))

(defn style-key->prop [key]
  (dynamic-translate-style-key key))

(defn style-bean [v]
  (bean v
        :prop->key style-prop->key
        :key->prop style-key->prop
        :recursive true))

(defn props-prop->key [prop]
  (keyword (dynamic-reverse-translate-prop-key prop)))

(defn props-key->prop [key]
  (dynamic-translate-prop-key key))

(defn props-val-transform [orig-style v]
  (if (and (object? orig-style) (identical? orig-style v))
    (style-bean v)
    v))

(defn props-bean [js-props]
  (let [style-val (.-style js-props)]
    (bean js-props
          :prop->key props-prop->key
          :key->prop props-key->prop
          :transform (partial props-val-transform style-val)
          :recursive true)))

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(comment
  )
