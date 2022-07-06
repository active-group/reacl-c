(ns reacl-c.test-util.dom-testing
  (:require [reacl-c.main.react :as main-react]
            [reacl-c.main :as main]
            [reacl-c.core :as c]
            [active.clojure.functions :as f]
            [active.clojure.lens :as lens]
            [cljs-async.core :as async]
            ["@testing-library/dom" :as dom-tu]
            ["@testing-library/react" :as react-tu])
  (:refer-clojure :exclude [get find]
                  :rename {get map-get}))

;; Note: effects and subscriptions are executed by default.

;; TODO: https://github.com/testing-library/user-event ?
;; TODO: getNodeText, getRoles, isInaccessible

(defn ^:no-doc with-config [config f]
  (let [previous (atom nil)]
    (react-tu/configure (fn [current]
                          (reset! previous current)
                          (clj->js config)))
    (async/try-finally f
                       #(react-tu/configure @previous))))

(defn- pretty-nodes [document nodes]
  (let [d (.createDocumentFragment document)]
    (doseq [x nodes]
      (.appendChild d (.cloneNode x true)))
    (react-tu/prettyDOM d)))

(defn- default-get-element-error [message container]
  (let [container (if (instance? js/Node container)
                    container
                    (.-container container))]
    (doto (js/Error (str message "\n\n"
                         (react-tu/prettyDOM container)))
      (aset "name" "TestingLibraryElementError"))))

(defn- run [aux]
  ;; run 'controlled', storing state in the :current atom
  (let [c (:current aux)
        aq (:action-queue aux)
        component (main-react/embed (main/execute-effects (:item @c))
                                    {:state (:state @c)
                                     :set-state!
                                     (fn [v] (reset! c (assoc @c :state v)))
                                     :handle-action!
                                     (fn [a]
                                       (if (some? @aq)
                                         (swap! aq conj a)
                                         (main/action-error a)))})]
    (reset! (:component aux) component)
    component))

(defn- aux
  ([env v]
   (aset env "_reacl_c_aux" v))
  ([env]
   (aget env "_reacl_c_aux")))

(defn- rerender! [env]
  (.rerender env (run (aux env))))

(defn ^{:arglists '[(item options... f)]
        :doc "Calls `f` with a rendering environment that 'runs' the given item.
Options can be

- `:state` for the initial state of the item (defaulting to `nil`),
- `:visible?` specifying if the rendered dom should be made visible (the default looks at the `hidden` property of `js/document`),
- `:configuration` for configuration corresponing to https://testing-library.com/docs/dom-testing-library/api-configuration
- `:container`, `:baseElement`, `:hydrate` and `:wrapper` for rendering options corresponing to https://testing-library.com/docs/react-testing-library/api#render-options
- `:queue-actions?` to specify that actions emitted by the rendered item should be stored in a queue, accessible via [[pop-action!]]

Note that if `f` is asynchronous (returns a promise), then rendering will continue until the promise is resolved or rejected.
 "}
  rendering
  [item & args]
  (assert (c/item? item) item)
  (assert (not-empty args))
  (let [f (last args)
        options (apply hash-map (drop-last args))]
    (assert (ifn? f) f)
    (let [container (map-get options :container
                             ;; by default, hide the container if the document is visible.
                             ;; Note: visibility changes when putting tabs in foreground/background (in Chrome)
                             (let [visible? (map-get options :visible? (.-hidden js/document))]
                               (let [e (js/document.createElement "DIV")
                                     body js/document.body]
                                 (when (not visible?) (set! (.-visibility (.-style e)) "hidden"))
                                 (.appendChild body e)
                                 e)))

          initial-state (:state options)

          ;; Note: because a toplevel state change requires a
          ;; (.rerender env), and we also want to have access to the
          ;; toplevel state directly, we start with and empty item
          ;; (which cannot change state) to create the env, then set a
          ;; watch on the atom below, after the env has beed created.
          a {:current (atom {:state nil :item nil})
             :component (atom nil)
             :action-queue (atom (when (:queue-actions? options) #queue []))}
          
          env (doto (react-tu/render (run a)
                                     (clj->js (-> options
                                                  (dissoc :visible?
                                                          :queue-actions?
                                                          :state
                                                          :configuration)
                                                  (assoc :container container))))
                (aux a))]

      (add-watch (:current a) :updater
                 (fn [_ atom old new]
                   (rerender! env)))
      (reset! (:current a) {:state initial-state :item item})
      
      (with-config (merge {:getElementError default-get-element-error}
                          (:configuration options))
        (fn []
          (async/try-finally (fn []
                               (f env))
                             (fn []
                               (react-tu/cleanup env))))))))

(defn ^:no-doc as-fragment [env]
  (.-asFragment env))

(defn ^:no-doc act [env & args]
  (apply (.-act env) args))

(defn unmount!
  "Stops rendering in the given rendering environment. You usually don't have to call this."
  [env]
  (.unmount env))

(defn update!
  "Change the item and state rendered in the given rendering
  environment. If not state is given, the state will stay as it is."
  ([env item]
   (let [a (aux env)]
     (swap! (:current a) merge {:item item}))
   nil)
  ([env item state]
   (let [a (aux env)]
     (swap! (:current a) merge {:state state :item item}))
   nil))

(defn current-state
  "Returns the current state of the item running in the given rendering environemnt."
  [env]
  (:state @(:current (aux env))))

(defn set-state!
  "Changes the state of the item running in the given rendering
  component to a new value."
  [env state]
  (let [a (aux env)]
    (swap! (:current a) merge {:state state})
    nil))

(defn queue-actions
  "Enables action queueing during the evaluation of the given
  thunk. This has the same effect as setting the option
  `:queue-actions?` in the call to [[rendering]]
  otherwise. See [[pop-action!]] to check for queued actions."
  [env thunk]
  (let [aq (:action-queue (aux env))
        prev @aq]
    (if (some? prev) ;; already active?
      (thunk)
      (async/try-finally
       (fn []
         (reset! aq #queue [])
         (thunk))
       (fn []
         (reset! aq prev))))))

(defn- pop-action* [env on-empty]
  (let [aq (:action-queue (aux env))]
    (if-let [as @aq]
      (if (empty? as)
        (on-empty)
        (let [v (peek as)]
          (reset! aq (pop as))
          v))
      (throw (js/Error "Action queueing not active. Set the option `:queue-actions?` in the call to `rendering`, or use `queue-actions`.")))))

(defn pop-action!
  "Removes and returns an action that was previously emitted by the item
  running in the given rendering environment. Throws if action queueing
  is not enabled, either by the `queue-actions?` option
  to [[rendering]], or by [[queue-actions]]. Also throws if the queue
  is empty, unless a second argument is provided, which is then
  returned instead.

  Note that you can use [[wait-for]] to wait for an action that is
  emitted asynchronously.
  "
  ([env]
   (pop-action* env #(throw (js/Error (str "No actions captured yet.")))))
  ([env default]
   (pop-action* env (constantly default))))

(defn send-message!
  "Sends a message to the item running in the given rendering environment."
  [env msg]
  (main-react/send-message! @(:component (aux env)) msg))

(defn container
  "Returns the container element used in the given rendering environment."
  [env]
  (.-container env))

(defn base-element
  "Returns the base element used in the given rendering environment."
  [env]
  (.-baseElement env))

(defn pretty-dom
  "Returns a string showing a pretty HTML markup corresponding to the
  given DOM node as good as possible."
  [node]
  (react-tu/prettyDOM node))

(defn within-node
  "Specifies `node` as different root node in element queries. `base`
  should be either a rendering environemnt, or the result of a
  previous call to `within-node`."
  [base node]
  ;; Note: does not need base/env yet; but we might use it to copy
  ;; 'custom queries' from there to here; which imho is a flaw in
  ;; testing-library they are not inherited. 'within' has a second
  ;; argument to add new custom queries...
  (react-tu/within node))

(defn query
  "Returns matching node or nil, but throws if more than one node matches.

  For example `(query env (by-text \"Hello\"))` will find an element containing the text \"Hello\"."
  [env what]
  (what env "query"))

(defn query-all
  "Returns all matching nodes.

  For example `(query-all env (by-text \"Hello\"))` will find all elements containing the text \"Hello\"."
  [env what]
  (what env "queryAll"))

(defn get
  "Returns the matching node, and throws if none or more than one node matches.

  For example `(get env (by-text \"Hello\"))` will find an element containing the text \"Hello\"."
  [env what]
  (what env "get"))

(defn get-all
  "Returns all matching nodes, but throws if no nodes match.

  For example `(get-all env (by-text \"Hello\"))` will find all elements containing the text \"Hello\"."
  [env what]
  (what env "getAll"))

(def ^:private default-wait-for-options
  ;; Note: the default onTimeout appends the markup of 'container' again to the
  ;; messages generated by getBy*. Terrible, as it's often even the whole document.
  ;; TODO: printing the wait time would be nice though. (default 1s)
  {:onTimeout identity})

(defn wait-for*
  "Functional version of the macro [[wait-for]]."
  [f & options]
  ;; TODO: assert proper options
  (react-tu/waitFor f (clj->js (merge default-wait-for-options (apply hash-map options)))))

(defn wait-for-removal*
  "Functional version of the macro [[wait-for-removal]]."
  [f & options]
  ;; TODO: assert proper options
  (react-tu/waitForElementToBeRemoved f (clj->js (merge default-wait-for-options (apply hash-map options)))))

(defn- find* [f where what options]
  (apply wait-for* (fn [] (f where what))
         options))

(defn find
  "Asynchronously returns the matching node. If none or more than one nodes
  match, this will repeat the search until a timeout expires and then
  throw an exception. Options are the same as for [[wait-for]]."
  [env what & options]
  (find* get env what options))

(defn find-all
  "Asynchronously returns all matching node. If no nodes match, this
  will repeat the search until a timeout expires and then throw an
  exception. Options are the same as for [[wait-for]]."
  [env what & options]
  (find* get-all env what options))

;; TODO: add 'removed'...? Note: wait-for-removal requires the item to be present first. So what about 'not-find'/absense?

(defn- query-fn [where how q]
  ;; how = find|get|query|findAll|getAll|queryAll
  ;; q = ByName|ByLabel etc.
  ;; or
  ;; q = a vector of [queryAll, query, getAll, get, findAll, find], as returned by build-queries
  (if (string? q)
    (aget where (str how q))
    (let [[qa q ga g fa f] q]
      (partial (case how
                 "queryAll" qa
                 "query" q
                 "getAll" ga
                 "get" g
                 "findAll" fa
                 "find" f)
               where))))

(defn- run-q [where how q & args]
  (let [f (query-fn where how q)]
    (when (nil? f)
      (if (string? q)
        (throw (js/Error (str "Query " how q " not found in " where ".")))
        (throw (js/Error (str "Query " how " not defined in custom query " q ".")))))
    (apply f args)))

(let [h (fn [q args args-to-js where how]
          (apply run-q where how q (args-to-js args)))]
  (defn- q-runner [q args & [args-to-js]]
    (f/partial h q args (or args-to-js identity))))

(defn- build-js-query-fn
  [query-all make-multi-error make-missing-error & [args-to-js]]
  (let [r (react-tu/buildQueries query-all
                                 make-multi-error make-missing-error)]
    ;; buildQuery does not return the query-all, but we want that to have all in one binding.
    (let [qs (vec (cons query-all (array-seq r)))]
      (fn [& args]
        (q-runner qs args)))))

(defn build-query-fn
  "Creates a function that returns custom query like [[by-label-text]],
  via a function that implements the 'query-all' logic, which should
  return a sequence of all matching nodes, and two functions that
  return an error message for the cases of finding more than one,
  resp. nothing at all.

  For example:

```
  (def by-my-property (build-query-fn (fn [env v1] node-seq...) (fn [env v1] \"Error\") (fn [env v1] \"Error\")))

  (find node (by-my-property \"foo\"))
```

  See https://testing-library.com/docs/react-testing-library/setup#add-custom-queries for some additional notes."
  [query-all make-multi-error make-missing-error]
  (build-js-query-fn (fn [& args]
                       ;; array or js interable?
                       (to-array (apply query-all args)))
                     make-multi-error make-missing-error))

(let [to-js (fn [[text options]]
              [text (clj->js options)])]
  (defn- std-q-runner [q text options]
    (q-runner q
              [text (apply hash-map options)]
              to-js)))

(defn by-label-text
  "Query by the label text of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#bylabeltext"
  [text & options]
  (std-q-runner "ByLabelText"
                text options))

(defn by-placeholder-text
  "Query by the placeholder text of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#byplaceholdertext"
  [text & options]
  (std-q-runner "ByPlaceholderText"
                text options))

(defn by-text
  "Query by the text of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#bytext"
  [text & options]
  (std-q-runner "ByText"
                text options))

(defn by-alt-text
  "Query by the alternate text of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#byalttext"
  [text & options]
  (std-q-runner "ByAltText"
                text options))

(defn by-title
  "Query by the title of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#bytitle"
  [text & options]
  (std-q-runner "ByTitle"
                text options))

(defn by-display-value
  "Query by the text shown in an input element. See
  https://testing-library.com/docs/dom-testing-library/api-queries#bydisplayvalue"
  [text & options]
  (std-q-runner "ByDisplayValue"
                text options))

(defn by-role
  "Query by the Aria role of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#byrole"
  [text & options]
  (std-q-runner "ByRole"
                text options))

(defn by-testid
  "Query by the `:data-testid` attribute of a node. See
  https://testing-library.com/docs/dom-testing-library/api-queries#bytestid"
  [value & options]
  (std-q-runner "ByTestId"
                value options))

(defn ^{:deprecated true
        :doc "Query by the `:data-testid` attribute of a node. Use [[by-testid]] instead."}
  by-test-id
  [value & options]
  (apply by-testid value options))

(def ^{:doc "Query for anything."} anything
  ((build-query-fn (fn [env]
                     (letfn [(r [n]
                               (let [nodes (.-childNodes n)]
                                 (concat nodes (mapcat r nodes))))]
                       (r (container env))))
                   (constantly "Found multiple elements")
                   (constantly "Unable to find any element"))))

(def ^{:doc "Query for nothing."} nothing
  ((build-query-fn (constantly nil)
                   (constantly "Found a multiple of no elements, which sounds impossible")
                   (constantly "Unable to find no element, as expected"))))

(let [and-q-0
      (build-query-fn (fn [env q1 more]
                        (reduce (fn [res q]
                                  (if (empty? res)
                                    res
                                    (let [s (set (query-all env q))]
                                      (filter s res))))
                                (query-all env q1)
                                more))
                      ;; TODO: better error messages would be nice; but how?
                      (constantly "Found multiple elements that match all of the given queries")
                      (constantly "Unable to find any element that matches all of the given queries"))]
  (defn ^{:arglists '([& queries])
          :doc "Query for all elements that match all of the given queries."}
    all-of
    ([] anything)
    ([q] q)
    ([q1 & more]
     (and-q-0 q1 more))))

(let [or-q-0
      (build-query-fn (fn [env q1 more]
                        (distinct (mapcat (partial query-all env)
                                          (cons q1 more))))
                      ;; TODO: better error messages would be nice; but how?
                      (constantly "Found multiple elements that match one or more of the given queries")
                      (constantly "Unable to find any element that matches any of the given queries"))]
  (defn ^{:arglists '([& queries])
          :doc "Query for all elements that match any of the given queries."}
    any-of
    ([] nothing)
    ([q] q)
    ([q1 & more]
     (or-q-0 q1 more))))

(let [sub-q (build-query-fn (fn [env base-q child-q]
                              (mapcat #(query-all (within-node env %) child-q)
                                      (query-all env base-q)))
                            ;; TODO: better error messages would be nice; but how?
                            (constantly "Found multiple elements that match the query")
                            (constantly "Unable to find any element that matches the given query"))]
  (defn within
    "Query for elements matching `child-q` within the elements matching `base-q`."
    [base-q child-q]
    (sub-q base-q child-q)))

(let [q (build-js-query-fn (fn [where attribute text & options]
                             (dom-tu/queryHelpers.queryAllByAttribute attribute (container where) text
                                                                      (clj->js (apply hash-map options))))
                           ;; TODO: what to print if text is a predicate? (not string nor regex)
                           (fn [where attribute text & options]
                             (str "Found multiple elements with: [" attribute "=" text "]"))
                           (fn [where attribute text & options]
                             (str "Unable to find an element by: [" attribute "=" text "]")))]
  (defn by-attribute
    "Query by the given attribute, matching nodes where the attribute
  value matches the given string, regex of predicate `text`."
    [attribute text & options]
    ;; Note: text here (and in all other standard queries, can be a predicate fn too.
    (apply q attribute text options)))

(defn- fire-event* [node event]
  (react-tu/fireEvent node event))

(defn fire-event
  "Fire an event on the given DOM `node`, where `event` can be a DOM
  Event object, or a keywork like `:click` for standard events. See
  https://github.com/testing-library/dom-testing-library/blob/master/src/event-map.js
  for a list."
  [node event & [event-properties]]
  (fire-event* node
               (if (instance? js/Event event)
                 (do (assert (nil? event-properties) "Additional event properties can not be set on an event object argument.")
                     event)
                 ;; Note: createElemnt.click() uses default properties, but createEvent("click") doesn't :-/ damn!
                 ;; How can we distinguish it? Let's use: keyword event => default; string event => generic.
                 (let [f (if (string? event)
                           (partial react-tu/createEvent event)
                           (or (aget react-tu/createEvent (name event))
                               (throw (js/Error (str "Not a known standard event: " (pr-str event) ".")))))]
                   (f node (clj->js event-properties))))))
