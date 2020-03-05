# Introduction to Reacl-c

## Organization

Reacl-c consists of two namespaces:

- `reacl-c.core` for basic functions, and
- `reacl-c.dom` for functions related to a browser's DOM.

In the following, a `:require` clause like this is assumed:

```clj
 (:require [reacl-c.core :as c :include-macros true]
           [reacl-c.browser :as browser]
           [reacl-c.dom :as dom])
```

## Basic concepts

In Reacl-c, the user interface is defined by an /Item/.

All items have an optional /visual appearance/, consisting of one or
more DOM nodes (elements or text). Items without a visual appearance,
are called invisible.

The `reacl-c/dom` namespace contains functions that construct items
with a visual appearance, while the functions from `reacl-c/core` that
create items, are related to the behaviour of the user interface, and
are usually invisible themself.

Just like in the DOM, some items can have child items. So together
they form a tree, with one item at the top.

All items may emit /actions/, which propagate upwards in the item
tree, until they are handled.

All items may be sent /messages/ to. Those can be handled, but some
items forward such a message down to a child item, while others always
raise an error.

All items have a /state/, which may change over time, as the user
interacts with the user interface. That state however, is not stored
or mutated /inside/ an item. It can instead be thought of as an
implicit argument the item, which can be passed to it multiple
times. Also, a change of the state can be though of as an implicit
return value from the item.

Most items just pass the state they get down to all their child items,
and propagate any changed state from any of their children upwards in
the item tree. A state change that reaches the toplevel, is then
actually stored by the Reacl-C runtime, and passed back to the root
item as its new state. Like that, the state becomes an /application
state/, as all items share the same state, and all of them can update
any part of it. But of course, there are ways to break this basic
model up at certain points in the item tree - by either making a child
item /work/ on just a part of the state, or by introducting new state,
that is stored at a lower part in the tree as so called /local state/.

Finally, an item can be run in a browser, underneath a specific DOM
node, by calling `reacl-c.browser/run`, specifying an initial state
for the given item:

```clojure
(browser/run (js/document.getElementById "app")
             my-item
			 my-initial-state)
```

With the basic concepts explained, we can now go through all the
functions that allow you to create new items, and which visual
appearance and behaviour they will have.

## DOM and other simple items

The most simple item is the empty item:

```clojure
c/empty
```

It has no visual appearance, can take any state, never changes the
state, never emits any actions and sending a message to it always
raises an error.

The empty item is actually nothing else than a /fragment/ item with no
children. But fragment items can be constructed from arbitrarily many
items:

```clojure
(c/fragment item1 ...)
```

The visual appearance of fragment items is that of the child items
concatenated. They can take any state, which they pass to all child
items, and any changed state from a child item is passed upwards
unchanged. They also pass all actions from any child item upwards, and
sending a message to a fragment always raises an error.

Next, all strings are items:

```clojure
"Hello World"
```

Strings have a corresponding text node as their visual appearance in
the browser, can take any state, don't change the state, don't emit
any actions, and sending a message to them raises an error.

Now DOM elements make things interesting. For all HTML tags, there is
a function in the `reacl-c.dom` namespace, that creates an item with
that visual appearance. Some examples:

```clojure
(dom/br)

(dom/h1 "Hello")

(dom/a {:href "#"} "Link")

(dom/div (dom/span {:class "verb"} "Bla") "Blub")

(dom/div {:style {:width "100px"}} "Foo")
```

All these functions take an optional map of attributes as the first
argument, and then arbitrarily many child items. The `style` attribute
is somewhat special, in that it can be a map of CSS style
settings.

DOM elements can take any state, which they pass down to all child
items, and they pass any changed state from a child upwards. They also
pass actions from any child upwards, and raise an error if a message
is sent to them.

Things get interesting, when adding event handler to DOM
elements. Resembling HTML, event handlers are functions that can be
attached to an element via attributes that start with `:on`, like
`:onclick`:

```clojure
(dom/button {:onclick (fn [state event] ...)} "Click me")
```

A event handler function takes the current state of the item and the
DOM Event object as arguments. It can then do three things: Pass a
state change upwards, emit an action, or send a message to some other
item. Do specify that, the event handler function must return a value
created by `c/return`. For example:

```clojure
(c/return)

(c/return :state "foo")

(c/return :action "baz")

(c/return :message [target "message"])

(c/return :state "foo" :action "bar" :action "baz")
```

So the state change is optional, but can be specified at most
once. Actions and messages can be specified multiple times. What a
valid target for messages is, will be described later in this
document.

The `c/return` values will also be used to specify the reaction to
many other discrete events, like when handling actions for
example. They are a central concept of Reacl-c.

## Items that add behavior

...TODO intro

### Working with state

With the items described so far, your application will be quite static
and always look the same. Change over time is introduced by /dynamic/
items:

```clojure
(c/dynamic
  (fn [state]
    (if (> state 0)
	  "positive"
	  "not positive")))
```

So `dynamic` creates a dynamic item. Initially and whenever the state
changed, the given function is called and must return an item. The
dynamic item has the same visual appearance as the returned item for
the current state, passes the state down to it and every state change
upwards, passes all actions upwards, and forwards all messages sent to
it to the item returned for the current state.

Reacl-c also offers a convenient macro to define functions that create
dynamic items, which should usually be used:

```clojure
(c/defn-dynamic greeting state [lang]
  (dom/div (if (= lang "de") "Hallo, " "Hello, ")
           state))
		   
(greeting "de")   ;; is an item
```

Items created by this `greeting` function expect a string as their
state - the name of a person for example. To use this item in a place
where the state is a map of things, we can use `focus`:

```clojure
(c/focus :name (greeting "de"))
```

This creates an item with the same visual appearance as the item
passed as the second argument, but the state can be any associative
collection with a key `:name` in it. When the `greeting` item would
cause a state change to a new string, the focused item will embed the
new string in its current state via `assoc` in this case, and pass the
resulting new map as a state change upwards. Of course the focused
item also passes up all actions from the inner item and forwards all
messages sent to it down.

The possible values for the first argument to `focus` are not
restricted to keywords. If you pass an integer value, then the
returned item focuses on the n-th item of a vector. And you can also
pass a function of two arities. The single arity is used to extract
the inner state from the outer state, and the two-argument arity is
used to embed a new inner state in the current outer state:

```clojure
(fn
  ([outer-state] ...inner-state)
  ([outer-state new-inner-state] ...new-outer-state))
```

This conforms to the functional concept of a /lens/. All functions
like this can be used, as well as keywords and integers with the
aforementioned meaning. Reacl-c includes a few lenses that are
frequently used: `c/first-lens` on the first part of a tuple,
`c/second-lens` on the second part, the identity lens `c/id-lens`, and
a `c/merge-lens` that merges two associative collections into one in a
certain way. For many more predefined lenses and lens combinators,
take a look at
[`active.clojure.lens`](https://github.com/active-group/active-clojure).

The next thing concerning the state of items, is introducing new state
into a branch of the item tree, or hiding a part of the inner state,
which is the same thing but from the other perspective. The most
primitive way to do that, is the `local-state` function:

```clojure
(c/local-state 42 (c/dynamic pr-str))
```

If the state of the returned item is `"foo"` for example, then the
state of the inner dynamic item will be `["foo" 42]` initially. When
the inner item changes the state, the first part will be propagated
upwards as a state change, and the second part of the tuple will be
stored at this point in the item tree, and will be passed down again
afterwards. Note that this depends one a somewhat fixed position of
the returned item in the tree. If you 'move' that item to a different
position, it might 'restart' with the initial state again.
(TODO: mention 'keyed' here?)

There are a few convenience functions in Reacl-c that also apply some
lens on the `[outer inner]` tuple (`c/add-state`, `c/hide-state`), or
completely remove the outer state (`c/isolate-state`).

Finally, /static items/ can be created. Static items ignore the state
they receive from above, and to prevent mistakes, it's an error if
they try to change the state:

```clojure
(c/static (fn [] (dom/h1 "Foo")))

(c/defn-static header [text]
  (dom/h1 text))
```

They are similar to the items created by `c/isolate-state` with an
initial local state of `nil`, but because of the indirection via the
no-argument function, the inner item does not have to be constructed
again for the same function. Therefor, static items are mainly way to
increase the performance of your application, by 'cutting off' larger
item branches from any updated state. Note that it is important that
you pass the *same* function to `static` each time. In Clojure, an
anonymous function is a different object each time then `fn` form is
evaluated. So when working on performance, the use of the
`c/defn-static` or the `c/def-static` macros is strongly encouraged.

### Working with actions

### Working with messages


## The outside world

### Effects

### Subscriptions

### External control


