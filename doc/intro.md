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

The empty item is actually nothing else than a *fragment* item with no
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

An event handler function takes the current state of the item and the
DOM Event object as arguments. It can then do three things: Pass a
state change upwards, emit an action, or send a message to some other
item. To specify that, the event handler function must return a value
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
example. These return values are a central concept of Reacl-c.

## Items that add behavior

With the items described so far, your application will be very static,
always look the same, and the user cannot not really interact with
it. This chapter introduces the various ways to create items that
change their appearance over time, the ways to change state and how to
trigger or react to discrete events in the application.

### Working with state

With the items described so far, your application will be quite static
and always look the same. Change over time is introduced by *dynamic*
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
collection with a key `:name` in it. When the `greeting` item causes a
state change to a new string, the focus item will embed the new string
in its current state via `assoc` in this case, and pass the resulting
new map as a state change upwards. Of course the focused item also
passes all actions from the inner item upwards and forwards all
messages sent to it down.

The possible values for the first argument to `focus` are not
restricted to keywords. If you pass an integer value, then the item
focuses on the n-th item of a vector. And you can also pass any
function of two arities: The single arity is used to extract the inner
state from the outer state, and the two-argument arity is used to
embed a new inner state in the current outer state:

```clojure
(fn
  ([outer-state] ...inner-state)
  ([outer-state new-inner-state] ...new-outer-state))
```

This conforms to the functional programming concept of a *lens*. All
functions like this can be used, as well as keywords and integers with
the aforementioned meaning. Reacl-c includes a few lenses that are
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

If the state of the resulting item is `"foo"` for example, then the
state of the inner dynamic item will be `["foo" 42]` initially. When
the inner item changes the state, the first part will be propagated
upwards as a state change, and the second part of the tuple will be
stored at this point in the item tree, and will be passed down again
afterwards.

Note that this depends on the position of the item in the tree. If you
'move' that item to a different position, it might 'restart' with the
initial state again. Actually, as most primitive items in Reacl-c are
referentially transparent values, using the the created item multiple
times is possible, and at each place it will start with `42` intially,
but then store updated states independantly from each other.

One way to allow an item to 'move' in a limited way, is adding a *key*
on it:

```clojure
(c/keyed some-item "some-key")
```

Within a list of child items (in a fragment or DOM item), a keyed item
may change its position over time and still keep the same local state
and will not be reset to the initial state. The keys within the same
child list must of course be unique for that.

There are a few convenience functions in Reacl-c that additionally
apply some lens on the `[outer inner]` tuple (`c/add-state`,
`c/hide-state`) via `c/focus`, or completely remove the outer state
(`c/isolate-state`).

Finally, *static items* can be created. Static items ignore the state
they receive from above, and, to prevent mistakes, it's an error if
they try to change the state:

```clojure
(c/static (fn [] (dom/h1 "Foo")))

(c/defn-static header [text]
  (dom/h1 text))
```

They are similar to the items created by `c/isolate-state` with an
initial local state of `nil`, but because of the indirection via the
no-argument function, the inner item does not have to be constructed
again, given the same function. Static items are mainly a way to
increase the performance of your application, by 'cutting off' larger
item branches from any state update. Note that it is important that
you pass the *same* function to `static` each time. In Clojure,
anonymous functions are different objects each time then `fn` form is
evaluated. So when working on performance, the use of the
`c/defn-static` or the `c/def-static` macros is strongly encouraged.

### Working with actions

Actions emitted by an item, should be handled somewhere up in the item
tree. The `c/handle-action` creates an item that does this:

```clojure
(c/handle-action
  some-other-item
  (fn [state action]
    (c/return ...)))
```

Any action emitted by `some-other-item` is not passed upwards, but is
passed to the given function, which must return a `c/return` value to
specify what to do as a reaction to the action. See above for an
explanation of `c/return`. Note that `handle-action` returns a new
item; the action handler is not 'attached' to the given item in any
way. New state is passed unchanged to the inner item, and any state
changes made by it are passed upwards, and messages sent to the
resulting item are forwarded to the inner item.

There is also `c/map-actions`, that can be used to simply transform
actions on their way up in the tree.

### Working with messages

To create an item that accepts messages, the function
`c/handle-message` can be used:

```clojure
(c/handle-message
  (fn [state message]
    (c/return ...))
  some-other-item)
```

Just like always, the resulting item looks like `some-other-item`, has
the same state as it, and passes all actions emitted by it upwards.

The other task regarding messages, is of course sending messages to
items, either in reaction to an action or to a message received, for
example. The key concept to this are *references*. Messages can be
targeted indirectly to a reference, which resolve to a concrete item
at a place in the item tree, or to an item that a reference was
assigned to. There are low-level utilities to do that (`c/with-ref`
and `c/set-ref`), but the most convenient way for the common use
cases is the `c/ref-let` macro:

```clojure
(c/ref-let [child some-other-item]
  (c/handle-message
    (fn [state msg]
	  (c/return :message [child msg]))
    (dom/div child)))
```

This example defines an item, whose visual appearance is that of
`some-other-item`, wrapped in a `dom/div`, and messages sent to the
item are forwarded unchanged to `some-other-item`. Note that you must
use `child` at both places in this code - it is also just an item, but
it contains the reference information that is needed to identify the
target for the message. As usual, `ref-let` passes any state down and
up unchanged, as well as any action upwards. Messages sent to the
`ref-let` item are forwarded to the item defined in the body.

Note that an item with a reference assigned (`child` in this case),
can only be used once in the body of `ref-let`.

Also note that the item created by the `ref-let` macro is not
referentially transparent, i.e. evaluating the same code twice will
not be be equal as of Clojure's `=` function. When optimizing an
application for performance, you can use the functional equivalent
`c/ref-let*` to create referntially transparent items.

Sometimes, items may fail at runtime, for example dynamic items that
make wrong assumptions on the state they get, or are being used on the
wrong state of course. There is one primitive way to handle such
errors, `c/error-boundary`, and a slightly more convenient way, the
items created by the `try-catch` function:

```clojure
(def try-item (dynamic (fn [state] (/ 42 state))))
(def catch-item (dynamic (fn [[state error]] ...)))

(c/try-catch try-item catch-item)
```

The item returned by `c/try-catch` will initially look and behave like
the item given as the first argument - `try-item`. After that causes a
runtime exception, the `catch-item` will be shown instead. The state
of the `catch-item` will be a tuple of the outer state and the
exception value. The `catch-item` may then, automatically or after a
user interaction, change the second part of the tuple state to
`nil`. That causes the error to be cleared, and the `try-catch` item
shows the `try-item` again. Sometimes, you may also want to reset the
left part of the tuple, the main state, to something that might
prevent the error from happening.

Note that these utilities will not catch errors in message, action or
event handlers, but only those during the creation or update of the item
tree after a state change.

### The livetime of an item

Although items are usually just a referentially transparent
description of a visual appearance and a behaviour (i.e. items have no
identity), when they are used at a specific place in the item tree, a
certain livetime can be associated with them, starting when they are
first used in that position, via changes of the state they get from
above over time, the points in time at which they handle messages and
action, to the point in time they are no longer used at that place in
item tree again.

Above, we already mentioned a few cases where the livetime of an item
plays a role, e.g. for `local-state` items. But occasionally, one also
want's to make use of that and write items that can react to those
transitions in the livecycle of an item.

The first such items are those created by the `c/once` function:

```clojure
(c/once (fn [state] (c/return :action "I'm in use"))
        (fn [state] (c/return :action "I'm not in use anymore")))
```

The `once` function returns an item, which calls the first function
when used initially at some place in the item tree, and the second one
(which is optional) when it is not used there anymore. The first
function is actually also called on each state update, but the retured
`return` value is then only 'executed' when it is different from the
last time it was executed. So with `once` items you can do something
at the first and last points of an items livetime at some place in the
item tree, as well as the points in time when the state is
changed. Note that you can easily combine these items with others in a
fragment item, which is then equivalent to *their* livetime:

```clojure
(c/fragment (c/once ...) some-other-item)
```

## The outside world

### Effects

### Subscriptions

### External control

