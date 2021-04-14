# Introduction to Reacl-c

## Organization

Reacl-c consists of these main namespaces:

- `reacl-c.core` for basic functions,
- `reacl-c.dom` for functions related to a browser's DOM, and
- `reacl-c.main` for functions to actually run an item in a browser.

In the following, a `:require` clause like this is assumed:

```clj
 (:require [reacl-c.core :as c :include-macros true]
           [reacl-c.main :as main]
           [reacl-c.dom :as dom])
```

## Basic concepts

In Reacl-c, the user interface is defined by an *Item*.

All items have an implicit *state*, an optional *visual appearance*,
consisting of one or more DOM nodes (elements or text), and some
*behaviour*, like how they initialize their state. Items without a
visual appearance, are called invisible.

The `reacl-c/dom` namespace contains functions that construct items
with a visual appearance, while the functions from `reacl-c/core` that
create items, are related to the behaviour of the user interface, and
are usually invisible themself.

Just like in the DOM, some items can have child items. So together
they form a tree, with one item at the top.

All items may emit *actions*, which propagate upwards in the item
tree, until they are handled.

All items may be sent *messages* to. Those can be handled, but some
items forward such a message down to a child item, while others always
raise an error.

All items have a *state*, which may change over time, as the user
interacts with the user interface. That state however, is not stored
or mutated *inside* an item. It can instead be thought of as an
implicit argument the item, which can be passed to it multiple
times. Also, a change of the state can be though of as an implicit
return value from the item.

Most items just pass the state they get down to all their child items,
and propagate any changed state from any of their children upwards in
the item tree. A state change that reaches the toplevel, is then
actually stored by the Reacl-C runtime, and passed back to the root
item as its new state. Like that, the state becomes an *application
state*, as all items share the same state, and all of them can update
any part of it. But of course, there are ways to break this basic
model up at certain points in the item tree - by either making a child
item *work* on just a part of the state, or by introducting new state,
that is stored at a lower part in the tree as so called *local state*.

Finally, an item can be run in a browser, underneath a specific DOM
node, by calling `reacl-c.main/run`, optionally specifying an initial
state for the given item:

```clojure
(main/run (js/document.getElementById "app")
          "Hello World"
          {:initial-state nil})
```

With these basic concepts explained, we can now go through all the
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

Things get interactive, when adding event handlers to DOM
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

Reacting to state changes over time is introduced by *dynamic*
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

Reacl-c also offers a convenient macro to create dynamic items called
`with-state-as`:

```clojure
(c/defn-item greeting [lang]
  (c/with-state-as state
    (dom/div (if (= lang "de") "Hallo, " "Hello, ")
             state)))
		   
(greeting "de")   ;; is an item
```

Note that the `defn-item` macro of Reacl-c was used here to define a
function that returns an item. That macro is basically like an
enhanced variant of Clojure's `defn`, but can only be used to
define abstractions over items. See below for more features of the
`defn-item` and `def-item` macro.

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
restricted to keywords. You can also pass any function of two arities:
The single arity is used to extract the inner state from the outer
state, and the two-argument arity is used to embed a new inner state
in the current outer state:

```clojure
(fn
  ([outer-state] ...inner-state)
  ([outer-state new-inner-state] ...new-outer-state))
```

This conforms to the functional programming concept of a *lens*. All
functions like this can be used, as well as keywords with the
aforementioned meaning. Look at a large collection of useful lenses
and lens combinators in
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
times is possible, but at each place it will start with `42` intially,
and then store updated states independantly from each other.

One way to allow an item to 'move' in a limited way, is adding a *key*
on it:

```clojure
(c/keyed some-item "some-key")
```

Within a list of child items (in a fragment or DOM item), a keyed item
may change its position over time and still keep the same local state
without being reset to the initial state. The keys within the same
child list must of course be unique.

Because it's often convenient to use the current value of the local
state to change the returned item, new state can also be introduced
directly with the `with-state-as` macro:

```clojure
(c/with-state-as [outer inner :local 42]
  (dom/div (pr-str (+ outer inner))))
```

The state of the returned item is bound to `outer`, and `inner` to the
new local state, initialized to `42`. Note that the state of the inner
item (the `div` in this case) is again a tuple of the outer and inner
states. The above is equivalent to

```clojure
(c/local-state 42
  (c/dynamic (fn [[outer inner]]
                (dom/div (pr-str (+ outer inner))))))
```

Finally, *static items* can be created. Static items ignore the state
they receive from above, and, to prevent mistakes, it's an error if
they try to change the state:

```clojure
(c/static (fn [] (dom/h1 "Foo")))
```

They are similar to the items created by `c/isolate-state` with an
initial local state of `nil`, but because of the indirection via the
no-argument function, the inner item does not have to be constructed
again, given the same function. Static items are mainly a way to
increase the performance of your application, by 'cutting off' larger
item branches from any state update. Note that it is important that
you pass the *same* function to `static` each time. In Clojure,
anonymous functions are different objects each time the `fn` form is
evaluated. So when used as an optimization, you should use the `defn-item`
macros of Reacl-c, which can define abstract static items in the
following way:

```clojure
(c/defn-item table-1-header :static [label1 label2]
  (dom/tr (dom/th label1) (dom/th label2)))
```

In this way, the body of `table-1-header` will not be evaluated again
nor re-rendered on any state change from above, as long as it's
used with same arguments.

### Working with actions

Actions emitted by an item should be handled somewhere up in the item
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
the same state that, and passes all actions emitted by it upwards.

The other task regarding messages, is of course sending messages to
items, either in reaction to an action or to a received message, for
example. The key concept to this are *references*. Messages can be
targeted indirectly to a reference, which resolve to a concrete item
at a place in the item tree, or directly to an item to which a reference
was assigned to. There are low-level utilities to do that
(`c/with-ref` and `c/set-ref`), but the most convenient way for the
common use cases is the `c/ref-let` macro:

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
`ref-let` item are forwarded to the item bound - `some-other-item` in
this case.

Note that an item with a reference assigned (`child` in this case),
can only be used once in the body of `ref-let`.

Also note that the item created by the `ref-let` macro is not
referentially transparent, i.e. evaluating the same code twice will
not be be equal as of Clojure's `=` function. When optimizing an
application for performance, you can use the functional equivalent
`c/ref-let*` to create referntially transparent items.

### Unexpected errors

Sometimes, items may fail at runtime, for example dynamic items that
make wrong assumptions on the state they get, or are being used on the
wrong state of course. There is one primitive way to handle such
errors, `c/error-boundary`, and a slightly more convenient way: the
items created by the `try-catch` function:

```clojure
(def try-item (dynamic (fn [state] (/ 42 state))))
(def catch-item (dynamic (fn [[state error]] ...)))

(c/try-catch try-item catch-item)
```

The item returned by `c/try-catch` will initially look and behave like
the item given as the first argument - the `try-item`. After that causes a
runtime exception, the `catch-item` will be shown instead. The state
of the `catch-item` will be a tuple of the outer state and the
exception value. The `catch-item` may then, automatically or after a
user interaction, change the second part of the tuple state to
`nil`. That causes the error to be cleared, and the `try-catch` item
shows the `try-item` again. Sometimes, you may also want to reset the
left part of the tuple, the main state, to something that might
prevent the error from happening again.

Note that these utilities will not catch errors in message, action or
event handlers, but only those during the creation or update of the item
tree after a state change.

### The lifetime of an item

Although items are usually just a referentially transparent
description of a visual appearance and a behaviour (i.e. items have no
identity), when they are used at a specific place in the item tree, a
certain lifetime can be associated with them, starting when they are
first used in that position, via changes of the state they get from
above over time, the points in time at which they handle messages and
action, to the point in time they are no longer used at that place in
item tree again.

Above, we already mentioned a few cases where the lifetime of an item
plays a role, e.g. for `local-state` items. But occasionally, one also
want's to make use of that and write items that can react to those
transitions in the lifecycle of an item.

The first such items are those created by the `c/init` and `c/finalize` functions:

```clojure
(c/fragment
  (c/init (c/return :action "I'm in use"))
  (c/finalize (c/return :action "I'm not in use anymore")))
```

In this example the first action is emitted by the resulting item
when it is used at some place in the tree, and the second action
when it is not used anymore there.

For more advanced reactions to lifecycle events, there are the `c/once`
and the `c/lifecycle` items.

Note that you can easily combine these items with others in a
fragment item, which is then equivalent to *their* lifetime:

```clojure
(c/fragment (c/init ...) some-other-item)
```

## The outside world

There are hardly any applications that do not interact with the
outside world and be useful at the same time. So most of the time, you
will want to modify the browser's session store, retrieve or push data
to a server, or modify the browser history. In this chapter we go
through the utilities that Reacl-c offers to do this in a safe,
functional and fully testable way.

### Effects

Another concept of Reacl-c is that of *effects*. You should
encapsulate all side effects of your application in effects - be it
the communication with a server, creating a random number or just
looking up the current time. Effects are a special kind of action,
which can not be captured by `c/handle-action`, but are implicitly
handled at the toplevel, by *executing* them. Effects can be created
by `c/effect`, but more conveniently with the `c/defn-effect` macro:

```clojure
(c/defn-effect reload! [force?]
  (.reload (.-location js/window) force?))
```

With this definition, an item may trigger a reload as a reaction to
some other event by returning the effect as an action. For example:

```clojure
(dom/buttom {:onclick (fn [state ev]
                        (c/return :action (reload! true)))})
```

Or, for effects that have a result, for example generating random numbers:

```clojure
(c/defn-effect rand-int [n]
  (clojure.core/rand-int n))
```

the function `c/handle-effect-result` can be used:

```clojure
(c/handle-effect-result
  (fn [state v]
    (c/return :state (assoc state :next-id v)))
  (rand-int 1000))
```

Note that `c/handle-effect-result` returns an item, which triggers the
given effect each time it is placed in the item tree somewhere.

### Subscriptions

Subscriptions are a high-level feature included in Reacl-c, which make
it easy to register at a global source of asynchronous discrete events
and create items that handle them. Such sources can be Ajax requests
to a HTTP server, which will usually create only one asynchronous
result, or an interval timer which can make inifinitely many:

```clojure
(c/defn-subscription interval-timer deliver! [ms]
  (let [id (js/window.setInterval (fn []
                                    (deliver! :tick))
                                  ms)]
    (fn [] (js/window.clearInterval id))))
```

With this definition, you can create an item that emits `:tick` as an
action every 100 milliseconds:

```clojure
(c/handle-action (interval-timer 100)
  (fn [state tick]
    (c/return ...)))
```

You can of course create multiple subscription items from the same
subscription definition. In this case, they will all have their own
timer running though.

Note that the subscription definition must return a *stop function*,
which is called when an item creted from it is removed from the item
tree. The subscription definition must make sure that the `deliver!`
function is not called again, after the stop function has been called.

There are also more primitive items that can be used to attach
asynchronous sources of events or data (`c/with-async-return` and
variants of it), but they are more difficult to use, as you must take
care that they stop emitting events when the receiving item is removed
from the item tree. Subscription items are the most convenient way.

### External control

When using Reacl-c in an outer framework, it is sometimes necessary to
communicate with a 'running item'. The `run` function from the
`reacl-c.main` namespace mentioned in the beginning of this
document, actually returns an *application handle*:

```clojure
(def my-app
  (main/run (js/document.getElementById "app")
            my-item))
```

If `my-item` handles messages sent to it, you can do so with `main/send-message!`:

```clojure
(main/send-message! my-app :a-message)
```

To receie results or handling unsolicited events from an application,
you could use an effect that sets an atom for example, or use the
`:handle-action!` option of `run` with a side effect that push actions
into a `core.async` channel for example.
