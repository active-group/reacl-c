# Changelog

All notable changes to this project will be documented in this file.

## [0.12.0]

- Minimum React version is now 18!

- React issues a Warning if `reacl-c.main/run` is called multiple
  times on the same DOM node. This happens typically with hot code
  reloading.  To prevent that use the new function `reacl-c.main/root`
  with a `defonce`, and pass that to `run`, which you can do
  repeatedly without warning.

- The WebComponent utility (`reacl-c.main.wc`) has been moved to its own
  separate library [reacl-c-wc](https://github.com/active-group/reacl-c-wc).

- `reacl-c.main.react/embed` now assumes that state changes (if allowed)
  take effect eventually. Previously toplevel action handlers didn't
  see the new state, if state was changed and an action emitted
  simultanously. This might break some usages that relied on the
  previous behaviour.

- Event handler names. Potentially breaking change:

  DOM element event handler names must now be like React wants them,
  i.e. camel-cased: `:onClick`. Other forms like `:onclick` won't
  work anymore. React prints a warning when it doesn't match.
  
  Similarly, in custom elements (i.e. `main.wc/use` and via `dom/h`),
  the event handler names must now match the event name, except for
  the third letter, which must be uppercase and is transformed to
  lowercase. So in order to handle `myEvent` you must use `:onMyEvent`.
  An assertion is raised if the third letter is not uppercase.

## [0.11.0]

### Removed

- Removed `reacl-c.core/handle-effect-result` in favour of
  `reacl-c.core/execute-effect`. To migrate just flip the arguments
  around.
  
- The `dom-testing` namespace has moved to its own library
  [reacl-c-testing](https://github.com/active-group/reacl-c-testing).
  
- The Reacl interop (`reacl-c.main.reacl` and `reacl-c.interop.reacl`)
  have to its own library
  [reacl-c-reacl](https://github.com/active-group/reacl-c-reacl)

### Changed

- The representation/translation to React classes has been overhauled
  greatly. The majority of classes used now are generated for each
  `defn-item` (and `defn-dom`). This makes the React component
  hierarchy more useful when debugging or analyzing the performance of
  an application.  It may also increase the general performance
  because less React classes are generated and in a more predictible
  way.
