# Changelog

All notable changes to this project will be documented in this file.

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
