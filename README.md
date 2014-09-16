# Sablono-tools

is a tiny library that supports selectors and transformations for [Sablono](https://github.com/r0man/sablono). The syntax is intended to match that of Enlive's [selectors](https://github.com/cgrand/enlive#selectors) and [transformations](https://github.com/cgrand/enlive#transformations).
The full Enlive selectors syntax is documented [here](http://cgrand.github.io/enlive/syntax.html).

Sablono represents DOM nodes as Clojure vectors: [tag attrs? content*], containing
a tag such as `:div`, an optional map of attrs such as `{:id "foo"}`,
and zero or more contents such as contained nodes. So a template may look like:

`[:span {:class "foo"} "bar"]`

Sablono is based on [hiccup](https://github.com/weavejester/hiccup), which uses the same format. A fuller description of the DOM/template syntax is [here](https://github.com/weavejester/hiccup/wiki/Syntax).


The Sablono-tools `template` function takes a source template in Sablono's
format and a sequence of selector-transformer pairs, and returns the transformed
template. The pairs are processed in sequence: for each pair, the transformer
will be run on all nodes that match the selector.

A selector is either a single selector step, representing a Boolean function
of a DOM node, such as "Is the node's tag :p?"; or a chain of steps, which represents a Boolean function over the DOM hierarchy, such as "Is the node a child of the node
whose id is 'To-Do List'?".

Only a subset of Enlive's selectors and transformations are currently supported;
adding more should be straightforward.

Note that we always write the selector within a vector,
so `[:h1]` and `[[:p (attr? :lang)]]` each contain a single step,
while `[:p (attr :lang)]` contains two steps and is therefore a chain.

## Single-Step Selectors

tag selector as `:tag`, example: `[:h1]` selects all elements whose tags are h1

id selector as `:#id`, example: `[:#apple]` selects the element whose id is "apple"

`any-node`, universal selector, example: `[any-node]` selects all nodes

AND selector as a vector of selectors, example: `[[:p (attr? :lang)]]`
selects all elements that have a `lang` attribute AND have a :p tag

OR selector as a set of selectors, example: `[#{:p (attr? :lang)}]`
selects all elements that have a `lang` attribute OR have a :p tag

## Chained Selectors

several selectors, example: `[:p (attr? :lang)]` selects all elements that have a
`lang` attribute and are descendants of a :p node. Each selector applies to descendants
of nodes matching the preceding selector.

several selectors with the special form `:>`, example: `[:p :> (attr? :lang)]` selects all
elements that have a `lang` attribute and are immediate children of a :p node.
A `:>` preceding any selector causes it to apply only to immediate children
(rather than any descendant) of nodes matching the preceding selector.

## Transformations

`set-attr`

`remove-attr`

`add-class`

`content`: replace the content with what follows, example: `(content "some text")

`replace-vars`: replace any ${var} in text or attributes by the value found in the map,
example: `(replace-vars {:name "User" :group "Client"})`

Please see the [test cases](test/sablono-tools/core.cljs) for further examples.

## Differences from Enlive

* No macros: `template` is a function, not a macro
* Operates in the ClojureScript environment, not Clojure
* Operates on Sablono templates, not html
* User-defined selector must be simply a Boolean function of the node, not a state machine
* Simpler, no compile phase
* Undoubtedly slower


A template may be converted to a React component with the `html` macro from sablono.core.

## Tests

You can run the tests as follows:

$ `lein cljsbuild test`

You must have phantomjs installed.

