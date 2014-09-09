# Sablono-tools

is a tiny library that supports selectors and transformations for [Sablono](https://github.com/r0man/sablono). The syntax is intended to match that of Enlive's [selectors](https://github.com/cgrand/enlive#selectors) and [transformations](https://github.com/cgrand/enlive#transformations).

Only a subset of Enlive's selectors and transformations are currently supported.

## Selectors

tag selector as `:tag`, example: `[:h1]` selects all elements whose tags are h1

id selector as `:#id`, example: `[:#apple]` selects the element whose id is "apple"

`any-node`, universal selector, example: `[:div any-node]` selects all children of a div

## Transformations

`content`: replace the content with what follows, example: `(content "some text")

`replace-vars`: replace any ${var} in text and attributes by the value found in the map,
example: `(replace-vars {:name "User" :group "Client"})`

Please see the [test cases](test/sablono-tools/core.cljs) for furhter examples.

## Differences from Enlive

* `template` is a function, not a macro
* Operates in the ClojureScript environment, not Clojure
* Operates on Sablono templates, not html

A Sablono template is a normal Clojure data structure: a vector whose first element is a keyword (representing the tag), optional second element is a map (representing the attrs), and remaining elements represent the body. For example:

`[:span {:class "foo"} "bar"]`

A template may be converted to html with the `html` macro from sablono.core. Sablono is based on [hiccup](https://github.com/weavejester/hiccup).
