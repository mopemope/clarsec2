# clarsec

clarsec is an attempt to port haskell parsec to clojure

This library is inspired on http://kotka.de/projects/clojure/parser.html and uses the Meikel Brandmeyer's "monad" library (slightly adapted), as I didn't understand how the clojure contrib monad
library works.

## Installation
-------------

    (defproject your-project "0.0.1-SNAPSHOT"
           :description "descriptions for your project"
           :dependencies [[org.clojars.mopemope/clarsec "0.0.1"]
                           ...]
           ...)
## Usage

there is an example parser which I ported straight from a Haskell Parsec code.

The library comes with a small number of basic combinators. I hope it will be useful.


## License

http://www.apache.org/licenses/LICENSE-2.0.html
