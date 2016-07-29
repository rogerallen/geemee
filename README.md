# geemee

geemee is under construction.

## Overview

FIXME: Write a paragraph about the library/project and highlight its goals.

## Setup

You'll need a local-repo of https://github.com/rogerallen/gamma/ due to issue #1

Basically,
```
git clone the repo
git checkout fix-eval
lein jar
lein localrepo install target/gamma-0.0-135-15-gfd44.jar rogerallen/gamma 0.0-135-15-gfd44
```

To get an interactive development environment run:

    rlwrap lein figwheel

To clean all compiled files:

    lein clean

To create a production build run:

    lein do clean, cljsbuild once min

And open your browser in `resources/public/index.html`. You will not
get live reloading, nor a REPL.

## License

Copyright © 2016 Roger Allen

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
