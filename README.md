# Minimal package for poly1305 message-authentication codes

[![Build Status](https://travis-ci.org/thoughtpolice/hs-poly1305.png?branch=master)](https://travis-ci.org/thoughtpolice/hs-poly1305)
[![MIT](http://b.repl.ca/v1/license-MIT-blue.png)](http://en.wikipedia.org/wiki/MIT_License)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://www.haskell.org)

This package implements minimal bindings to the [poly1305][]
message-authentication code.  function, which was an SHA-3
finalist. It should be relatively easy to both depend on, or include
outright in your executable/package itself.

The underlying implementation is the `ref` code of `poly1305` from
[SUPERCOP][], which was originally implemented by Dan J. Bernstein.

[poly1305]: http://cr.yp.to/mac.html
[SUPERCOP]: http://bench.cr.yp.to/supercop.html

# Installation

It's just a `cabal install` away on [Hackage][]:

```bash
$ cabal install poly1305
```

# Join in

Be sure to read the [contributing guidelines][contribute]. File bugs
in the GitHub [issue tracker][].

Master [git repository][gh]:

* `git clone https://github.com/thoughtpolice/hs-poly1305.git`

There's also a [BitBucket mirror][bb]:

* `git clone https://bitbucket.org/thoughtpolice/hs-poly1305.git`

# Authors

See [AUTHORS.txt](https://raw.github.com/thoughtpolice/hs-poly1305/master/AUTHORS.txt).

# License

MIT. See
[LICENSE.txt](https://raw.github.com/thoughtpolice/hs-poly1305/master/LICENSE.txt)
for terms of copyright and redistribution.

[contribute]: https://github.com/thoughtpolice/hs-poly1305/blob/master/CONTRIBUTING.md
[issue tracker]: http://github.com/thoughtpolice/hs-poly1305/issues
[gh]: http://github.com/thoughtpolice/hs-poly1305
[bb]: http://bitbucket.org/thoughtpolice/hs-poly1305
[Hackage]: http://hackage.haskell.org/package/poly1305
