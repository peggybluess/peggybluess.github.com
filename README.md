# peggybluess.github.com
Photographer portfolio website made with [Axiom](https://github.com/transient-haskell/axiom)

It is a full client-side application server statically from this repo by git.

Site:  http://mariajtalonso.com   https://peggybluess.github.io/

Source:  Fotos.hs

How to make it run:

It uses the [GHCJS](https://github.com/ghcjs/ghcjs) haskell compiler to Javascript.

clone the repo:

git clone https://github.com/peggybluess/peggybluess.github.com

cd peggybluess.github.com

To compile it and update the javascript file with each change in fotos.hs execute:

ghcjs fotos.hs -o ../peggybluess.github.com

And put the content of the repo in a server.
