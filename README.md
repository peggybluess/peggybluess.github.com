# peggybluess.github.com

Photographer portfolio website made with [Axiom](https://github.com/transient-haskell/axiom)

Axiom is the browser side of Transient. It also can execute code in the Web server (with `atServer`)  if the content is served by a transient program.  But in this case, It is a full client-side application, initialized with `runBody`. 

It is hosted in github itself. This repo is the very hosted content.

Site:  http://mariajtalonso.com   https://peggybluess.github.io/

The entire Haskell Source:  [Fotos.hs](https://github.com/peggybluess/peggybluess.github.com/blob/master/fotos.hs)
The rest is CSS and JavaScript generated by GHCJS. It also has photos and a configuration file content.txt which is included in
the source code at compilation time.


How to make it run:

It uses the [GHCJS](https://github.com/ghcjs/ghcjs) haskell compiler to Javascript.

clone the repo:

git clone https://github.com/peggybluess/peggybluess.github.com

cd peggybluess.github.com

To compile it and update the javascript file with each change in fotos.hs execute:

ghcjs fotos.hs -o ../peggybluess.github.com

And put the content of the repo in a server.
