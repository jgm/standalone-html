standalone-html is a simple filter that tries to create a standalone
HTML file by incorporating scripts, styles, and images using 'data:'
URIs, thus eliminating external dependencies.

Installing:

    cabal install

Usage:

    standalone-html < myfile.html > mynewfile.html
