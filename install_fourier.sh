#!/bin/sh

elm-make Fourier.elm
scp index.html elm.js birdglue:public_html/fourier-circles
