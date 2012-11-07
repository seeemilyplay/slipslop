SlipSlop
====================

This is an executable and library for playing around with probabilistic data structures for top-k queries.
It's called slipslop because it's answering tip top queries, but in a slippy sloppy way.

Please see: http://highlyscalable.wordpress.com/2012/05/01/probabilistic-structures-web-analytics-data-mining/

It's written in Haskell, so to install the executable just do 'cabal install'.

You then run it like this:

slipslop stream-summary -k 10 -n 100000 histogram <some_sample_histogram.csv>

Or you can run it with a stream of elements like this:

cat <some_element_file.txt> | slipslop count-min-sketch -k 10
