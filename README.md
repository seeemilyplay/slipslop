SlipSlop
====================

This is an executable and library for playing around with probabilistic data structures for top-k queries.

Please see: http://highlyscalable.wordpress.com/2012/05/01/probabilistic-structures-web-analytics-data-mining/

It's written in Haskell, so to install the executable just do 'cabal install'. You then run it like this:

slipslop stream-summary -n 100000 samples/all_day_consumer_count_histogram.txt

It's called slipslop because it's answering tip top queries, but in a slippy sloppy way.