gcs
===

Golomb-compressed sequences (with indices) for large datasets

To create a golomb-compressed sequence, first install packages and compile code.

    cabal install bytestring-show pure-md5

    ghc --make -O2 make-hash-sequences.hs
    ghc --make -O2 golomb-encode.hs
    ghc --make -O2 make-index.hs
    ghc --make -O2 base-64-encode.hs

And then, write one key per line on stdin to mk-gcs.sh, and mk-gcs.sh will write binary data to stdout, and will write a JSON object to stderr.
(The JSON object makes sense to use in a browser, the binary data makes sense to use on the command line.)
