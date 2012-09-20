#!/bin/sh

fp=$1
if [ -z $fp ]
then
  fp=100
fi
unarybits=$2
if [ -z $unarybits ]
then
  unarybits=`awk "BEGIN { print int(log($fp)/log(2)) }"`
fi

input=`mktemp`
hashes=`mktemp`
sequence=`mktemp`

gzip -c1 > $input
linecount=`gzip -cd $input | wc -l`

modulus=$3
if [ -z $modulus ]
then
  modulus=$(( linecount * fp ))
fi

indexfrequency=$4
if [ -z $indexfrequency ]
then
  indexfrequency=16384
fi

#sort -n can't handle bigints
export LC_ALL=C
gzip -cd $input | ./make-hash-sequences $modulus | awk -v len=${#modulus} '{ printf("% " len "s\n",$1) }' | sort -S 50% -u | tr -d ' ' | gzip -c1 > $hashes

gzip -cd $hashes | ./golomb-encode $unarybits > $sequence

outputlinecount=`gzip -cd $hashes | wc -l`
(echo '{'
  echo '"lineCount":' ${outputlinecount},
  echo '"modulus":' ${modulus},
  echo '"unaryBits":' ${unarybits},
  echo -n '"partialSumBitcounts":' ; gzip -cd $hashes | ./make-index $unarybits $indexfrequency ; echo ,
  echo -n '"b64EncodedGolombCodedSequence": "' ; ./base-64-encode < $sequence ; echo '"'
echo '}') > /dev/stderr

cat $sequence
rm $input $hashes $sequence
