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

gzip -c1 > $input
linecount=`gzip -cd $input | wc -l`

modulus=$3
if [ -z $modulus ]
then
  modulus=$(( linecount * fp ))
fi

#sort -n can't handle bigints
export LC_ALL=C
gzip -cd $input | ./make-hash-sequences $modulus | awk -v len=${#modulus} '{ printf("% " len "s\n",$1) }' | sort -S 50% -u | tr -d ' ' | gzip -c1 > $hashes

outputlinecount=`gzip -cd $hashes | wc -l`
echo "{'lineCount': ${outputlinecount}, 'modulus': ${modulus}, 'unaryBits': ${unarybits}}" > /dev/stderr
gzip -cd $hashes | ./golomb-compressed-sequences $unarybits
rm $input $hashes