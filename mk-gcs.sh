#!/bin/sh

fp=$1
if [ -z $fp ]
then
  fp=100
fi

input=`mktemp`
hashes=`mktemp`
compressionsizes=`mktemp`

gzip -c1 > $input
linecount=`gzip -cd $input | wc -l`

modulus=$(( linecount * fp ))

gzip -cd $input | ./make-hash-sequences $modulus | gzip -c1 > $hashes

for i in 4 5 6 7 8 9 10 11 ; do
  seqname=`mktemp`
  gzip -cd $hashes | ./golomb-compressed-sequences $i > $seqname
  bytecount=`wc -c < $seqname`
  echo -n $i@$(( $bytecount * 8000 / $linecount ))mbpc '/ ' > /dev/stderr
  echo $i $bytecount $seqname >> $compressionsizes
done

minsize=`sort -n -k 2 < $compressionsizes | head -n 1`
minmodbit=`echo $minsize | cut -d ' ' -f 1`
minbytecount=`echo $minsize | cut -d ' ' -f 2`
seqname=`echo $minsize | cut -d ' ' -f 3`

(echo ; echo $minmodbit modulus bits, hashes modulo $modulus) > /dev/stderr

cat $seqname

rm `cut -d ' ' -f 3 < $compressionsizes` $input $hashes $compressionsizes