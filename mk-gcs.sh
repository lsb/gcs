#!/bin/sh
input=`mktemp`
hashes=`mktemp`
sequence=`mktemp`
gzip -c1 > $input
linecount=`gzip -cd $input | wc -l`

fp=100
binarybits=
modulus=$(( linecount * fp ))
indexfrequency=16384
while getopts "hp:b:m:i:" OPTION
do
    case $OPTION in
	h)
	    echo "usage: $0 -p false-positive-rate (default: 100) -b bits-for-binary-Rice-encoding (default: floor(log2(fp))) -m hash-modulus (default: linecount * fp) -i index-stride (default: 16k)"
	    echo "stdin: one key per line. stdout: golomb-compressed sequence. stderr: json with base64-encoded gcs and index."
	    exit 1
	    ;;
	p)
	    fp=$OPTARG
	    ;;
	b)
	    binarybits=$OPTARG
	    ;;
	m)
	    modulus=$OPTARG
	    ;;
	i)
	    indexfrequency=$OPTARG
	    ;;
    esac
done

if [ -z $binarybits ]
then
    binarybits=`awk "BEGIN { print int(log($fp)/log(2)) }"`
fi

#sort -n can't handle bigints
export LC_ALL=C
gzip -cd $input | ./make-hash-sequences $modulus | awk -v len=${#modulus} '{ printf("% " len "s\n",$1) }' | sort -S 50% -u | tr -d ' ' | gzip -c1 > $hashes

gzip -cd $hashes | ./golomb-encode $binarybits > $sequence

outputlinecount=`gzip -cd $hashes | wc -l`
(echo '{'
  echo '"lineCount":' ${outputlinecount},
  echo '"modulus":' ${modulus},
  echo '"binaryBits":' ${binarybits},
  echo -n '"partialSumBitcounts":' ; gzip -cd $hashes | ./make-index $binarybits $indexfrequency ; echo ,
  echo -n '"b64EncodedGolombCodedSequence": "' ; ./base-64-encode < $sequence ; echo '"'
echo '}') > /dev/stderr
cat $sequence
rm $input $hashes $sequence
