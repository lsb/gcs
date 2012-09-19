function golombFilterQueries(lineCount, modulus, binaryBits, golombCodedSequence, queries) { // queries :: [(String, kWin :: IO (), kFail :: IO ())]
    var queriesHashes = queries.map(function(s_kW_kF) { return [hashMod(modulus,s_kW_kF[0]), s_kW_kF[1], s_kW_kF[2]] });
    var sortedHashes = queriesHashes.sort(function(s_kW_kF1, s_kW_kF2) { return s_kW_kF1[0] - s_kW_kF2[0] });
    fastGolombDecodeIsectK(lineCount, binaryBits, golombCodedSequence, sortedHashes);
}

function hashMod(modulus, string) {
    return BigInteger.parse(hex_md5(string),16).remainder(BigInteger(modulus)).toJSValue();
    // we use JS integers (doubles) instead of big ints because even at 0.001% FP rate, you'd need well over a quadrillion values inserted into your sequence, by which point you'd probably need an index
}

// fastGolombDecode decodes lineCount bytes from the start of a base64-encoded golomb-coded sequence in the string bytes, fuses the iterative sum, and the set intersection with queries.
// the code is identical to the haskell original except that the tail recursion has been hand-compiled to an imperative loop, and the input is base64-encoded.
// (can you imagine starting off with half a dozen crazy different conditions, if you *didn't* have referential transparency?)
function fastGolombDecodeIsectK(lineCount, binaryBits, bytes, queries) {
    var alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    var mapping = {};
    alphabet.split(/(?=.)/).forEach(function(char,i) { mapping[char] = i });
    var sequenceAccumulator = 0;
    var unary = true, bytePtr = 0, bitIndex = 0, currentWord = 0, intBitsRemaining = 0, currentIntAccum = 0, intsRemaining = 0;
    bitIndex = -1;
    intsRemaining = lineCount;
    while (true) {
	if(!unary) {
	    if(intBitsRemaining == 0) {
		sequenceAccumulator += currentIntAccum;
		if(sequenceAccumulator > queries[0][0]) {
		    queries[0][2]();
		    queries.shift();
		} else {
		    while(sequenceAccumulator == queries[0][0]) {
			queries[0][1]();
			queries.shift();
		    }
		}
		if(queries.length == 0) return;
		unary = true;
		// bitIndex stays as is
		// currentWord stays as is
		currentIntAccum = 0;
		intsRemaining -= 1;
	    } else if(bitIndex == -1) {
		bitIndex = 5;
		currentWord = mapping[bytes[bytePtr]];
		// intBitsRemaining stays as is
		// currentIntAccum stays as is
		// intsRemaining stays as is
		bytePtr += 1;
	    } else {
		currentIntAccum = (currentIntAccum << 1) + ((currentWord >> bitIndex) & 1); //OoO because bitIndex changes
		bitIndex -= 1;
		// currentWord stays as is
		intBitsRemaining -= 1;
		// intsRemaining stays as is
	    }
	    continue;
	}
	// unary is true, because of the continue
	if(intsRemaining == 0) {
	    while(queries.length > 0) {
		queries[0][2](); queries.shift();
	    }
	    return;
	}
        if(bitIndex == -1) {
	    bitIndex = 5;
	    currentWord = mapping[bytes[bytePtr]];
	    // currentIntAccum stays as is
	    // intsRemaining stays as is
	    bytePtr += 1;
	} else if(currentWord & (1 << bitIndex)) {
	    bitIndex -= 1;
	    // currentWord stays as is
	    currentIntAccum += 1;
	    // intsRemaining stays as is
	} else {
	    unary = false;
	    bitIndex -= 1;
	    // currentWord stays as is
	    intBitsRemaining = binaryBits;
	    // currentIntAccum stays as is
	}
    }
}
