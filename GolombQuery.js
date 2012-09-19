// fastGolombDecode decodes lineCount bytes from the start of a base64-encoded golomb-coded sequence in the string bytes.
// the code is identical to the haskell original except that the tail recursion has been hand-compiled to an imperative loop, and the input is base64-encoded.
// (can you imagine starting off with half a dozen crazy different conditions, if you *didn't* have referential transparency?)
function fastGolombDecode(lineCount, binaryBits, bytes) {
    var alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    var mapping = {};
    alphabet.split(/(?=.)/).forEach(function(char,i) { mapping[char] = i });
    var ints = new Array(lineCount);
    var unary = true, intPtr = 0, bytePtr = 0, bitIndex = 0, currentWord = 0, intBitsRemaining = 0, currentIntAccum = 0, intsRemaining = 0;
    bitIndex = -1;
    intsRemaining = lineCount;
    while (true) {
	// console.log(unary ? "unary" : "binary", "intPtr", intPtr, "bytePtr", bytePtr, "bitIndex", bitIndex, "currentWord", currentWord, "intBitsRemaining", intBitsRemaining, "currentIntAccum", currentIntAccum, "intsRemaining", intsRemaining);
	if(!unary) {
	    if(intBitsRemaining == 0) {
		ints[intPtr] = currentIntAccum;
		intPtr += 1;
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
	if(intsRemaining == 0) break;
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
    return ints;
}
