// This file is imported by .github/workflows/slash-command-merge.yml
// and tested by ./koch testTools

function getSegments(regex, str) {
  // Returns the segments that `regex`
  // divides `str` into, along with the
  // info of wether each segment was matching
  // the regex or not
  let segments = [];
  let lastIndex = 0;
  let match;
  regex.lastIndex = 0; // In case there's a dangling previous search
  while (match = regex.exec(str)) {
    if (match.index > lastIndex) {
      segments.push([false, str.substring(lastIndex, match.index)]);
    }
    segments.push([true, match[0]]);
    lastIndex = match.index + match[0].length;
  }
  if (lastIndex < str.length) {
    segments.push([false, str.substring(lastIndex)]);
  }
  return segments;
}

exports.lineWrapPRdescription = (text, maxLineLength) =>
  getSegments(/(?:^ {0,3}(```|~~~)[^]*?^ {0,3}\1 *$)|^(?:\n {4,}.*)+/gm, text)
    .map( ([isCodeBlock, segment]) =>
      isCodeBlock
      ? segment // Don't touch code blocks
      : segment.split('\n').map(line =>
          line.length <= maxLineLength
          ? line // Line isn't too long, nothing to do
          : getSegments(/(`+).*?\1/g, line)
              // Treat codespans as single words:
              .flatMap( ([isCodeSpan, segment]) => isCodeSpan ? segment : segment.split(' ') )
              .reduce(([wrappedLine, lineLength], word) =>
                wrappedLine
                // Don't touch trailing spaces:
                ? word.length > 0 && lineLength + 1 + word.length > maxLineLength
                  ? [wrappedLine + '\n' + word, word.length]
                  : [wrappedLine + ' ' + word, lineLength + 1 + word.length]
                : [wrappedLine + word, lineLength + word.length]
              , ['', 0])[0]
        ).join('\n')
    ).join('')

// Tests after this line
function runTests() {
  let testingText = `Lorem ipsum dolor sit amet, consectetur i adipiscing elit. Nullam blandit mauris id venenatis tincidunt. Vestibulum at gravida sapien. Mauris tellus augue, aliquet sed laoreet blandit, pulvinar sed felis. Phasellus nec est vitae enim blandit facilisis.
Vestibulum fermentum ligula sit amet volutpat fermentum. Sed in faucibus orci. Pellentesque a dui ex. Curabitur sollicitudin, nulla id dignissim lacinia, odio mauris blandit nisi, eget auctor arcu odio nec est.

=========================================

## Summary
* The quick brown fox now jumps over the lazy dog


## Details
* THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG'S BACK 1234567890
* Hamburgevons


\`\`\`
this is a triple quote delimited codeblock:
  it should not be linewrapped as should no other style of codeblock be
    there are also some birds:
                                                               ___     ___
                                                              (o o)   (o o)
                                                             (  V  ) (  V  )
                                                            /--m-m-----m-m--/
\`\`\`
Here's some text.

    and this is an indented codeblock:
      with a cat:
        !"#%&'()*+,-./012  _._     _,-'""'-._      3456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefgh
        "#%&'()*+,-./0123 (,-.'._,'(       |\'-/|  456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghi
        #%&'()*+,-./01234     '-.-' \ )-'( \ o o)  56789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghij
        %&'()*+,-./012345           '-    \'\'-'_  6789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_abcdefghijk



https://www.some.url/that/is/quite/long/abcdefghijklmnopqrstuvw123467890xyz

(test) =========================================
(test) ======================================= i


   ~~~tokipona

	jan ali li kama lon nasin ni: ona li ken tawa li ken pali.
	jan ali li kama lon sama. jan ali li jo e ken pi pilin suli.
	jan ali li ken pali e wile pona ona.
	jan ali li jo e ken pi sona pona e ken pi pali pona.
	jan ali li wile pali nasin ni: ona li jan pona pi jan ante.

~~~

=================================== split this
================================= \`keep this\`

there are trailing spaces after this sentence      
and they must not leak into this line
`;

  let testingTextWrapped = `Lorem ipsum dolor sit amet, consectetur i
adipiscing elit. Nullam blandit mauris id
venenatis tincidunt. Vestibulum at
gravida sapien. Mauris tellus augue,
aliquet sed laoreet blandit, pulvinar sed
felis. Phasellus nec est vitae enim
blandit facilisis.
Vestibulum fermentum ligula sit amet
volutpat fermentum. Sed in faucibus orci.
Pellentesque a dui ex. Curabitur
sollicitudin, nulla id dignissim lacinia,
odio mauris blandit nisi, eget auctor
arcu odio nec est.

=========================================

## Summary
* The quick brown fox now jumps over the
lazy dog


## Details
* THE QUICK BROWN FOX JUMPED OVER THE
LAZY DOG'S BACK 1234567890
* Hamburgevons


\`\`\`
this is a triple quote delimited codeblock:
  it should not be linewrapped as should no other style of codeblock be
    there are also some birds:
                                                               ___     ___
                                                              (o o)   (o o)
                                                             (  V  ) (  V  )
                                                            /--m-m-----m-m--/
\`\`\`
Here's some text.

    and this is an indented codeblock:
      with a cat:
        !"#%&'()*+,-./012  _._     _,-'""'-._      3456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefgh
        "#%&'()*+,-./0123 (,-.'._,'(       |'-/|  456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghi
        #%&'()*+,-./01234     '-.-'  )-'(  o o)  56789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghij
        %&'()*+,-./012345           '-    ''-'_  6789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_abcdefghijk



https://www.some.url/that/is/quite/long/abcdefghijklmnopqrstuvw123467890xyz

(test)
=========================================
(test)
======================================= i


   ~~~tokipona

	jan ali li kama lon nasin ni: ona li ken tawa li ken pali.
	jan ali li kama lon sama. jan ali li jo e ken pi pilin suli.
	jan ali li ken pali e wile pona ona.
	jan ali li jo e ken pi sona pona e ken pi pali pona.
	jan ali li wile pali nasin ni: ona li jan pona pi jan ante.

~~~

=================================== split
this
================================= 
\`keep this\`

there are trailing spaces after this
sentence      
and they must not leak into this line
`;
  if (exports.lineWrapPRdescription(testingText, 41) != testingTextWrapped) {
    console.log("Test for PR description wrapping failed!");
    process.exit(1);
  } else {
    console.log("Test for PR description wrapping succeeded!");
  }
}

if (require.main === module) {
  runTests()
}

