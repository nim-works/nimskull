// This file is imported by .github/workflows/slash-command-merge.yml
// and tested by tests/tools/tci_forrmat_pr_description.nim

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
    : segment
      // Remove HTML comments first:
      .replace(/<\!--.*?-->/s, "")
      .split('\n').map(line =>
        line.length <= maxLineLength
        ? line // Line isn't too long, nothing to do
        : getSegments(/(`+).*?\1/g, line)
          // Treat codespans as single words:
          .flatMap( ([isCodeSpan, segment]) => isCodeSpan ? segment : segment.split(' ') )
          .reduce( ([wrappedLine, lineLength, isStart], word) =>
            // Don't touch indentation nor start with a line break:
            isStart
            ? [wrappedLine + word, lineLength + word.length, false]
            : // Don't touch trailing spaces:
              word.length > 0 && lineLength + 1 + word.length > maxLineLength
              ? [wrappedLine + '\n' + word, word.length, false]
              : [wrappedLine + ' ' + word, lineLength + 1 + word.length, false]
            , ['', 0, true]
          )[0]
      ).join('\n')
  ).join('')
