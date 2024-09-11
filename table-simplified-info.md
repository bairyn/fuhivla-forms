# Comprehensive fu'ivla form

CLL section 4.7 defines the morphology of fu'ivla. They are brivla that are not gismu or lujvo and that satisfy the other 4 criteria. This effectively means that fu'ivla consist of valid ‘y’-less brivla with either 1) a consonant/vowel cluster that cannot appear in lujvo (this is why stage 3 fu'ivla are necessarily fu'ivla: the 4th, 5th, and 6th letters are consonants, but (section 4.1) no valid _initial_ consonant pair starts with ‘r’, ‘n’, or ‘l’, and in lujvo consonant triples must have both pairs be valid _initial_ consonant pairs (i.e. what section CLL 4.1 denotes as ‘CC’ rather than ‘C/C’) (see CLL section 3.7 and also CLL section 4.7's 2 ‘triple’ sentences), but fu'ivla do not have this restriction), or 2) including, if hyphenization is ignored, the right cv form, with 1 of the 4 normal fu'ivla forms (v, ccc, ccvv, and cvcv), either at the beginning or with a prefix that has the right rafsi decomposition parser state (one that leaves no possibility of a valid rafsi decomposition, by following the reductions listed in the table), according to the table, or 3) ending in 1 of the 4 fu'ivla tail forms (c, cv, cc, ccvcc) (only ‘cv’ ends in ‘v’, so only it is conseqential for fu'ivla determination), if hyphenization is ignored.  (For cases #2 and #3, the table includes the slinku'i test (condition #3 in CLL 4.7).)

[table-simplified.txt](table-simplified.txt)

- `->` denotes a simplifying reduction, possibly to the empty string.
- `*` denotes more input is needed, or else there is no lujvo rafsi decomposition.
- `(…)` denotes a fu'ivla form: there exists no lujvo rafsi decomposition.

## Examples.

### ‘brodauca’: fu'ivla

‘brodacau’ uniquely follows a given parser state path, which ends up making it
a fu'ivla, since the `*` in state #6 that the parser ends in indicates that
without further text, it's a fu'ivla.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 6 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`vcv *`’): start, consume 3 characters, then 2, then 3.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 2 5 6 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’  ‘`vccvccvcv -> `’ ‘`v`’ ‘`vc`’ ‘`vcv *`’).)

Note the asterisk: as 6 is the final state, it's a fu'ivla.

### ‘brodaucau’: lujvo

But add ‘u’ at the end, and the parser after state #6 (on line #6 in the simple
table) ends up in state #10, indicating there exists a lujvo rafsi
decomposition.  State #10 is a normal parser state, not one with ‘`*`’ or
parens.  Thus ‘brodaucau’ does not include 1 of the 4 normal fu'ivla forms with
the right prefix, nor does it end in the fu'ivla tail form ‘cv’ with the right
prefix.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 10 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`vcvv -> v`’): start, consume 3 characters, then 2, then 4.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 2 5 6 10 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’  ‘`vccvccvcv -> `’ ‘`v`’ ‘`vc`’ ‘`vcv *`’ ‘`vcvv -> v`’).)

### ‘brodacuca’: fu'ivla

‘brodacuca’ when parsed ends in state #14, corresponding to 1 of the 4 normal
fu'ivla forms (‘cvcv’).  Even if there are more characters after ‘cuca’, no
matter what else comes after it provided it's still a valid y-less brivla, it's
still a fu'ivla (for example, we could add an arbitrary suffix making a word
like ‘brodacucafrobrodibordobroda’, and we know it's still a fu'ivla).  This
word has 1 of the 4 normal fu'ivla forms with the right prefix, so it's a
fu'ivla.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 24 14 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`cv -> vcv`’ ‘`(vcvcv)`’): start, consume 3 characters, then 2, then 2, then 2.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 3 24 11 14 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’  ‘`vccvccvcv -> `’ ‘`c`’ ‘`cv -> vcv`’ ‘`vcvc`’ ‘`(vcvcv)`’).)

### ‘brodacraubacadafagajakalamanaporosotovoxozo’: fu'ivla

Likewise ‘brodacraubacadafagajakalamanaporosotovoxozo’ has path 18 10 (‘`ccvcvv
-> `’ ‘`(ccvv)`’) followed by extra text, so no matter what comes after it
(once we parse ‘brodacrau’, we know it's a fu'ivla), as long as it's otherwise
a valid y-less brivla, it's a fu'ivla.

#### Slinku'i path: ‘brodacraubacadafagajakalamanaporosotovoxozo’.

Likewise ‘brodacraubacadafagajakalamanaporosotovoxozo’ includes 1 of 4 the 4
normal fu'ivla forms (‘cvcv’) with the right prefix followed by extra text, so no
matter what comes after it, as long as it's otherwise a valid y-less brival,
it's a fu'ivla; once we parse ‘brodacrau’, we know it's a fu'ivla.  It ends in
state (line) #TODO.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 28 20 14 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`(vcvcv)`’): start, consume 3 characters, then 2, then 3, then 1, then 4.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 3 25 28 20 5 6 11 14 14 14 … 14 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’ ‘`vccvccvcv -> `’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`vc *`’ ‘`vcv *`’ ‘`vcvc`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ … ‘`(vcvcv)`’).
(Each character after in an X state preserves the parser state in 14.))

The initial subtext that puts us in a final fu'ivla parser state with 1 of the
4 normal fu'ivla forms (‘cvcv’) is ‘brodacraubaca’

# Other notes

## Simplified table creation

The simplified table can be derived from the annotated table which is output by
Main.hs after ignoring the first three lines and manually adding an empty first
line (e.g. line 5 in simplified corresponds to line 7 in annotated (3-1=+2))
and simplifying L (as in ‘loopback’) as reductions/transitions to the
corresponding chain state earlier in the file, B (and in ‘both’) as new states
(no parens) that can if ended decompose into rafsi tokens, X as being
impossible to decompose into lujvo rafsi (denote with parens), and O as an
asterisk (can decompose if the right text is added but cannot if input ends
there (as in ‘$’)).  The first line afterwards is like a B, which I leave as an
empty line.  For the cv pattern word itself, trace the input characters leading
to that state, and look at the left side of the annotated transition line to
see what that the state is combined with an appended input character.  ‘^’
denotes the initial parser state.

The generation of this table is now automated with the ‘--simple’ flag.

## Simplified table notes

With the slinku'i test enabled, the rafsi decomposition tables _includes_ a
hypothetical CV word before that we want to exclude, so for decompositions that
don't include a hypothetical prefix, the slinku'i-less table may be more
suitable.
