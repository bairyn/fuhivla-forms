# Comprehensive fu'ivla form

CLL section 4.7 defines the morphology of fu'ivla. They are brivla that are not gismu or lujvo and that satisfy the other 4 criteria. This effectively means that fu'ivla consist of valid ‘y’-less brivla with either 1) a consonant/vowel cluster that cannot appear in lujvo (this is why stage 3 fu'ivla are necessarily fu'ivla: the 4th, 5th, and 6th letters are consonants, but (section 4.1) no valid _initial_ consonant pair starts with ‘r’, ‘n’, or ‘l’, and in lujvo consonant triples must have both pairs be valid _initial_ consonant pairs (i.e. what section CLL 4.1 denotes as ‘CC’ rather than ‘C/C’) (see CLL section 3.7 and also CLL section 4.7's 2 ‘triple’ sentences), but fu'ivla do not have this restriction), or 2) including, if hyphenization is ignored, the right cv form, with 1 of the 4 normal fu'ivla forms (v, ccc, ccvv, and cvcv), either at the beginning or with a prefix that has the right rafsi decomposition parser state (one that leaves no possibility of a valid rafsi decomposition, by following the reductions listed in the table), according to the table, or 3) ending in 1 of the 4 fu'ivla tail forms (c, cv, cc, ccvcc) (only ‘cv’ ends in ‘v’, so only it is conseqential for fu'ivla determination), if hyphenization is ignored.  (For cases #2 and #3, the table includes the slinku'i test (condition #3 in CLL 4.7).)

[table-simplified.txt](table-simplified.txt)

- `->` denotes a simplifying reduction, possibly to the empty string.
- `*` denotes more input is needed, or else there is no lujvo rafsi decomposition.
- `(…)` denotes a fu'ivla form: there exists no lujvo rafsi decomposition.

## Examples

### ‘brodauca’: fu'ivla

For example, ‘brodauca’ uniquely follows the parser state path #18 (by line)
(‘`ccvcvv -> `’) and then #4 (‘`cv *`’), making it a fu'ivla, since the `*`
indicates that without further text, it's a fu'ivla.

### ‘brodaucau’: lujvo

But add ‘u’ at the end, and the path becomes 18 8 1 (‘`ccvcvv -> `’ ‘`cvv ->`’
‘’), reducing to the empty string, so ‘brodaucau’ is a lujvo.  It does not
include 1 of the 4 normal fu'ivla forms with the right prefix, nor does it end
in the fu'ivla tail form ‘cv’ with the right prefix.

### ‘brodacuca’: fu'ivla

‘brodacuca’ has path 18 12 (‘`ccvcvv -> `’ ‘`cvv ->`’ ‘`(cvcv)`’), so even if
there are more characters after ‘cuca’, no matter what else comes after it (we
can add anything valid that we want to add variants, like
‘brodacucafrobrodibordobroda’), we can stop right now and know the word is a
fu'ivla.

### ‘brodacraubacadafagajakalamanaporosotovoxozo’: fu'ivla

Likewise ‘brodacraubacadafagajakalamanaporosotovoxozo’ has path 18 10 (‘`ccvcvv
-> `’ ‘`(ccvv)`’) followed by extra text, so no matter what comes after it
(once we parse ‘brodacrau’, we know it's a fu'ivla), as long as it's otherwise
a valid y-less brivla, it's a fu'ivla.

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
to that state, and look at the right-hand side of the annotated transition line
to see what that input character leads to.

Finally, after the slinku'i test is enabled, since a brivla that starts with a
vowel is necessarily morphologically separate from preceding words, we manually
exclude this line #2 case ad-hoc from the slinku'i test and revert it from ‘`v
-> `’ (L) back to its pre-slinku'i test ‘(v)’.  A valid brivla starting with a
vowel is a fu'ivla even if it would _otherwise_ (i.e. if you dropped the ‘`.`’
dot) fail the slinku'i test.

## Simplified table notes

With the slinku'i test enabled, the rafsi decomposition tables _includes_ a
hypothetical CV word before that we want to exclude, so for decompositions that
don't include a hypothetical prefix, the slinku'i-less table may be more
suitable.
