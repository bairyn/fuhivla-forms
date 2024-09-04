# Comprehensive fu'ivla form

CLL section 4.7 defines the morphology of fu'ivla. They are brivla that are not gismu or lujvo and that satisfy the other 4 criteria. This effectively means that fu'ivla consist of valid ‘y’-less brivla with either 1) a consonant/vowel cluster that cannot appear in lujvo (this is why stage 3 fu'ivla are necessarily fu'ivla: the 4th, 5th, and 6th letters are consonants, but (section 4.1) no valid _initial_ consonant pair starts with ‘r’, ‘n’, or ‘l’, and in lujvo consonant triples must have both pairs be valid _initial_ consonant pairs (i.e. what section CLL 4.1 denotes as ‘CC’ rather than ‘C/C’) (see CLL section 3.7 and also CLL section 4.7's 2 ‘triple’ sentences), but fu'ivla do not have this restriction), or 2) including, if hyphenization is ignored, the right cv form, with 1 of the 4 normal fu'ivla forms (v, ccc, ccvv, and cvcv), either at the beginning or with a prefix that has the right rafsi decomposition parser state (one that leaves no possibility of a valid rafsi decomposition, by following the reductions listed in the table), according to the table, or 3) ending in 1 of the 4 fu'ivla tail forms (c, cv, cc, ccvcc) (only ‘cv’ ends in ‘v’, so only it is conseqential for fu'ivla determination), if hyphenization is ignored.

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
a valid y-less brival, it's a fu'ivla.
