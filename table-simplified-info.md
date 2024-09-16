# Comprehensive fu'ivla forms

## fu'ivla-forms: comprehensive fu'ivla forms

Lojban ‘content words’ brivla have 3 forms: gismu, lujvo, and fu'ivla.  This
project shows the comprehensive form of fu'ivla, which are the most flexible in
terms of morphological structure and lettering.  (gismu are simple CVC/CV or
CCVCV form brivla, and they are root words for lujvo, which are structured as
chains of other words or rafsi.  Rafsi are the variants of gismu that can
include shorter affix variants.)

CLL section 4.7 defines the morphology of fu'ivla. They are brivla that are not
gismu or lujvo and that satisfy the other 4 of the 5 CLL conditions. This
effectively means that fu'ivla consist of valid ‘y’-less brivla with either 1)
a consonant/vowel cluster that cannot appear in lujvo (this is why stage 3
fu'ivla are necessarily fu'ivla: the 4th, 5th, and 6th (+1 if the rafsi isn't
the CVC variant) letters are consonants, but (section 4.1) no valid _initial_
consonant pair starts with ‘r’, ‘n’, or ‘l’, and in lujvo consonant triples
must have both pairs be valid _initial_ consonant pairs (i.e. what section CLL
4.1 denotes as ‘CC’ rather than ‘C/C’) (see CLL section 3.7 and also CLL
section 4.7's 2 ‘triple’ sentences), but fu'ivla do not have this restriction),
or 2) starts with a vowel (‘.v’) that necessarily morphologically separates it
from anything preceding it, or 3) including, if hyphenization is ignored, the
right cv form, with 1 of the 4 normal fu'ivla forms (v, ccc, ccvv, and cvcv),
either at the beginning or with a prefix that has the right rafsi decomposition
parser state (one that leaves no possibility of a valid rafsi decomposition, by
following the reductions listed in the table), according to the table, or 4)
ending in 1 of the 4 fu'ivla tail forms (c, cv, cc, ccvcc) (only ‘cv’ ends in
‘v’, so only it is conseqential for fu'ivla determination), if hyphenization is
ignored.  (For cases #3 and #4, the table includes the slinku'i test (condition
#3 in CLL 4.7).)

[table-simplified.txt](table-simplified.txt)

## Comprehensive fu'ivla form conditions

If a word is a valid brivla without ‘y’, it is a fu'ivla iff at least one of
the following comprehensive fu'ivla conditions are true:

- 1) It includes a consonant/vowel cluster that cannot appear in lujvo.  Stage
  3 fu'ivla have its 5th, 6th, and 7th\* letters as consonants, forming a
  consonant triple, but in lujvo consonant triples have to end with a valid
  _initial_ consonant pair (CLL 3.7).  No initial consonant pair starts with
  ‘r’, ‘n’, or ‘l’, and stage 3 fu'ivla use one of these in the middle letter.
  Thus stage 3 fu'ivla always satisfy this condition.  \*: If the rafsi
  attached to the stage 3 fu'ivla is a shorter CVC rafsi, the same principle
  applies; just consider the 4th, 5th, and 6th letters instead.  (See also the
  distinction between CC and C/C in CLL 4.1, and also notes about consonant
  triples in CLL 4.7.)
- 2) It starts with a vowel.
- 3) It includes 1 of the 4 normal fu'ivla forms, with the right prefix from
     the beginning of the word, according to the [comprehensive fu'ivla parser
     state table](table-simplified.txt), ignoring CVV-rn hyphenization\*:
	- 1) ‘v’
	- 2) ‘ccc’
	- 3) ‘ccvv’
	- 4) ‘cvcv’
     \*: CLL 4.5 specifies lujvo non-y hyphenization with ‘r’ or, if invalid,
     ‘n’, at the beginning of the word after a CVV rafsi.  To handle this case,
     if the first 4 letters have the form ‘cvvrcv’ (where letter 5 is not ‘n’)
     or ‘cvvnrv’, remove the 4th letter, and then try decomposing into rafsi
     according to the table.  No rafsi that starts with a hyphen letter ‘r’,
     ‘n’, or ‘l’, has a consonant for its second letter, so you do not need to
     consider the original.
- 4) It ends in the fu'ivla tail form ‘cv’ with the right prefix from the
     beginning of the word, according to the [comprehensive fu'ivla parser
     state table](table-simplified.txt) (there are actually 3 others, but they
     would only end cmevla), ignoring CVV-rn hyphenization at the beginning of
     the word as in comprehensive condition #3.

## Comprehensive fu'ivla parser state table

[Comprehensive fu'ivla parser state table](table-simplified.txt):

```

v
c
(vv)
vc *
vcv *
vcc *
vccv
(vccc)
vcvv -> v
vcvc
(vccvv)
vccvc
(vcvcv)
vcvcc
vccvcv -> 
vccvcc *
vccvccv
vccvccc -> vcc
vccvccvv -> v
vccvccvc
vcvccv -> 
vcvccc -> vccvcc
cv -> vcv
cc
vccvccvcv -> 
vccvccvcc -> cc
ccv -> vccvccv
ccc -> vccvcc
```

- `->` denotes a simplifying reduction, possibly to the empty string.
- `*` denotes more input is needed, or else there is no lujvo rafsi decomposition.
- `(…)` denotes a fu'ivla form: there exists no lujvo rafsi decomposition.

## Examples

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

Likewise ‘brodacraubacadafagajakalamanaporosotovoxozo’ includes 1 of 4 the 4
normal fu'ivla forms (‘cvcv’) with the right prefix followed by extra text, so
no matter what comes after it, as long as it's otherwise a valid y-less brival,
it's a fu'ivla; once we parse ‘brodacraubaca’, we know it's a fu'ivla.  It ends
in state (line) #14.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 28 20 14 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`(vcvcv)`’): start, consume 3 characters, then 2, then 3, then 1, then 4.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 3 25 28 20 5 6 11 14 14 14 … 14 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’ ‘`vccvccvcv -> `’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`vc *`’ ‘`vcv *`’ ‘`vcvc`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ … ‘`(vcvcv)`’).
(Each character after in an X state preserves the parser state in 14.))

The initial subtext that puts us in a final fu'ivla parser state with 1 of the
4 normal fu'ivla forms (‘cvcv’) is ‘brodacraubaca’

## Other notes

### Simplified table creation

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

### Simplified table notes

With the slinku'i test enabled, the rafsi decomposition tables _includes_ a
hypothetical CV word before that we want to exclude, so for decompositions that
don't include a hypothetical prefix, the slinku'i-less table may be more
suitable.

### ilmentufa bugs

ilmentufa is based on camxes, and the LLG ilmentufa Lojban parser fails to
parse dipthongs in the expanded dipthong table in CLL 3.4 besides the first 4.
However, CLL 4.7 specifies the expanded table, except for the last 2
cmevla-only dipthongs, are permissible in fu'ivla, and even provides an example
of a fu'ivla that uses the expanded dipthong table, which that parser fails to
parse.  Thus ilmentufa fails to parse the valid fu'ivla {baufklia}, but it does
parse {baufklai}.  The expanded dipthong table is useful in providing greater
flexibility for fu'ivla.
