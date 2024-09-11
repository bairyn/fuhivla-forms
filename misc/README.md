Tables without the slinku'i test.

This may be more useful for investigating chains without the hypothetical CV
prefix used in the slinku'i test (see condition #3 in the fu'ivla definition
provided by CLL 4.7).

## Examples ignoring the slinku'i condition (condition #3).

The word classifications still hold when slinku'i is re-enabled.

### ‘brodauca’: fu'ivla

For example, ‘brodauca’ uniquely follows the parser state path #18 (by line)
(‘`ccvcvv -> `’) and then #4 (‘`cv *`’), making it a fu'ivla, since the `*`
indicates that without further text, it's a fu'ivla.

#### Slinku'i path: ‘brodauca’.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 6 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`vcv *`’): start, consume 3 characters, then 2, then 3.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 2 5 6 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’  ‘`vccvccvcv -> `’ ‘`v`’ ‘`vc`’ ‘`vcv *`’).)

Note the asterisk: as 6 is the final state, it's a fu'ivla.

### ‘brodaucau’: lujvo

But add ‘u’ at the end, and the path becomes 18 8 1 (‘`ccvcvv -> `’ ‘`cvv ->`’
‘’), reducing to the empty string, so ‘brodaucau’ is a lujvo.  It does not
include 1 of the 4 normal fu'ivla forms with the right prefix, nor does it end
in the fu'ivla tail form ‘cv’ with the right prefix.

#### Slinku'i path: ‘brodaucau’.

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 10 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`vcvv -> v`’): start, consume 3 characters, then 2, then 4.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 2 5 6 10 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’  ‘`vccvccvcv -> `’ ‘`v`’ ‘`vc`’ ‘`vcv *`’ ‘`vcvv -> v`’).)

### ‘brodacuca’: fu'ivla

‘brodacuca’ has path 18 12 (‘`ccvcvv -> `’ ‘`cvv ->`’ ‘`(cvcv)`’), so even if
there are more characters after ‘cuca’, no matter what else comes after it (we
can add anything valid that we want to add variants, like
‘brodacucafrobrodibordobroda’), we can stop right now and know the word is a
fu'ivla.

#### Slinku'i path: ‘brodacuca’.

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

With slinku'i enabled, then skipping non-final B parser states, the parser
state path is 1 28 26 28 20 14 (‘’ ‘`ccv -> vccvccv`’ ‘`vccvccvcv -> `’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`(vcvcv)`’): start, consume 3 characters, then 2, then 3, then 1, then 4.

(If the state between every character is counted, then the parser state path is
1 3 25 28 21 26 3 25 28 20 5 6 11 14 14 14 … 14 (‘’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvc`’ ‘`vccvccvcv -> `’ ‘`c`’ ‘`cc`’ ‘`ccv -> vccvccv`’ ‘`vccvccvv -> v`’ ‘`vc *`’ ‘`vcv *`’ ‘`vcvc`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ ‘`(vcvcv)`’ … ‘`(vcvcv)`’).
(Each character after in an X state preserves the parser state in 14.))
