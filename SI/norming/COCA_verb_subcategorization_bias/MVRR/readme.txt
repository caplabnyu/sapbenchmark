MVRR=0: with auxiliary verb (have or be)
MVRR=1: target ditransitive RR
MVRR=2: main verb
MVRR=3: main verb

ambigV=1: ambiguous; ambigV is 1 only when the target word is the first inflected word in the sentence
ambigV=0: not ambiguous (still might be ditransitive RR, but not an ambiguous one)

bias ratio:
ambiguous (MVRR=1 & ambigV=1)/(MVRR=1or2or3 & ambigV=1)
bias ratio:
general (MVRR=1 & ambigV=0or1)/(MVRR=1or2or3 & ambigV=0 or 1)

check:
MVRR=1 & ambigV =1 (many of them are not truly ambiugous; e.g., prep NP CV NP -> ditransitive RR but not ambiugous)