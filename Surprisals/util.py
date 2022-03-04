import re

def clean(token):
    return re.sub("[^a-zA-Z0-9*.,!?]", "", token) # filter out non-alphanumeric or punctuation characters

def align(words, wordpieces, debug=False):
    # Remove the "not beginning of sentence" character from the wordpieces
    wordpieces = [clean(piece) for piece in wordpieces]

    aligned = [] # list containing lists of wordpieces that make up each word
    idx_word = 0 # idx of the next word
    current_pieces = [] # wordpieces that don't align with the next word

    for idx_piece, piece in enumerate(wordpieces):
        if idx_word < len(words):
            if debug: print("not EOS")
            word = words[idx_word]
        else:
            current_pieces += wordpieces[idx_piece:]
            break

        if debug: print(piece, word, piece == word[:len(piece)])

        if piece == word[:len(piece)]:
            # if the new wordpiece is aligned to the next word

            # all current pieces belong to the current word
            aligned.append(current_pieces)

            # and the new piece belongs to the next word
            idx_word += 1
            current_pieces = [piece]
        else:
            # otherwise, the new piece belongs to the current word too
            current_pieces.append(piece)

    # at EOS, all remaining wordpieces belong to the last word
    aligned.append(current_pieces)
    if debug: print("EOS, merging the rest in: " + ",".join(current_pieces))

    # First entry in aligned is always empty (first wordpiece should always match the first word)
    aligned = aligned[1:]

    # get the indices of the wordpiece that correspond to word boundaries (breaks)
    breaks = [len(pieces) for pieces in aligned]
    breaks = [0] + [sum(breaks[:i+1]) for i in range(len(breaks))]

    return aligned, breaks


# Tests 
if __name__ == "__main__":
    test_1 = "The dog is in the boat ...".split()
    test_2 = ["T", "he", "d", "og", "is", "in", "th", "e", "bo", "at", "..." ]
    print(test_1)
    a, b = align(test_1, test_2, debug=True)
    print(a, b)
    a_recon = [test_2[b[i]:b[i+1]] for i in range(len(a))]
    print(a_recon)
    assert a == a_recon
