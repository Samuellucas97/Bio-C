module Genetics where

complementoDNA :: String -> String
complementoDNA [] = []
complementoDNA dna 
    | head dna == 'A' = ['T'] ++ complementoDNA(tail dna)
    | head dna == 'T' = ['A'] ++ complementoDNA(tail dna)
    | head dna == 'G' = ['C'] ++ complementoDNA(tail dna)
    | head dna == 'C' = ['G'] ++ complementoDNA(tail dna)
    | otherwise = [head dna] ++ complementoDNA(tail dna)

complementoReversoDNA :: String -> String
complementoReversoDNA "" = ""
complementoReversoDNA dna = reverse(complementoDNA dna)

transcricao :: String -> String
transcricao [] = []
transcricao dna
    | head dna == 'T' = ['U'] ++ transcricao (tail dna)
    | otherwise = [head dna] ++ transcricao (tail dna)


complementoRNA :: String -> String
complementoRNA [] = []
complementoRNA dna 
    | head dna == 'A' = ['U'] ++ complementoRNA(tail dna)
    | head dna == 'U' = ['A'] ++ complementoRNA(tail dna)
    | head dna == 'G' = ['C'] ++ complementoRNA(tail dna)
    | head dna == 'C' = ['G'] ++ complementoRNA(tail dna)
    | otherwise = [head dna] ++ complementoRNA(tail dna)

complementoReversoRNA :: String -> String
complementoReversoRNA "" = ""
complementoReversoRNA dna = reverse(complementoRNA dna)


reverseTranscricao :: String -> String
reverseTranscricao [] = []
reverseTranscricao rna
    | head rna == 'U' = ['T'] ++ transcricao (tail rna)
    | otherwise = [head rna] ++ transcricao (tail rna)