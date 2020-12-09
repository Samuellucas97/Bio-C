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
    | head rna == 'U' = ['T'] ++ reverseTranscricao (tail rna)
    | otherwise = [head rna] ++ reverseTranscricao (tail rna)


traducao :: String -> String
traducao [] = []
traducao [_,_] = []
traducao [_] = []
traducao rna
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UUU" = ['F'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UUC" = ['F'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UUA" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UUG" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CUU" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CUC" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CUA" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CUG" = ['L'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AUU" = ['I'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AUC" = ['I'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AUA" = ['I'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AUG" = ['M'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GUU" = ['V'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GUA" = ['V'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GUG" = ['V'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GUC" = ['V'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UCU" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UCC" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UCA" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UCG" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CCU" = ['P'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CCC" = ['P'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CCA" = ['P'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CCG" = ['P'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "ACU" = ['T'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "ACC" = ['T'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "ACA" = ['T'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "ACG" = ['T'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GCU" = ['A'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GCA" = ['A'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GCC" = ['A'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GCG" = ['A'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UAU" = ['Y'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UAC" = ['Y'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UAA" = ['*'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UAG" = ['*'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CAU" = ['H'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CAC" = ['H'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CAA" = ['Q'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CAG" = ['Q'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AAU" = ['N'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AAC" = ['N'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AAA" = ['K'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AAG" = ['K'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GAU" = ['D'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GAC" = ['D'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GAA" = ['E'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GAG" = ['E'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UGU" = ['C'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UGC" = ['C'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UGA" = ['*'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "UGG" = ['W'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CGU" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CGA" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CGG" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "CGC" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AGU" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AGC" = ['S'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AGA" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "AGG" = ['R'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GGU" = ['G'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GGA" = ['G'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GGG" = ['G'] ++ traducao (tail (tail(tail rna)))
            | ([(head rna)] ++ [(head (tail rna))] ++ [(head (tail(tail rna)))]) == "GGC" = ['G'] ++ traducao (tail (tail(tail rna)))
