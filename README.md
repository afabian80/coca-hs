# coca-hs
Coca analysis in Haskell

This is an experimental project to help learning English vocabulary in a systematic way. I study words from a "frequency list" consecutively, based on www.lextutor.ca.

This program processes any text file (e.g. an ebook, a movie subtitle) and convert it to a html file where some words are colorized based on COCA (CORPUS OF CONTEMPORARY AMERICAN English) vocabulary databases.
The COCA database consists of 25 text files, each one containing 1000 basewords and their different forms (e.g. past tense, superlative form, etc.) Baseword-1K means the most frequent 1000 English basedwords, 2K is the secons most frequent 1000 basewords and so on.

The program takes 3 parameters, the first parameter is the text file, the second paremeter (k-min) is the highest k-number that you know by heart, and the third parameter (k-max) is the highest k-number that you plan to study in the near future. Based on these 2 numbers, the input text is colorized so the every word you know (<= kmin) is not colored, every word between k-min and k-max is colored green, and every word above k-max is colored red. Furthermore, all words that are not found in the 25K database is colored gray. In this way it is easier to read the input text because you know that you are supposed to know every non-colored word by heart, and you can ignore everything that is either red or gray. On the other hand, you can focus on the green words.  
