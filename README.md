[![Build Status](https://travis-ci.org/afabian80/coca-hs.svg?branch=master)](https://travis-ci.org/afabian80/coca-hs)

# coca-hs
Coca analysis in Haskell

This is an experimental project to help learning English vocabulary in a systematic way. I study words from a "frequency list" consecutively, based on www.lextutor.ca.

This program processes any text file (e.g. an ebook, a movie subtitle) and convert it to a html file where some words are colorized based on COCA (CORPUS OF CONTEMPORARY AMERICAN English) vocabulary databases.
The COCA database consists of 25 text files, each one containing 1000 basewords and their different forms (e.g. past tense, superlative form, etc.) Baseword-1K means the most frequent 1000 English basedwords, 2K is the seconds most frequent 1000 basewords and so on.

The program takes 4 parameters, the first parameter is the text or html file; the second parameter is either Html or Text; the third parameter (k-min) is the highest k-number that you know by heart, and the fourth parameter (k-max) is the highest k-number that you plan to study in the future. Based on the 2 k-numbers, the input text is colorized so the every word you know (<= kmin) is not colored, every word between k-min and k-max is colored green, and every word above k-max is colored red. Furthermore, all words that are not found in the 25K database are colored gray. In this way it is easier to read the input text because you know that you are supposed to know every non-colored word by heart, and you can ignore everything that is either red or gray. On the other hand, you can focus on the green words. The program also produces 2 text files as output, one containing the green words and another containing the red words.

Example usage: I know the first 4000 words from the COCA DB and I'd like to know 8000 in the near future. I would like to colorize it as a html file. Use the command:

`runhaskell colorize.hs html-samples/TheSecretGarden.html Html 4 8`. Then open index.hml in the current directory. For original links in the html file, it is better replace the original html file with the generated index.html.

(Note: you need to be able to run Haskell on your machine!)
