def f = new File(args[0])

def rootWord = ""
def formedWords = [:]
def wordList = []

f.eachLine {
    if (!it.startsWith("\t")) {
        formedWords[rootWord] = wordList
        rootWord = it.split()[0].toLowerCase()
        wordList = []
    } else {
        wordList += it.split()[0].toLowerCase()
    }
}
formedWords[rootWord] = wordList   // last word family

formedWords.findAll{it.key != ''}.each { k, v ->
    println "$k ${v.join(' ')}"
}

println ""
