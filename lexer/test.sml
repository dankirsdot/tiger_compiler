fun fileRead(filename) =
    let val file = TextIO.openIn filename
        val text = TextIO.input file
        val _ = TextIO.closeIn file
    in String.tokens (fn c => c = #"\n") text
    end
val test = fileRead "file.txt" 
