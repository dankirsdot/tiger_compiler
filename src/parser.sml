structure Parser =
struct
    fun is_separator #" " = true
      | is_separator #"\n" = true
      | is_separator #"\t" = true
      | is_separator _ = false

    fun fileRead(filename) =
        let
            val file = TextIO.openIn filename
            val text = TextIO.inputAll file
            val _ = TextIO.closeIn file
        in
            String.tokens is_separator text
        end
end
