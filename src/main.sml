structure Main =
    struct
        structure parser = Parser

        fun main(arg0, argv) =
            let
                val _ = print (arg0 ^ "\n")
                val filename::rest = argv
                val _ = print (filename ^ "\n")
                val source = parser.fileRead filename
                val _ = map (fn s => print ("\t" ^ s ^ "\n")) source
            in
                OS.Process.success
            end
    end
