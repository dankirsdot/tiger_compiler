structure Main =
    struct
        exception FatalError of string

        structure parser = Parser

        fun main(prog, args) =
            let
                exception Args

                fun usage() = print ("Usage: " ^ prog ^ " [-h] [-f filename]\n")

                fun parse filename =
                    let
                        val source = parser.fileRead filename
                    in
                        (* print filename; *)
                        map (fn s => print (s ^ "\n")) source
                    end

                (* nil is just a way to write [] *)
                fun parseArgs nil = ()
                  | parseArgs ("-h" :: ts) = (usage(); parseArgs ts)
                  | parseArgs ("-f" :: filename :: ts) = (parse filename; parseArgs ts)
                  | parseArgs _ = (usage(); raise Args)
            in
                parseArgs args handle Args => raise FatalError "Error parsing args. Use the -h option.";
                OS.Process.exit OS.Process.success
            end

        handle FatalError e => (print ("FatalError:\n" ^ e ^ "\n"); OS.Process.exit OS.Process.failure)
    end
