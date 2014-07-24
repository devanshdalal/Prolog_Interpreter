open Execute;;
exception Null_Query;;

let process s =                                   (* loads the program from the file s *)
  let cin = open_in s in
  let lexbuf = Lexing.from_channel cin in
  try
    progList := ([]:program) ;                    (* initializing the global variables. *)
    queryOn := false;
    let _ = Yacc.prog Lexer.token lexbuf in ()
  with
    | End_of_file -> if ( checkWf !progList ) then print_string("ok, well-formed.\n") else print_string("Not Well-Formed.\n")
    											  (* checking the program is well-formed or not. *)
;;

let handleQuery query =							  (* answers the queries *)
  queryList := [];                                (* initializing the global variables. *)
  queryOn := true;
  let lexbuf = Lexing.from_string query in
  try
    Yacc.query Lexer.token lexbuf
  with
    | (End_of_file ) -> ( 
                if ((List.length (!queryList)) = 0) then raise Null_Query
                else
                solveWithNot(!queryList)
        )
;;

let main () =                                   (* main function for prolog interpreter. *)
  while true do
      print_string "?- ";
      flush stderr;
      let inp = read_line () in
      if (inp=".") then exit 0
      else
        if (inp.[0] = '[') then
          let file = String.sub inp 1 ((String.length inp) - 3) in
          process file
        else
          handleQuery inp
  done

let _ = Printexc.print main ();;                (* calling the main function. *)
