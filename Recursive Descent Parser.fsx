(* COSC455-101
Assignment: Program 2
Recursive Descent Parser in F#
Name: Phan, Anh 
Name: Rozycki, Damian *)


(* BNF FOR THE PARSER:
<program> ::= <stmt_list> $$
<stmt_list> ::= <stmt> <stmt_list> | ε
<stmt> ::= id <id_tail> | <read_stmt> | <write_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
<id_tail> ::= <fun_call> | <assignment>
<expr> ::= id <expr_tail> | ( <expr> )
<expr_tail> ::= <arith_op> <expr> | ε
<arith_op> ::= - | + | * | /
<rel_oper> ::= > | < | ==
<cond> ::= <expr> <rel_oper> <expr>
<assignment> ::= = <expr>
<read_stmt> ::= read id
<write_stmt> ::= write <expr>
<if_stmt> ::= if <cond> then <stmt_list> <else_stmt>
<else_stmt> ::= else <stmt_list> endif | endif
<fun_call> ::= <- id ( <param_list> )
<param_list> ::= <expr> <param_tail>
<param_tail> ::= , <expr> <param_tail> | ε
<while_stmt> ::= while <cond> do <stmt_list> done
<do_stmt> ::= do <stmt_list> until <cond>
id ::= <any lexeme/token (numbers or letters) not already expressed as a terminal above> 
*)


// Token Type  (Same as the "Lexer" enum in the Java version)
// !!! Remember "tokens" are "terminals"  ***NOT*** "productions"
// Terminals are represented by Tokens/Types, productions are represented by functions.
type Token =
    | L_PAREN // (
    | R_PAREN // )
    | ARITH_OP // - | + | * | /
    | REL_OP // > | < | ==
    | ASSIGN_OP // =
    | FUNC_OP // <-
    | COMMA // ,
    | READ
    | WRITE
    | IF
    | THEN
    | ELSE
    | ENDIF 
    | WHILE
    | DO 
    | DONE 
    | UNTIL 
    | ID of string // any lexeme/token (numbers or letters) not already expressed as a terminal above
     

    // Member (of the type) function to get a token from a lexeme (String)
    static member tokenFromLexeme str =
        match str with
            | "(" -> L_PAREN
            | ")" -> R_PAREN
            | "-" | "+" | "*" | "/" -> ARITH_OP 
            | "<" | ">" | "==" -> REL_OP 
            | "=" -> ASSIGN_OP  
            | "<-" -> FUNC_OP
            | "," -> COMMA
            | "read" -> READ 
            | "write" -> WRITE
            | "if" -> IF
            | "then" -> THEN
            | "else" -> ELSE
            | "endif" -> ENDIF
            | "while" -> WHILE
            | "do" -> DO
            | "done" -> DONE
            | "until" -> UNTIL
            | x -> ID x


let matchToken (theExpectedToken: Token) theList =
    match theList with
    // resolve to the rest of the list when head is the expected type.
    | head :: tail when head = theExpectedToken -> tail

    // head of list did not match the expected type, so we don't even care about "the rest" (_)
    | head :: _ -> failwithf $"Wrong Type! Expected %A{theExpectedToken} but found %A{head}. Unprocessed Tokens: %A{theList.Tail}"

    // Couldn't match anything!
    | _ -> failwithf $"Nothing to match! Expected %A{theExpectedToken} but found nothing."


// NOTE: The |> operator sends (pipes) the output of one function directly to the next one in line.
// "and" just allows multiple, mutually recursive functions to be defined under a single "let"


let rec parse (theList: Token list) = theList |> program


// <program> ::= <stmt_list> $$
and program lst = lst |> stmt_list |> ``$$``


and ``$$`` =
    function
    | [] -> printfn "Done!"; ([] : Token list)
    | remainingTokens -> failwithf $"Invalid input! Cannot start with {remainingTokens.Head}! Unprocessed Tokens: {remainingTokens.Tail}"


// <stmt_list> ::= <stmt> <stmt_list> | ε
and stmt_list =
    function
    | (ID _ | READ | WRITE | IF | DO | WHILE) :: _ as lst -> lst |> stmt |> stmt_list
    | x -> x // ε is permitted, so just resolve to what was passed if no other match.


// <stmt> ::= id <id_tail> | <read_stmt> | <write_stmt> | <if_stmt> | <do_stmt> | <while_stmt>
and stmt lst =  
    match lst with
    | ID _ :: xs -> xs |> id_tail 
    | READ :: _ as lst -> lst |> read_stmt
    | WRITE :: _ as lst -> lst |> write_stmt
    | IF :: _ as lst -> lst |> if_stmt
    | DO :: _ as lst -> lst |> do_stmt
    | WHILE :: _ as lst -> lst |> while_stmt
    | _ -> failwith $"Expected an id, 'read', 'write', 'if', 'do', or 'while', but found {lst.Head}. Unprocessed tokens: {lst.Tail}"

// <id_tail> ::= <fun_call> | <assignment>
and id_tail lst = 
    match lst with
    | FUNC_OP :: _ as lst -> lst |> fun_call
    | ASSIGN_OP :: _ as lst -> lst |> assignment
    | [] -> failwith "Unexpected end of input! Expected '<-' or '=', but found nothing." // no empty case allowed
    | _ -> failwithf $"Expected '<-' or '=', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <expr> ::= id <expr_tail> | ( <expr> )
and expr lst =
    match lst with
    | ID _ :: xs -> xs |> expr_tail
    | L_PAREN :: xs -> xs |> expr |> matchToken R_PAREN
    | [] -> failwith "Unexpected end of input! Expected an id or '(', but found nothing." // no empty case allowed
    | _ -> failwithf $"Expected an id or '(', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <expr_tail> ::= <arith_op> <expr> | ε
and expr_tail =
    function
    | ARITH_OP :: xs -> xs |> expr
    | x -> x  // ε case, so just return the list unchanged.


// <cond> ::= <expr> <rel_oper> <expr>
and cond lst = lst |> expr |> matchToken REL_OP |> expr


// <assignment> ::= = <expr>
and assignment lst = 
    match lst with 
    | ASSIGN_OP :: xs -> xs |> expr
    | _ -> failwithf $"Expected '=', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <read_stmt> ::= read id
and read_stmt lst = 
    match lst with
    | READ :: ID _ :: xs -> xs
    | READ :: [] -> failwith $"Unexpected end of input! Expected an id after 'read', but found nothing" // no empty id allowed
    | READ :: xs -> failwith $"Expected an id after 'read', but found {xs.Head}. Unprocessed tokens: {xs.Tail}" // no empty id allowed
    | _ -> failwith $"Expected 'read', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" 


// <write_stmt> ::= write <expr>
and write_stmt lst = 
    match lst with
    | WRITE :: xs -> xs |> expr
    | _ -> failwith $"Expected 'write', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <if_stmt> ::= if <cond> then <stmt_list> <else_stmt>
and if_stmt lst = 
    match lst with
    | IF :: xs -> xs |> cond |> matchToken THEN |> stmt_list |> else_stmt
    | _ -> failwith $"Expected 'if', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <else_stmt> ::= else <stmt_list> endif | endif
and else_stmt lst = 
    match lst with
    | ELSE :: xs -> xs |> stmt_list |> matchToken ENDIF
    | ENDIF :: xs -> xs
    | [] -> failwith "Unexpected end of input! Expected 'else' or 'endif', but found nothing." // no empty case allowed
    | _ -> failwith $"Expected 'else' or 'endif', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed

// <fun_call> ::= <- id ( <param_list> )
and fun_call lst = 
    match lst with
    | FUNC_OP :: ID _ :: L_PAREN :: xs -> xs |> param_list |> matchToken R_PAREN
    | FUNC_OP :: xs -> failwith $"Expected an id followed by '(' after '<-', but found {xs}" 
    | _ -> failwith $"Expected '<-', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <param_list> ::= <expr> <param_tail>
and param_list lst = lst |> expr |> param_tail


// <param_tail> ::= , <expr> <param_tail> | ε
and param_tail = 
    function
    | COMMA :: xs -> xs |>  expr |> param_tail
    | x -> x  // ε case, so just return the list unchanged.


// <while_stmt> ::= while <cond> do <stmt_list> done
and while_stmt lst = 
    match lst with
    | WHILE :: xs -> xs |> cond |> matchToken DO |> stmt_list |> matchToken DONE
    | _ -> failwith $"Expected 'while', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


// <do_stmt> ::= do <stmt_list> until <cond>
and do_stmt lst = 
    match lst with
    | DO :: xs -> xs |> stmt_list |> matchToken UNTIL |> cond
    | _ -> failwith $"Expected 'do', but found {lst.Head}. Unprocessed tokens: {lst.Tail}" // no empty case allowed


(* **********************************************************************************************
   YOU MAY LEAVE THE FOLLOWING CODE AS IS.  IT IS NOT NECESSARY TO MODIFY IT FOR THIS ASSIGNMENT.
   *********************************************************************************************** *)

(* Get the user input and start parsing *)
open System.Text.RegularExpressions

// NOTE: To make the let assignment be a function that accepts no parameters,
// an "empty tuple" must be accepted in ML/SML/OCaml/F#.
let main () =

    // Convert a list of stings to Tokens:
    //    Split the String (which creates an Array)
    //             -> convert the Array to a List
    //             -> MAP the list of strings into a list of Tokens.
    //
    // (Note, though arrays are a lot like lists, lists are a bit easier to use for the pattern matching.)
    
    // 'mapTokens' is mainly it's own function as an example of the ">>" operator.
    // This just means that the mapTokens function is a combination of the convert
    // to list function and the Map to list function. (No parameters are specified!)
    let mapTokens = Array.toList >> List.map Token.tokenFromLexeme  

    // This is very ".NET" specific. Split is part of the .NET API.        
    let getTokenList (str: string) = 
        Regex.Split(str.Trim(), "\\s+")
        |> Array.filter (fun s -> s <> "") // This filters out any empty strings resulting from the split.
        |> mapTokens

    (* Begin Parsing Process *)
    let startParsing str =
        // Display our list of tokens...
        printfn $"\nInitial String: %s{str}"

        // Try to parse the list of tokens and display the results.
        try
            let tokenList = getTokenList str
            printfn $"Tokens Before Parsing: %A{tokenList}"
            let parsedList = parse tokenList


            // Check if there are any remaining tokens after parsing
            if parsedList.Length = 0 then
                // Successful parsing message
                printfn $"The Sentence \"%s{str}\" follows the grammar."


        // If an exception ("failwith") is thrown, display the error message.
        with Failure msg -> printfn $"The Sentence \"%s{str}\" is incorrect because: \nError: %s{msg}"

    // Get the user input and start parsing
    let getUserInput () =
        printf "Enter the program here (all on one line, lowercase, space-delimited)\n=> "
        
        // A case where it's easier to use the .NET ReadLine as opposed to the more restrictive OCaml native variant.        
        System.Console.ReadLine()

    in
    // Get the user input and start parsing
    getUserInput () |>  startParsing |> ignore  // Just ignore the result, as we are just printing results above.

// Execute the main function!
main ()

