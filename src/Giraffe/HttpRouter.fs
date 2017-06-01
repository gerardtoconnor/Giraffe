module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic

type HttpHandler = HttpHandler

//between inclusive
let inline intIn x l u = (x - l) * (u - x) >= 0

let inline int64In x l u = (x - l) * (u - x) >= 0L

let inline floatIn x l u = (x - l) * (u - x) >= 0.

type RouteState(path:string) =
    member val path = path with get
    member val pos = 0 with get , set

/// Private Range Parsers that quickly try parse over matched range (all fpos checked before running)

let private stringParse (path:string) ipos fpos = path.Substring(ipos,fpos - ipos) |> box |> Some

let private  charParse (path:string) ipos fpos = path.[ipos] |> box |> Some // this is not ideal method (but uncommonly used)

let private boolParse (path:string) ipos fpos =
    if intIn (fpos - ipos) 4 5 then 
        match path.[ipos] with
        | 't' | 'T' -> true  |> box |> Some // todo: Laxy matching, i'll complete later
        | 'f' | 'F' -> false |> box |> Some
        | _ -> None
    else None

let private intParse (path:string) ipos fpos =

    let mutable result = 0
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int path.[pos] - int '0'
        if intIn charDiff 0 9 then
            result <- (result * 10) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)
    
let private int64Parse (path:string) ipos fpos =

    let mutable result = 0L
    let mutable negNumber = false
    let rec go pos =
        let charDiff = int64 path.[pos] - int64 '0'
        if int64In charDiff 0L 9L then
            result <- (result * 10L) + charDiff
            if pos = fpos then
                if negNumber then - result else result 
                |> box |> Some 
            else go (pos + 1)       // continue iter
        else None
    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)
    
let floatParse (path:string) ipos fpos =
    let mutable result = 0.
    let mutable decPlaces = 0
    let mutable negNumber = false
    let decPower = [|1.;10.;100.;1000.;10000.;100000.;1000000.;10000000.;100000000.;100000000.|]
    let rec go pos =
        if path.[pos] = '.' then
            decPlaces <- 1
            if pos < fpos then go (pos + 1) else None
        else
            let charDiff = float path.[pos] - float '0'
            if floatIn charDiff 0. 9. then
                if decPlaces = 0 then 
                    result <- (result * 10.) + charDiff
                else
                    result <- result + ( charDiff / decPower.[decPlaces])
                    decPlaces <- decPlaces + 1
                if pos = fpos || decPlaces > 9 then
                    if negNumber then - result else result 
                    |> box |> Some 
                else go (pos + 1)   // continue iter
            else None   // Invalid Character in path

    //Start Parse taking into account sign operator
    match path.[ipos] with
    | '-' -> negNumber <- true ; go (ipos + 1)
    | '+' -> go (ipos + 1)
    | _ -> go (ipos)

let formatStringMap =
    dict [
    // Char    Range Parser
    // ---------------  -------------------------------------------
        'b', (boolParse  )  // bool
        'c', (charParse  )  // char
        's', (stringParse)  // string
        'i', (intParse   )  // int
        'd', (int64Parse )  // int64
        'f', (floatParse )  // float
    ]

// implimenation of Trie Node
// assumptions: memory and compile time not relevant, all about execution speed, initially testing with Dictionary edges

type Node(iRouteFn:RouteCont<'T>) = 
    let mutable hasEdges = false //quick field to check if node has edges
    let edges = Dictionary<char,Node>()
    member val RouteFn = iRouteFn with get,set 
    member x.Add v routeFn =
        match edges.TryGetValue v with
        | true, node -> 
            match routeFn with
            | EmptyMap -> () 
            | rf -> 
                x.RouteFn <- rf
            node
        | false, _ -> 
            let node = Node(routeFn)
            edges.Add(v,node)
            if not hasEdges then hasEdges <- true //quick field to check if node has edges
            node
    
    member x.EdgeCount 
        with get () = edges.Count
    
    member x.GetEdgeKeys = edges.Keys
    member x.TryGetValue v = edges.TryGetValue v
    //member val Edges = edges with get

    member x.Search v =
        match edges.TryGetValue v with
        | true,node-> Some node
        | false,_-> None

    member x.HasCompletion (path:string) ipos =
        let fin = path.Length
        let rec go pos (node:Node) =
            if pos < fin then
                match node.TryGetValue path.[pos] with
                | true,cNode->
                    match node.RouteFn with
                    | EmptyMap -> go (pos + 1) cNode
                    | x -> Some x                    
                | false,_-> None
            else None
        go ipos x

and RouteCont<'T> =
| EmptyMap
| HandlerMap of HttpHandler
| ApplyMatch of ( char * (Node) )
| MatchComplete of ( (int) * ('T -> HttpHandler) ) // (lastParser, No# Parsers, Cont) 
| ApplyMatchAndComplete of ( char * (int) * ('T -> HttpHandler) ) // (lastParser, No# Parsers, Cont) 
| PartialMatch of ( char *  * (Node) )
| MatchComplete of ( (char list) * ('T -> HttpHandler) )


// test construction of Node Trie using node mapping functions
////////////////////////////////////////////////////

// Simple route that iterates down nodes and if function found, execute as normal
let tRoute (path:string) (fn:HttpHandler) (root:Node)=
    let last = path.Length - 1 
    let rec go i (node:Node) =
        if i = last then
            node.Add path.[i] (HandlerMap fn)
        else
            let nextNode = node.Add path.[i] EmptyMap
            go (i + 1) nextNode
    go 0 root

// parsing route that iterates down nodes, parses, and then continues down further notes if needed
let tRoutef (path : StringFormat<_,'T>) (fn:'T -> HttpHandler) (root:Node)=
    let last = path.Value.Length - 1
    let rec go i (pcount) (node:Node)  =
        if i = last then
            // have reached the end of the string match so add fn handler, and no continuation node
            node.Add path.Value.[i] (MatchComplete( pcount , fn ))
        else
            if path.Value.[i] = '%' && i + 1 <= last then
                let fmtChar = path.Value.[i + 1]
                // overrided % case
                if fmtChar = '%' then
                    node.Add '%' EmptyMap
                    |> go (i + 2) pcount
                // formater with valid key
                else if formatStringMap.ContainsKey fmtChar then

                    if i + 1 = last then // if at the end of the parse
                        node.RouteFn <- ApplyMatchAndComplete( fmtChar , pcount + 1 , fn )
                        node
                    else 
                        let newNodeBranch = Node(EmptyMap)
                        node.RouteFn <- ApplyMatch( fmtChar , newNodeBranch ) // need to insert Parser on this current node before match string part
                        go (i + 2) (pcount + 1) newNodeBranch 
                // badly formated format string that has unknown char after %
                else
                    failwith (sprintf "Routef parsing error, invalid format char identifier '%c' , should be: b | c | s | i | d | f" fmtChar)
                    node.Add path.Value.[i] EmptyMap
                    |> go (i + 1) pcount
            else
                node.Add path.Value.[i] EmptyMap
                |> go (i + 1) pcount
    go 0 0 root 

// choose root will apply its root node to all route mapping functions to generate Trie at compile time, function produced will take routeState (path) and execute appropriate function
let chooseRoute (routeState:RouteState) (fns:(Node->Node) list)  =
    let root = Node(EmptyMap)
    let rec go ls =
        match ls with
        | [] -> ()
        | h :: t ->
            h root |> ignore
            go t
    go fns

// process path fn that returns httpHandler
let processPath (path:string) (ipos:int) (root:Node) : HttpHandler =
    fun succ fail ctx -> 

        let last = path.Length - 1

        //gets pathposition and next function to apply
        let getNextCont ipos (inode:Node) = 
            let rec go pos (node:Node) =
                if pos <= last then
                    match node.TryGetValue path.[pos] with
                    | true,cnode -> 
                        match cnode.RouteFn with
                        | EmptyMap -> go (pos + 1) cnode
                        | x -> Some (pos ,x)
                    | false,_ -> None //go (pos + 1) node // if node does not match this pos, keep trying down path for match
                else
                    None
            go ipos inode

        //in case of patern match, where suffix text(s) have to match, keep trying down path till matches, returning end position to parse
        //this needs to be optimised 
        let findClosue ipos (inode:Node) =
            let rec find pos (node:Node) =
                match getNextCont pos node with
                | Some r -> Some r
                | None ->
                    if ipos <= last then
                        find (pos + 1) node
                    else
                        None
            find ipos inode

        let revLay pcount ls =
            let results = Array.zeroCreate<obj>(pcount)
            let rec go ls i =
                if i < 0 then None
                else match ls with
                        | [] -> Some results
                        | h :: t -> 
                            results.[i] <- h
                            go t (i - 1)
            go ls (pcount - 1)
        
        let rec go pos (node:Node) pobjs =
            
            let rec routeFnMatch pos pobjs = 
                function
                | EmptyMap -> go (pos + 1) n
                | HandlerMap fn -> fn
                | ApplyMatch (fmt,cnode) ->
                    match findClosure pos cnode with
                    | Some (npos,fn) -> 
                        match formatStringMap fmt pos npos with
                        | Some o -> routeFnMatch npos (o :: pobjs) fn
                        | None -> fail ctx //<< failed to parse, failed to match
                    | None -> fail ctx //<< no ending seq match, failed to match
                | MatchComplete (pcount,mfn) -> 
                    match argsOpt with
                    | Some args ->
                        let result =
                            match values.Length with
                            | 1 -> values.[0]
                            | _ ->
                                let types =
                                    values
                                    |> Array.map (fun v -> v.GetType())
                                let tupleType = FSharpType.MakeTupleType types
                                FSharpValue.MakeTuple(values, tupleType)
                        result :?> 'T |> Some 
                    | None -> 
                | ApplyMatchAndComplete of ( char * (int) * ('T -> HttpHandler) ) // (lastParser, No# Parsers, Cont) 

            
            match node.TryGetValue path.[pos] with
            | true, n -> 
                routeFnMatch pos pobjs n.RouteFn 
            | false , _ -> fail ctx

        go ipos root []