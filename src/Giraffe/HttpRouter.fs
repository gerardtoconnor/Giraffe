module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic

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
        | 't' | 'T' -> true  |> box |> Some // Laxy matching, i'll complete later
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
            node

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

        /// Multiple Edge designs possible, for now use simple dictionary over sorted array w/ binary search

        // let rec go l r = 
        //     if l > r then 
        //         None 
        //     else
        //         let m = (l + r) / 2
        //         if edges.[m].Char = v then edges.[m].Node |> Some
        //         else if edges.[m].Char < v then go (m + 1) r
        //         else if edges.[m].Char > v then go l (m - 1)
        //         else None
    
        // match edges.Count with 
        // | 0 -> None
        // | 1 -> if edges.[0].Char = v then edges.[0].Node |> Some else None 
        // | n -> go 0 (n - 1) 

and RouteCont<'T> =
| EmptyMap
| HandlerMap of HttpHandler
| PartialMatch of ( char * (Node) )
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
let tRoutef (path : StringFormat<_,'U>) (fn:'U -> HttpHandler) (root:Node)=
    let last = path.Value.Length - 1 
    let rec go i (fmap:char list) (node:Node)  =
        if i = last then
            // have reached the end of the string match so add fn handler, and no continuation node
            node.Add path.Value.[i] (MatchComplete( fmap , fn ))
        else
            if path.Value.[i] = '%' && i + 1 <= last then
                let fmtChar = path.Value.[i + 1]
                // overrided % case
                if fmtChar = '%' then
                    node.Add '%' EmptyMap
                    |> go (i + 2) fmap
                // formater with valid key
                else if formatStringMap.ContainsKey fmtChar then
                    let fmap' = fmtChar :: fmap 
                    if i + 1 = last then // if at the end of the parse
                        node.RouteFn <- MatchComplete( fmap' , fn )
                        node
                    else 
                        let newNodeBranch = Node(EmptyMap)
                        node.RouteFn <- PartialMatch( fmtChar , newNodeBranch ) // need to insert Parser on this current node before match string part
                        go (i + 2) fmap' newNodeBranch 
                // badly formated format string that has unknown char after %
                else
                    failwith (sprintf "Routef parsing error, invalid format char '%c' , should be: b | c | s | i | d | f" fmtChar)
                    node.Add path.Value.[i] EmptyMap
                    |> go (i + 1) fmap
            else
                node.Add path.Value.[i] EmptyMap
                |> go (i + 1) fmap
    go 0 [] root 

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

        let pathLen = path.Length

        let findClosure ipos (inode:Node) =
            let last = path.Length - 1
            let rec go pos (node:Node) =
                if pos < last then
                    match node.TryGetValue path.[pos] with
                    | true,cnode -> 
                        match cnode.RouteFn with
                        | EmptyMap -> go (pos + 1) node
                        | x -> Some (pos ,x)
                    | false,_ -> go (pos + 1) node
                else
                    None
            go ipos inode
            
        let rec routeFnMatch pos = 
                function
                | EmptyMap -> go (pos + 1) n
                | HandlerMap fn -> fn
                | PartialMatch (fmt,cnode) ->
                    match findClosure pos cnode with
                    | Some (npos,fn) -> routeFnMatch 
                    | None -> fail ctx
                | MatchComplete (cls,mfn) -> 
                    let matchBounds =
                        match nOpt with
                        | None -> (pos + 1 , path.Length - 1)
                        | Some snode -> 
                            match findClosure path pos snode with
                            | None -> (pos + 1 , path.Length - 1)
                            | Some fin -> (pos + 1, fin -1)

        

        let rec go pos (node:Node) =
            match node.TryGetValue path.[pos] with
            | true, n -> 
                routeFnMatch n.RouteFn
            | false , _ -> fail ctx
        go ipos root