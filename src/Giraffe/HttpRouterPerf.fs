module Giraffe.HttpRouter

open Giraffe.HttpHandlers
open FSharp.Core.Printf
open System.Collections.Generic

// this is a model for further performant router that uses struct nodes

type INodeType =
| EmptyNode = 0y
| HandlerFn = 1y
| MatchFn = 2y
// performance Node Trie
type Matcher() =
    member val Fmts = Array.zeroCreate<char>(0) with get, set
    member val IsAnotherNode = false with get,set
    member val NextNode = Unchecked.defaultof<TNode> with get,set
    member val HandlerFn = Unchecked.defaultof<HttpHandler> with get,set
and TNode =
    struct
        val Ubound : int
        val LBound : int
        val Edges : TNode []

        val NodeType : INodeType 
        val HandlerFn : HttpHandler
        val MatherFn : Matcher
        //new(noteType) { Ubound = }
    end