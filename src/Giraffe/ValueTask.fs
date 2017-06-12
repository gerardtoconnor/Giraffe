[<AutoOpenAttribute>]
module Giraffe.ValueTask
    open System
    open System.Collections
    open System.Collections.Generic

    open System.Threading
    open System.Threading.Tasks
       
    let toAsync (t: Task<'T>): Async<'T> =
        let abegin (cb: AsyncCallback, state: obj) : IAsyncResult = 
            match cb with
            | null -> upcast t
            | cb -> 
                t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
                upcast t
        let aend (r: IAsyncResult) = 
            (r :?> Task<'T>).Result
        Async.FromBeginEnd(abegin, aend)

    /// Transforms a Task's first value by using a specified mapping function.
    // let inline mapWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) f (m: Task<_>) =
    //     m.ContinueWith((fun (t: Task<_>) -> f t.Result), token, continuationOptions, scheduler)

    // /// Transforms a Task's first value by using a specified mapping function.
    // let inline map f (m: Task<_>) =
    //     m.ContinueWith(fun (t: Task<_>) -> f t.Result)

    // let inline bindWithOptions (token: CancellationToken) (continuationOptions: TaskContinuationOptions) (scheduler: TaskScheduler) (f: 'T -> Task<'U>) (m: Task<'T>) =
    //     if m.IsCompleted then f m.Result
    //     else
    //         let tcs =  new TaskCompletionSource<_>() // (Runtime.CompilerServices.AsyncTaskMethodBuilder<_>.Create())
    //         let t = tcs.Task
    //         let awaiter = m.GetAwaiter()
    //         awaiter.OnCompleted(fun _ -> tcs.SetResult(f m.Result))
    //         t.Unwrap()
    //         //m.ContinueWith((fun (x: Task<_>) -> f x.Result), token, continuationOptions, scheduler).Unwrap()
    let inline vtbind (f: 'T -> ValueTask<'U>) (m: ValueTask<'T>) =
        if m.IsCompleted then f m.Result
        else
            let tcs =  new TaskCompletionSource<'U>() // (Runtime.CompilerServices.AsyncTaskMethodBuilder<_>.Create())
            let t = tcs.Task
            let vt = ValueTask<'U>(t)
            let awaiter = m.GetAwaiter()
            awaiter.OnCompleted(fun _ -> tcs.SetResult((f m.Result).Result))
            vt
            //t.Unwrap()
            //m.ContinueWith((fun (x: Task<_>) -> f x.Result)).Unwrap()
    let inline tbind (f: 'T -> ValueTask<'U>) (m: Task<'T>) =
        if m.IsCompleted then f m.Result
        else
            let tcs =  new TaskCompletionSource<'U>() // (Runtime.CompilerServices.AsyncTaskMethodBuilder<_>.Create())
            let t = tcs.Task
            let vt = ValueTask<'U>(t)
            let awaiter = m.GetAwaiter()
            awaiter.OnCompleted(fun _ -> tcs.SetResult((f m.Result).Result))
            vt

    let inline asyncBind (f: 'T -> ValueTask<'U>) (am: Async<'T>) =
        let tcs =  new TaskCompletionSource<'U>() 
        let t = tcs.Task
        let vt = ValueTask<'U>(t)
        Async.StartWithContinuations (am,
                (fun v -> tcs.SetResult((f v).Result)),
                (fun e -> printfn "%A" e),
                (fun op -> printfn "%A" op))
        vt
            //t.Unwrap()
            //m.ContinueWith((fun (x: Task<_>) -> f x.Result)).Unwrap()
    let inline taskBind (f:unit -> ValueTask<'U>) (m:Task) =
        if m.IsCompleted then f ()
        else
            let tcs =  new TaskCompletionSource<'U>() // (Runtime.CompilerServices.AsyncTaskMethodBuilder<_>.Create())
            let t = tcs.Task
            let vt = ValueTask<'U>(t)
            let awaiter = m.GetAwaiter()
            awaiter.OnCompleted(fun _ -> tcs.SetResult((f ()).Result))
            vt

    let inline returnM (a:'T) = ValueTask<'T>(a)

    type TaskBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
        let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
        let scheduler = defaultArg scheduler TaskScheduler.Default
        let cancellationToken = defaultArg cancellationToken CancellationToken.None

        member this.Return x = returnM x

        member this.Zero() = returnM ()

        member this.ReturnFrom (a: ValueTask<'T>) = a
        member this.ReturnFrom (a: Task<'T>) = ValueTask<'T>(a)

        member this.Bind(m, f) = vtbind f m // bindWithOptions cancellationToken contOptions scheduler f m

        member this.Bind(m, f) = tbind f m // bindWithOptions cancellationToken contOptions scheduler f m

        member this.Bind(m, f) = taskBind f m

        member this.Bind(m, f) = asyncBind f m
        //member this.Bind(m,f) = bindTask f m

        //member this.Bind(m, f) = bindTask f m

        member this.Combine(comp1:Task<_>, comp2) =
            this.Bind(comp1, comp2)
            
        member this.Combine(comp1:ValueTask<_>, comp2) =
            this.Bind(comp1, comp2)

        member this.While(guard, m:unit -> ValueTask<_>) =
            let rec whileRec(guard, m:unit -> ValueTask<_>) = 
                if not(guard()) then this.Zero() else
                    this.Bind(m(), fun () -> whileRec(guard, m))
            whileRec(guard, m)
        
        member this.TryWith(m:unit -> ValueTask<_>,exFn) =
            try this.ReturnFrom (m())
            with ex -> exFn ex

        member this.TryFinally(m:unit -> ValueTask<_>, compensation) =
            try this.ReturnFrom (m())
            finally compensation()

        member this.Using(res: #IDisposable, body: #IDisposable -> ValueTask<_>) =
            this.TryFinally((fun () -> body res), fun () -> match res with null -> () | disp -> disp.Dispose())

        member this.For(sequence: seq<_>, body) =
            this.Using(sequence.GetEnumerator(),
                                    fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

        member this.Delay (f: unit -> ValueTask<'T>) = f

        member this.Run (f: unit -> ValueTask<'T>) = f()

    let task = TaskBuilder(scheduler = TaskScheduler.Current)