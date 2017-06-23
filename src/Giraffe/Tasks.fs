[<AutoOpenAttribute>]
module Giraffe.Tasks

open System
open System.Collections
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

// open System.Runtime.ExceptionServices

let inline asTask (t : Task<unit>) = t :> Task

let toAsync (t : Task<'T>) : Async<'T> =
    let abegin (cb : AsyncCallback, state : obj) : IAsyncResult =
        match cb with
        | null -> upcast t
        | cb   ->
            t.ContinueWith(fun (_ : Task<_>) -> cb.Invoke t) |> ignore
            upcast t
    let aend (r : IAsyncResult) =
        (r :?> Task<'T>).Result
    Async.FromBeginEnd(abegin, aend)

let inline mapWithOptions (token     : CancellationToken)
                          (options   : TaskContinuationOptions)
                          (scheduler : TaskScheduler)
                          (f         : 'a -> 'b)
                          (m         : Task<_>) =
    m.ContinueWith((fun (t : Task<_>) -> f t.Result), token, options, scheduler)

let inline map f (m : Task<_>) = m.ContinueWith(fun (t : Task<_>) -> f t.Result)

let inline bindTaskWithOptions (token     : CancellationToken)
                               (options   : TaskContinuationOptions)
                               (scheduler : TaskScheduler)
                               (f         : unit -> Task<'U>)
                               (m         : Task) =
    if m.IsCompleted then f()
    else m.ContinueWith((fun _ -> f()), token, options, scheduler).Unwrap()

let inline bindWithOptions (token     : CancellationToken)
                           (options   : TaskContinuationOptions)
                           (scheduler : TaskScheduler)
                           (f         : 'T -> Task<'U>)
                           (m         : Task<'T>) =
    if m.IsCompleted then f m.Result
    else m.ContinueWith((fun (t : Task<_>) -> f t.Result), token, options, scheduler).Unwrap()

let inline bind (f : 'T -> Task<'U>) (m : Task<'T>) =
    if m.IsCompleted then f m.Result
    else m.ContinueWith(fun (t : Task<_>) -> f t.Result).Unwrap()

let inline returnM a = 
    let s = TaskCompletionSource()
    s.SetResult a
    s.Task

type TaskBuilder (?continuationOptions, ?scheduler, ?cancellationToken) =
    let options   = defaultArg continuationOptions TaskContinuationOptions.None
    let scheduler = defaultArg scheduler TaskScheduler.Default
    let token     = defaultArg cancellationToken CancellationToken.None

    member this.Return x = returnM x
    member this.Zero() = returnM ()
    member this.ReturnFrom (a: Task) = this.Bind(a, Task.FromResult)
    member this.ReturnFrom (a: Task<'T>) = a
    member this.Bind (m : Task, f : unit -> Task<'U>) = bindTaskWithOptions token options scheduler f m
    member this.Bind (m : Task<'T>, f : 'T -> Task<'U>) = bindWithOptions token options scheduler f m
    member this.Combine (comp1 : Task, comp2 : unit -> Task<'U>) = this.Bind(comp1, comp2)
    member this.Combine (comp1 : Task<'T>, comp2 : 'T -> Task<'U>) = this.Bind(comp1, comp2)

    member this.While (guard, m) =
        if not(guard())
        then this.Zero()
        else this.Bind(m(), fun () -> this.While(guard, m))

    member this.TryWith (body : unit -> Task<_>, catchFn : exn -> Task<_>) =  
        try
            body()
                .ContinueWith(fun (t : Task<_>) ->
                    match t.IsFaulted with
                    | false -> t
                    | true  -> catchFn(t.Exception.GetBaseException()))
                .Unwrap()
        with e -> catchFn e

    member this.TryFinally (m, compensation) =
        try this.ReturnFrom m
        finally compensation()

    member this.Using (res : #IDisposable, body : #IDisposable -> Task<_>) =
        this.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member this.For (sequence : seq<_>, body) =
        this.Using(
            sequence.GetEnumerator(),
            fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

    member this.Delay (f : unit -> Task<'T>) = f

    member this.Run (f : unit -> Task<'T>) = f()

let task = TaskBuilder(scheduler = TaskScheduler.Current)




// type AwaitBuilder(?continuationOptions, ?scheduler, ?cancellationToken) =
//    let contOptions = defaultArg continuationOptions TaskContinuationOptions.None
//    let scheduler = defaultArg scheduler TaskScheduler.Default
//    let cancellationToken = defaultArg cancellationToken CancellationToken.None

//    member this.Zero() = Task.CompletedTask

//    member this.ReturnFrom (a: Task) = a

//    member this.Bind(m:Task, f:unit->Task) : Task = 
//       if m.IsCompleted then f ()
//       else m.ContinueWith((fun (x: Task) -> f ()), cancellationToken, contOptions, scheduler).Unwrap()

//    member this.Bind(m:Task<'T>, f:'T->Task) : Task = 
//       if m.IsCompleted then f m.Result
//       else m.ContinueWith((fun (x: Task<'T>) -> f x.Result), cancellationToken, contOptions, scheduler).Unwrap()
   
//    member this.Combine(comp1:Task, comp2:unit->Task) =
//       this.Bind(comp1, comp2)

//    member this.Combine(comp1:Task<'T>, comp2:'T->Task ) =
//       this.Bind(comp1, comp2)

//    member this.While(guard, m) =
//       if not(guard()) then this.Zero() else
//             this.Bind(m(), fun () -> this.While(guard, m))

//    member this.TryWith(body:unit -> Task, catchFn:exn -> Task) =  
//       try
//          body()
//           .ContinueWith(fun (t:Task) ->
//              match t.IsFaulted with
//              | false -> t
//              | true  -> catchFn(t.Exception.GetBaseException()))
//           .Unwrap()
//       with e -> catchFn(e)

//    member this.TryFinally(body:unit->Task, compensation) =
//       let wrapOk () : Task =
//           compensation()
//           Task.CompletedTask

//       let wrapCrash (e:exn) : Task =
//             printfn ">> the following exception has been receieved : %A" e.Message
//             compensation()
//             ExceptionDispatchInfo.Capture(e).Throw() 
//             raise e
      
//       this.Bind(this.TryWith(body, wrapCrash), wrapOk)
//    member this.Using(res: #IDisposable, body: #IDisposable -> Task) =
//       this.TryFinally(
//             (fun () -> body res),
//             (fun () -> match res with null -> () | disp -> disp.Dispose())
//             )

//    member this.For(sequence: seq<_>, body) =
//       this.Using(sequence.GetEnumerator(),
//                      fun enum -> this.While(enum.MoveNext, fun () -> body enum.Current))

//    member this.Delay (body : unit -> Task) = body

//    member this.Run (body: unit -> Task) = body()

// let await = AwaitBuilder()