namespace Tiny.AsyncResult

open Tiny.Async
open Tiny.Result

[<RequireQualifiedAccess>]
module AsyncResult =
  let inline singleton (value: 'a) : AsyncResult<'a, 'e> = asyncResult.Return value

  let inline apply (f: AsyncResult<'a -> 'b, 'e>) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> =
    AsyncResultCE.asyncResult {
      let! f = f
      and! ar = asyncResult
      return f ar
    }

  let inline andMap (result: AsyncResult<'a, 'e>) (f: AsyncResult<'a -> 'b, 'e>) : AsyncResult<'b, 'e> = apply f result

  let inline map (f: 'a -> 'b) (asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'b, 'e> =
    AsyncResultCE.asyncResult.BindReturn(asyncResult, f)

  let inline mapError (f: 'e1 -> 'e2) (asyncResult: AsyncResult<'a, 'e1>) : AsyncResult<'a, 'e2> =
    Async.map (Result.mapError f) asyncResult

  let inline map2
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c)
    (result1: AsyncResult<'a, 'e>)
    (result2: AsyncResult<'b, 'e>)
    : AsyncResult<'c, 'e> =
    singleton f |> andMap result1 |> andMap result2

  let inline map3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> 'd)
    (result1: AsyncResult<'a, 'e>)
    (result2: AsyncResult<'b, 'e>)
    (result3: AsyncResult<'c, 'e>)
    : AsyncResult<'d, 'e> =
    singleton f |> andMap result1 |> andMap result2 |> andMap result3

  let inline bimap
    ([<InlineIfLambda>] f: 'a -> 'b)
    ([<InlineIfLambda>] g: 'e1 -> 'e2)
    (result: AsyncResult<'a, 'e1>)
    : AsyncResult<'b, 'e2> =
    (map f >> mapError g) result

  let inline bind
    ([<InlineIfLambda>] f: 'a -> AsyncResult<'b, 'e>)
    (asyncResult: AsyncResult<'a, 'e>)
    : AsyncResult<'b, 'e> =
    AsyncResultCE.asyncResult.Bind(asyncResult, f)

  let inline bindError ([<InlineIfLambda>] f: 'e1 -> Result<'a, 'e2>) (result: Result<'a, 'e1>) : Result<'a, 'e2> =
    match result with
    | Ok ok -> Ok ok
    | Error error -> f error

  let inline bind2
    ([<InlineIfLambda>] f: 'a -> 'b -> AsyncResult<'c, 'e>)
    (result1: AsyncResult<'a, 'e>)
    (result2: AsyncResult<'b, 'e>)
    : AsyncResult<'c, 'e> =
    asyncResult.Bind2(result1, result2, (fun (a, b) -> f a b))

  let inline bind3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> AsyncResult<'d, 'e>)
    (result1: AsyncResult<'a, 'e>)
    (result2: AsyncResult<'b, 'e>)
    (result3: AsyncResult<'c, 'e>)
    : AsyncResult<'d, 'e> =
    asyncResult.Bind3(result1, result2, result3, (fun (a, b, c) -> f a b c))

  let inline sequence (results: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
#if FABLE_COMPILER
    let rec loop state =
      function
      | [] -> state |> List.rev |> singleton
      | r :: rs -> r |> bind (fun a -> loop (a :: state) rs)

    loop [] results
#else
    asyncResult {
      let mutable collector = FSharp.Core.CompilerServices.ListCollector<'a>()

      for r in results do
        let! a = r
        collector.Add a

      return collector.Close()
    }
#endif

  let inline ``parallel`` (asyncResults: AsyncResult<'a, 'e> list) : AsyncResult<'a list, 'e> =
    asyncResults |> Async.Parallel |> Async.map (Array.toList >> Result.sequence)

  let inline zip (result1: AsyncResult<'a, 'e>) (result2: AsyncResult<'b, 'e>) : AsyncResult<'a * 'b, 'e> =
    asyncResult.MergeSources(result1, result2)

  let inline zip3
    (result1: AsyncResult<'a, 'e>)
    (result2: AsyncResult<'b, 'e>)
    (result3: AsyncResult<'c, 'e>)
    : AsyncResult<'a * 'b * 'c, 'e> =
    asyncResult.MergeSources3(result1, result2, result3)

  let inline ofAsync (asyncOp: Async<'a>) : AsyncResult<'a, exn> =
    asyncOp |> Async.Catch |> Async.map Result.ofChoice

  let inline ofOption (error: 'e) (option: 'a option) : AsyncResult<'a, 'e> =
    match option with
    | Some thing -> singleton thing
    | None -> Error error |> Async.singleton

  let inline ofResult (result: Result<'a, 'e>) : AsyncResult<'a, 'e> = Async.singleton result

  let inline ofChoice (choice: Choice<'a, 'e>) : AsyncResult<'a, 'e> =
    match choice with
    | Choice1Of2 left -> singleton left
    | Choice2Of2 right -> Error right |> Async.singleton
