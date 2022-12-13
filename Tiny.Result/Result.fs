namespace Tiny.Result

[<RequireQualifiedAccess>]
module Result =

  let inline singleton (value: 'a) : Result<'a, 'e> = result { return value }

  let inline map2
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c)
    (result1: Result<'a, 'e>)
    (result2: Result<'b, 'e>)
    : Result<'c, 'e> =
    result {
      let! r1 = result1
      and! r2 = result2
      return f r1 r2
    }

  let inline map3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> 'd)
    (result1: Result<'a, 'e>)
    (result2: Result<'b, 'e>)
    (result3: Result<'c, 'e>)
    : Result<'d, 'e> =
    result {
      let! r1 = result1
      and! r2 = result2
      and! r3 = result3
      return f r1 r2 r3
    }

  let inline bimap
    ([<InlineIfLambda>] f: 'a -> 'b)
    ([<InlineIfLambda>] g: 'e1 -> 'e2)
    (result: Result<'a, 'e1>)
    : Result<'b, 'e2> =
    (Result.map f >> Result.mapError g) result

  let inline apply (f: Result<'a -> 'b, 'e>) (result: Result<'a, 'e>) : Result<'b, 'e> = map2 (|>) result f

  let inline bindError ([<InlineIfLambda>] f: 'e1 -> Result<'a, 'e2>) (result: Result<'a, 'e1>) : Result<'a, 'e2> =
    match result with
    | Ok ok -> Ok ok
    | Error error -> f error

  let inline bind2
    ([<InlineIfLambda>] f: 'a -> 'b -> Result<'c, 'e>)
    (result1: Result<'a, 'e>)
    (result2: Result<'b, 'e>)
    : Result<'c, 'e> =
    result.Bind2(result1, result2, (fun (a, b) -> f a b))

  let inline bind3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> Result<'d, 'e>)
    (result1: Result<'a, 'e>)
    (result2: Result<'b, 'e>)
    (result3: Result<'c, 'e>)
    : Result<'d, 'e> =
    result.Bind3(result1, result2, result3, (fun (a, b, c) -> f a b c))

  let inline sequence (results: Result<'a, 'e> list) : Result<'a list, 'e> =
#if FABLE_COMPILER
    let rec loop state =
      function
      | [] -> state |> List.rev |> singleton
      | r :: rs -> r |> Result.bind (fun a -> loop (a :: state) rs)

    loop [] results
#else
    result {
      let mutable collector = FSharp.Core.CompilerServices.ListCollector<'a>()

      for r in results do
        let! a = r
        collector.Add a

      return collector.Close()
    }
#endif

  let inline zip (result1: Result<'a, 'e>) (result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
    result.MergeSources(result1, result2)

  let inline zip3
    (result1: Result<'a, 'e>)
    (result2: Result<'b, 'e>)
    (result3: Result<'c, 'e>)
    : Result<'a * 'b * 'c, 'e> =
    result.MergeSources3(result1, result2, result3)

  let inline ofOption (error: 'e) (option: 'a option) : Result<'a, 'e> =
    match option with
    | Some thing -> Ok thing
    | None -> Error error

  let inline toOption (result: Result<'a, 'b>) : 'a option =
    match result with
    | Ok ok -> Some ok
    | Error _ -> None

  let inline ofChoice (choice: Choice<'a, 'e>) : Result<'a, 'e> =
    match choice with
    | Choice1Of2 left -> Ok left
    | Choice2Of2 right -> Error right

  let inline toChoice (result: Result<'a, 'e>) : Choice<'a, 'e> =
    match result with
    | Ok ok -> Choice1Of2 ok
    | Error error -> Choice2Of2 error

  /// Creates a safe version of the supplied function, returning Error(exn) instead of throwing an exception.
  let inline ofThrowable ([<InlineIfLambda>] f: 'a -> 'b) (a: 'a) : Result<'b, exn> =
    try
      Ok(f a)
    with exn ->
      Error exn
