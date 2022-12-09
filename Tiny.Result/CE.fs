namespace Tiny.Result

[<AutoOpen>]
module ResultCE =

  type ResultBuilder() =
    member inline _.Return(value: 'a) : Result<'a, 'e> = Ok value

    member inline _.ReturnFrom(result: Result<'a, 'e>) : Result<'a, 'e> = result

    member inline this.Zero() : Result<unit, 'e> = this.Return()

    member inline _.Bind(result: Result<'a, 'e>, [<InlineIfLambda>] f: 'a -> Result<'b, 'e>) : Result<'b, 'e> =
      Result.bind f result

    member inline _.Delay([<InlineIfLambda>] f: unit -> Result<'a, 'e>) : unit -> Result<'a, 'e> = f

    member inline _.Run([<InlineIfLambda>] f: unit -> Result<'a, 'e>) : Result<'a, 'e> = f ()

    member inline _.Combine(result: Result<unit, 'e>, [<InlineIfLambda>] f: unit -> Result<'a, 'e>) : Result<'a, 'e> =
      Result.bind f result

    member inline this.TryWith
      (
        [<InlineIfLambda>] f: unit -> Result<'a, 'e>,
        [<InlineIfLambda>] g: exn -> Result<'a, 'e>
      ) : Result<'a, 'e> =
      try
        this.Run f
      with exn ->
        g exn

    member inline this.TryFinally
      (
        [<InlineIfLambda>] f: unit -> Result<'a, 'e>,
        [<InlineIfLambda>] g: unit -> unit
      ) : Result<'a, 'e> =
      try
        this.Run f
      finally
        g ()

    member inline this.Using
      (
        disposable: 'a :> #System.IDisposable,
        [<InlineIfLambda>] f: 'a -> Result<'b, 'e>
      ) : Result<'b, 'e> =
      this.TryFinally(
        (fun () -> f disposable),
        (fun () ->
          if not (obj.ReferenceEquals(disposable, null)) then
            disposable.Dispose())
      )

    member inline this.While
      (
        [<InlineIfLambda>] f: unit -> bool,
        [<InlineIfLambda>] g: unit -> Result<unit, 'e>
      ) : Result<unit, 'e> =
      while f () do
        g () |> ignore

      this.Zero()

    member inline this.For(sequence: #seq<'a>, [<InlineIfLambda>] f: 'a -> Result<unit, 'e>) : Result<unit, 'e> =
      this.Using(
        sequence.GetEnumerator(),
        fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> f enum.Current))
      )

    member inline _.BindReturn(result: Result<'a, 'e>, [<InlineIfLambda>] f: 'a -> 'b) : Result<'b, 'e> =
      Result.map f result

    member inline _.MergeSources(result1: Result<'a, 'e>, result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
      result1 |> Result.bind (fun r1 -> result2 |> Result.map (fun r2 -> r1, r2))

  let result = ResultBuilder()
