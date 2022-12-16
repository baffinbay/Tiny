namespace Tiny.Option

module OptionCE =
  type OptionBuilder() =
    member inline _.Return(value: 'a) : 'a option = Some value

    member inline _.ReturnFrom(option: 'a option) : 'a option = option

    member inline this.Zero() : unit option = this.Return()

    member inline _.Bind(option: 'a option, [<InlineIfLambda>] f: 'a -> 'b option) : 'b option = Option.bind f option

    member inline _.Bind2
      (
        option1: 'a option,
        option2: 'b option,
        [<InlineIfLambda>] f: 'a * 'b -> 'c option
      ) : 'c option =
      option1 |> Option.bind (fun r1 -> option2 |> Option.bind (fun r2 -> f (r1, r2)))

    member inline _.Bind3
      (
        option1: 'a option,
        option2: 'b option,
        option3: 'c option,
        [<InlineIfLambda>] f: 'a * 'b * 'c -> 'd option
      ) : 'd option =
      option1
      |> Option.bind (fun o1 ->
        option2
        |> Option.bind (fun o2 -> option3 |> Option.bind (fun o3 -> f (o1, o2, o3))))

    member inline _.Delay([<InlineIfLambda>] f: unit -> 'a option) : unit -> 'a option = f

    member inline _.Run([<InlineIfLambda>] f: unit -> 'a option) : 'a option = f ()

    member inline _.Combine(option: 'a option, [<InlineIfLambda>] f: 'a -> 'b option) : 'b option = Option.bind f option

    member inline this.TryWith
      (
        [<InlineIfLambda>] f: unit -> 'a option,
        [<InlineIfLambda>] g: exn -> 'a option
      ) : 'a option =
      try
        this.Run f
      with exn ->
        g exn

    member inline this.TryFinally
      (
        [<InlineIfLambda>] f: unit -> 'a option,
        [<InlineIfLambda>] g: unit -> unit
      ) : 'a option =
      try
        this.Run f
      finally
        g ()

    member inline this.Using(disposable: 'a :> System.IDisposable, [<InlineIfLambda>] f: 'a -> 'a option) : 'a option =
      this.TryFinally(
        (fun () -> f disposable),
        (fun () ->
          if not (obj.ReferenceEquals(disposable, null)) then
            disposable.Dispose())
      )

    member inline this.While
      (
        [<InlineIfLambda>] f: unit -> bool,
        [<InlineIfLambda>] g: unit -> unit option
      ) : unit option =
      while f () do
        g () |> ignore

      this.Zero()

    member inline this.For(sequence: #seq<'a>, [<InlineIfLambda>] f: 'a -> unit option) : unit option =
      this.Using(
        sequence.GetEnumerator(),
        fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> f enum.Current))
      )

    member inline _.BindReturn(result: Result<'a, 'e>, [<InlineIfLambda>] f: 'a -> 'b) : Result<'b, 'e> =
      Result.map f result

    member inline this.MergeSources(result1: Result<'a, 'e>, result2: Result<'b, 'e>) : Result<'a * 'b, 'e> =
      this.Bind2(result1, result2, (fun (r1, r2) -> this.Return(r1, r2)))

    member inline this.MergeSources3
      (
        result1: Result<'a, 'e>,
        result2: Result<'b, 'e>,
        result3: Result<'c, 'e>
      ) : Result<'a * 'b * 'c, 'e> =
      this.Bind3(result1, result2, result3, (fun (r1, r2, r3) -> this.Return(r1, r2, r3)))

  let option = OptionBuilder()
