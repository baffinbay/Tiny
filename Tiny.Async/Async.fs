namespace Tiny.Async

[<RequireQualifiedAccess>]
module Async =
  let inline singleton (value: 'a) : Async<'a> = async.Return value

  let inline bind ([<InlineIfLambda>] f: 'a -> Async<'b>) (asyncOp: Async<'a>) : Async<'b> = async.Bind(asyncOp, f)

  let inline bind2
    ([<InlineIfLambda>] f: 'a -> 'b -> Async<'c>)
    (asyncOp1: Async<'a>)
    (asyncOp2: Async<'b>)
    : Async<'c> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      return! f a b
    }

  let inline bind3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> Async<'d>)
    (asyncOp1: Async<'a>)
    (asyncOp2: Async<'b>)
    (asyncOp3: Async<'c>)
    : Async<'d> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      let! c = asyncOp3
      return! f a b c
    }

  let inline map ([<InlineIfLambda>] f: 'a -> 'b) (asyncOp: Async<'a>) : Async<'b> =
    async {
      let! a = asyncOp
      return f a
    }

  let inline map2 ([<InlineIfLambda>] f: 'a -> 'b -> 'c) (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) : Async<'c> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      return f a b
    }

  let inline map3
    ([<InlineIfLambda>] f: 'a -> 'b -> 'c -> 'd)
    (asyncOp1: Async<'a>)
    (asyncOp2: Async<'b>)
    (asyncOp3: Async<'c>)
    : Async<'d> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      let! c = asyncOp3
      return f a b c
    }

  let inline apply (f: Async<'a -> 'b>) (asyncOp: Async<'a>) : Async<'b> = map2 (|>) asyncOp f

  let inline andMap (asyncOp: Async<'a>) (f: Async<'a -> 'b>) : Async<'b> = apply f asyncOp

  let inline zip (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) : Async<'a * 'b> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      return a, b
    }

  let inline zip3 (asyncOp1: Async<'a>) (asyncOp2: Async<'b>) (asyncOp3: Async<'c>) : Async<'a * 'b * 'c> =
    async {
      let! a = asyncOp1
      let! b = asyncOp2
      let! c = asyncOp3
      return a, b, c
    }

[<AutoOpen>]
module AsyncCEExtensions =

  type FSharp.Control.AsyncBuilder with

    member inline _.BindReturn(async: Async<'a>, [<InlineIfLambda>] f: 'a -> 'b) : Async<'b> = Async.map f async

    member inline _.Bind2
      (
        asyncOp1: Async<'a>,
        asyncOp2: Async<'b>,
        [<InlineIfLambda>] f: 'a * 'b -> Async<'c>
      ) : Async<'c> =
      Async.bind2 (fun a b -> f (a, b)) asyncOp1 asyncOp2

    member inline _.Bind3
      (
        asyncOp1: Async<'a>,
        asyncOp2: Async<'b>,
        asyncOp3: Async<'c>,
        [<InlineIfLambda>] f: 'a * 'b * 'c -> Async<'d>
      ) : Async<'d> =
      Async.bind3 (fun a b c -> f (a, b, c)) asyncOp1 asyncOp2 asyncOp3

    member inline _.MergeSources(asyncOp1: Async<'a>, asyncOp2: Async<'b>) : Async<'a * 'b> =
      Async.zip asyncOp1 asyncOp2

    member inline _.MergeSources3(asyncOp1: Async<'a>, asyncOp2: Async<'b>, asyncOp3: Async<'c>) : Async<'a * 'b * 'c> =
      Async.zip3 asyncOp1 asyncOp2 asyncOp3

    member inline _.Source(asyncOp: Async<'a>) : Async<'a> = asyncOp

[<AutoOpen>]
module AsyncCEExtensions2 =

  open System.Threading.Tasks

  type FSharp.Control.AsyncBuilder with

    member inline _.Source(task: Task<'a>) : Async<'a> = Async.AwaitTask task

    member inline _.Source(unitTask: Task) : Async<unit> = Async.AwaitTask unitTask
