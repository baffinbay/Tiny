namespace Tiny.AsyncResult

open Tiny.Async
open Tiny.Result

type AsyncResult<'a, 'e> = Async<Result<'a, 'e>>

[<AutoOpen>]
module AsyncResultCE =
  type AsyncResultBuilder() =
    member inline _.Return(value: 'a) : AsyncResult<'a, 'e> =
      (Result.singleton >> Async.singleton) value

    member inline _.ReturnFrom(asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = asyncResult

    member inline this.Zero() : AsyncResult<unit, 'e> = this.Return()

    member inline _.Bind
      (
        asyncResult: AsyncResult<'a, 'e>,
        [<InlineIfLambda>] f: 'a -> AsyncResult<'b, 'e>
      ) : AsyncResult<'b, 'e> =
      async {
        match! asyncResult with
        | Ok ok -> return! f ok
        | Error err -> return Error err
      }

    member inline this.Bind2
      (
        asyncResult1: AsyncResult<'a, 'e>,
        asyncResult2: AsyncResult<'b, 'e>,
        [<InlineIfLambda>] f: 'a * 'b -> AsyncResult<'c, 'e>
      ) : AsyncResult<'c, 'e> =
      this.Bind(asyncResult1, (fun r1 -> this.Bind(asyncResult2, (fun r2 -> f (r1, r2)))))

    member inline this.Bind3
      (
        asyncResult1: AsyncResult<'a, 'e>,
        asyncResult2: AsyncResult<'b, 'e>,
        asyncResult3: AsyncResult<'c, 'e>,
        [<InlineIfLambda>] f: 'a * 'b * 'c -> AsyncResult<'d, 'e>
      ) : AsyncResult<'d, 'e> =
      this.Bind(
        asyncResult1,
        (fun r1 -> this.Bind(asyncResult2, (fun r2 -> this.Bind(asyncResult3, (fun r3 -> f (r1, r2, r3))))))
      )

    member inline _.Delay([<InlineIfLambda>] f: unit -> AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = async.Delay f

    member inline this.Combine
      (
        unitAsyncResult: AsyncResult<unit, 'e>,
        asyncResult: AsyncResult<'a, 'e>
      ) : AsyncResult<'a, 'e> =
      this.Bind(unitAsyncResult, (fun () -> asyncResult))

    member inline _.TryWith(asyncResult: AsyncResult<'a, 'e>, f: exn -> AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> =
      async.TryWith(asyncResult, f)

    member inline _.TryFinally
      (
        asyncResult: AsyncResult<'a, 'e>,
        [<InlineIfLambda>] f: unit -> unit
      ) : AsyncResult<'a, 'e> =
      async.TryFinally(asyncResult, f)

    member inline _.Using
      (
        disposable: 'a :> System.IDisposable,
        [<InlineIfLambda>] f: 'a -> AsyncResult<'b, 'e>
      ) : AsyncResult<'b, 'e> =
      async.Using(disposable, f)

    member inline this.While
      (
        [<InlineIfLambda>] f: unit -> bool,
        asyncResult: AsyncResult<unit, 'e>
      ) : AsyncResult<unit, 'e> =
      let mutable whileAsyncResult = this.Zero()

      whileAsyncResult <- this.Bind(asyncResult, (fun () -> if f () then whileAsyncResult else this.Zero()))

      whileAsyncResult

    member inline this.For
      (
        sequence: #seq<'a>,
        [<InlineIfLambda>] f: 'a -> AsyncResult<unit, 'e>
      ) : AsyncResult<unit, 'e> =
      this.Using(
        sequence.GetEnumerator(),
        fun enum -> this.While(enum.MoveNext, this.Delay(fun () -> f enum.Current))
      )

    member inline _.BindReturn(asyncResult: AsyncResult<'a, 'e>, [<InlineIfLambda>] f: 'a -> 'b) : AsyncResult<'b, 'e> =
      (Result.map >> Async.map) f asyncResult

    member inline this.MergeSources
      (
        asyncResult1: AsyncResult<'a, 'e>,
        asyncResult2: AsyncResult<'b, 'e>
      ) : AsyncResult<'a * 'b, 'e> =
      this.Bind2(asyncResult1, asyncResult2, (fun (r1, r2) -> this.Return(r1, r2)))

    member inline this.MergeSources3
      (
        asyncResult1: AsyncResult<'a, 'e>,
        asyncResult2: AsyncResult<'b, 'e>,
        asyncResult3: AsyncResult<'c, 'e>
      ) : AsyncResult<'a * 'b * 'c, 'e> =
      this.Bind3(asyncResult1, asyncResult2, asyncResult3, (fun (r1, r2, r3) -> this.Return(r1, r2, r3)))

    member inline _.Source(asyncResult: AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> = asyncResult

  let asyncResult = AsyncResultBuilder()

[<AutoOpen>]
module AsyncResultCEExtensions1 =
  type AsyncResultBuilder with

    member inline _.Source(sequence: #seq<'a>) : #seq<'a> = sequence
