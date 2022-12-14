namespace Tiny.AsyncResult

open System.Threading.Tasks

[<AutoOpen>]
module AsyncResultCEExtensions2 =
  type AsyncResultBuilder with

    member inline _.Source(asyncOp: Async<'a>) : AsyncResult<'a, exn> = AsyncResult.ofAsync asyncOp

    member inline _.Source(result: Result<'a, 'e>) : AsyncResult<'a, 'e> = AsyncResult.ofResult result

    member inline _.Source(task: Task<'a>) : AsyncResult<'a, exn> =
      task |> Async.AwaitTask |> AsyncResult.ofAsync

    member inline _.Source(unitTask: Task) : AsyncResult<unit, exn> =
      unitTask |> Async.AwaitTask |> AsyncResult.ofAsync

    member inline _.Source(taskResult: Task<Result<'a, 'e>>) : AsyncResult<'a, 'e> = taskResult |> Async.AwaitTask
