namespace PublicWeb

open System
open System.Reflection
open FSharp.Control.Tasks.ContextSensitive
open System.Threading
open Newtonsoft.Json
open Microsoft.FSharp.Reflection

open RequestParsing
open FSharp.Control
open System.Threading.Tasks

module NumberRequestFulfilment =
    type RandomAsync() =
        let random() = 
            let byte = Array.zeroCreate<byte> 4
            Security.Cryptography.RandomNumberGenerator.Fill(byte.AsSpan())
            new Random(BitConverter.ToInt32(ReadOnlySpan(byte)))
            
        member __.Next(min : int, max : int) = task {
            return! ValueTask<_>(random().Next(min, max + 1))
        }
        
        member __.NextMany(min : int, max : int, numberOfInts) = task {
            return Array.init numberOfInts (fun _ -> random().Next(min, max + 1))
        }


        member __.GetBytes(numOfBytes) = async {
            let bytes = Array.zeroCreate<byte> numOfBytes
            Security.Cryptography.RandomNumberGenerator.Fill(bytes.AsSpan())
            return bytes
        }

        member this.RandomizeArray(arr : int array) = task {
            let shuffleArray = arr |> Array.map id
            for i in Seq.init shuffleArray.Length (fun i -> shuffleArray.Length - 1 - i) do
                let! j = this.Next(0, i)
                let temp = shuffleArray.[i]
                shuffleArray.[i] <- shuffleArray.[j]
                shuffleArray.[j] <- temp;
            return shuffleArray
        }

    let random = RandomAsync()

    type RequestFulfilment =
    | OneInteger of int Async
    | ManyIntegers of int array AsyncSeq
    | Binary of byte array AsyncSeq

    let fulfillRequest (request : RequestParsing.RandomNumberRequest) = 
        match request.requestType with
        | RequestModeType.Normal (range, multiplier) ->
            match multiplier with
            | Some mult ->
                let intSegmentSize = 1024 * 16  //16kb
                Seq.init (mult / intSegmentSize) (fun _ -> intSegmentSize)
                |> Seq.append (match mult % intSegmentSize with | 0 -> [] | modu -> [modu])
                |> Seq.map (fun intSegmentSize -> random.NextMany(range.min, range.max, intSegmentSize) |> Async.AwaitTask)
                |> AsyncSeq.ofSeqAsync
                |> ManyIntegers
            | None ->
                OneInteger (random.Next(range.min, range.max) |> Async.AwaitTask)
        | RequestModeType.Shuffle (range) ->
            [async {
                let toShuffle = Array.init (range.max - range.min + 1) ((+) range.min)
                return! random.RandomizeArray toShuffle |> Async.AwaitTask
            }]
            |> AsyncSeq.ofSeqAsync
            |> ManyIntegers
        | RequestModeType.Unique (range, multiplier) ->
            [async {
                let toShuffle = Array.init (range.max - range.min + 1) ((+) range.min)
                let! shuffledArray = random.RandomizeArray toShuffle |> Async.AwaitTask
                return shuffledArray |> Array.take multiplier
            }]
            |> AsyncSeq.ofSeqAsync
            |> ManyIntegers
        | RequestModeType.Binary (numOfBytes) ->
            let byteSegmentSize = 1024 * 16 //16kb
            Seq.init (numOfBytes / byteSegmentSize) (fun _ -> byteSegmentSize)
            |> Seq.append (match numOfBytes % byteSegmentSize with | 0 -> [] | modu -> [modu])
            |> Seq.map random.GetBytes 
            |> AsyncSeq.ofSeqAsync
            |> Binary
    


