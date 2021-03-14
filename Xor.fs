module Xor

let xor (bts1 : byte []) (bts2 : byte []) = Array.map2 (^^^) bts1 bts2

let hammingDistance (b1 : byte []) (b2 : byte []) =
    let getByteDiff (b1 : byte) (b2 : byte) =
        let mutable cnt = 0
        let mutable xor = b1 ^^^ b2
        for _ in 0..7 do
            if 1uy &&& xor = 1uy then cnt <- cnt + 1
            xor <- xor >>> 1
        cnt
    [0..b1.Length - 1] |> List.sumBy (fun idx -> getByteDiff b1.[idx] b2.[idx])

let keySize (bts : byte []) trials = 
    [2..40]
    |> List.map (fun ks ->
        let firstBlock = bts.[0..ks-1]
        let avgDistance = [1..trials] |> List.averageBy (fun t ->
            let block = bts.[ks * t .. ks * (t + 1) - 1]
            (double <| hammingDistance block firstBlock) / (double ks))
        ks, avgDistance)
    |> List.minBy snd
    |> fst

let transposeBlocks (bts : byte []) keySize =
    let getBlock n = 
        let sq = seq {
            let mutable pos = n
            while pos < bts.Length do
                yield bts.[pos]
                pos <- pos + keySize
        }
        sq |> Array.ofSeq
    List.map getBlock [0..keySize - 1]

let encryptRepeating (str : string) (key : string) =
    Seq.mapi (fun idx ch -> (byte ch) ^^^ (byte key.[idx % key.Length])) str
    |> Array.ofSeq
