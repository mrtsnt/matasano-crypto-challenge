#if INTERACTIVE
#load "Convert.fs"
#endif

module Set1

open System
open System.IO


let challenge1 () =
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    |> Convert.hexToBytes
    |> Convert.base64Encode
    |> printfn "%s"

let challenge2 () = 
    let inBytes = "1c0111001f010100061a024b53535009181c" |> Convert.hexToBytes
    let xorBytes = "686974207468652062756c6c277320657965" |> Convert.hexToBytes
    [0..inBytes.Length - 1]
    |> List.map (fun idx -> inBytes.[idx] ^^^ xorBytes.[idx])
    |> Array.ofList
    |> Convert.bytesToHex
    |> printfn "%s"

let getFrequency ch = 
    let charFrequency =
        [('A', 8.34); ('B', 1.54); ('C', 2.73); ('D', 4.14); ('E', 12.60);
         ('F', 2.03); ('G', 1.92); ('H', 6.11); ('I', 6.71); ('J', 0.23);
         ('K', 0.87); ('L', 4.24); ('M', 2.53); ('N', 6.80); ('O', 7.70);
         ('P', 1.66); ('Q', 0.09); ('R', 5.68); ('S', 6.11); ('T', 9.37);
         ('U', 2.85); ('V', 1.06); ('W', 2.34); ('X', 0.20); ('Y', 2.04);
         ('Z', 0.06)]
        |> Map.ofList

    let upperChar = Char.ToUpper ch
    if Map.containsKey upperChar charFrequency then charFrequency.[upperChar]
    else float 0

let calcDistance (str : string) =
    Seq.averageBy (fun (ch, ocs) -> 
        let actual = (Seq.length ocs |> double) / double str.Length
        let expected = getFrequency ch // calc frequency for chars only !!!!!
        (actual - expected) |> abs) (str |> Seq.groupBy id)

let getXors str =
    let inBytes = str |> Convert.hexToBytes
    List.map (fun ch -> inBytes |> Array.map (fun el -> el ^^^ ch)) [32uy..126uy]
    |> List.filter (fun arr -> Array.forall (fun ch -> (ch <= 127uy && ch >= 32uy) || ch = 10uy) arr)
    |> List.map Convert.bytesToAscii

// Cooking MC's like a pound of bacon ?
let challenge3 () = 
    "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" 
    |> getXors
    |> List.map (fun xor -> (xor, calcDistance xor))
    |> List.sortBy snd
    |> List.take 15

// Now that the party is jumping ?
let challenge4 () =
    File.ReadAllLines("data/set1challange4") 
    |> Array.map (fun s -> s.Trim())
    |> Array.collect (getXors >> Array.ofList)
    |> Array.map (fun xor -> (xor, calcDistance xor))
    |> Array.sortBy snd
    |> Array.take 15

let encryptRepeatingXor (str : string) (key : string) =
    str 
    |> Seq.mapi (fun idx ch -> (byte ch) ^^^ (byte key.[idx % key.Length])) 
    |> Array.ofSeq

let challenge5 () =
    encryptRepeatingXor """Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal""" "ICE" 
    |> Convert.bytesToHex

let getHammingDistance s1 s2 =
    let getByteDiff (b1 : byte) (b2 : byte) =
        let mutable cnt = 0
        let mutable xor = b1 ^^^ b2
        for _ in 0..7 do
            if 1uy &&& xor = 1uy then cnt <- cnt + 1
            xor <- xor >>> 1
        cnt

    let b1 = Convert.asciiToBytes s1
    let b2 = Convert.asciiToBytes s2
    [0..b1.Length - 1] |> List.sumBy (fun idx -> getByteDiff b1.[idx] b2.[idx])
