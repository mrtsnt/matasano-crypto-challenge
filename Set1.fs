#if INTERACTIVE
#load "Decrypt.fs"
#load "Convert.fs"
#load "Aes.fs"
#load "Xor.fs"
#endif

module Set1

open System.IO

let challenge1 () =
    "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    |> Convert.hexToBytes
    |> Convert.base64Encode

let challenge2 () = 
    let inBytes = "1c0111001f010100061a024b53535009181c" |> Convert.hexToBytes
    let xorBytes = "686974207468652062756c6c277320657965" |> Convert.hexToBytes
    Xor.xor inBytes xorBytes |> Convert.bytesToHex

// Cooking MC's like a pound of bacon
let challenge3 () = 
    let bts = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" |> Convert.hexToBytes
    let possibilities = Xor.singleCharEncryptions bts |> List.map fst
    List.minBy Decrypt.calculateEnglishness possibilities

// Now that the party is jumping
let challenge4 () =
    let data = File.ReadAllLines("data/set1challenge4") 
    data 
    |> Array.collect (Convert.hexToBytes >> Xor.singleCharEncryptions >> Array.ofList)
    |> Array.map (fun xor -> (xor, Decrypt.calculateEnglishness (fst xor)))
    |> Array.sortBy snd

let challenge5 () =
    Xor.encryptRepeating """Burning 'em, if you ain't quick and nimble
I go crazy when I hear a cymbal""" "ICE" 
    |> Convert.bytesToHex

let challenge6 () = 
    let bts = File.ReadAllText("data/set1challenge6").Replace("\n", "") |> Convert.base64ToBytes
    let keySize = Xor.keySize bts 20

    Xor.transposeBlocks bts keySize 
    |> List.map (Xor.decryptSingleChar >> snd) 
    |> Array.ofList 
    |> Convert.bytesToAscii

let challenge7 () =
    let key = "YELLOW SUBMARINE" |> Convert.asciiToBytes
    let cipherBytes = File.ReadAllText("data/set1challenge7").Replace("\n", "") |> Convert.base64ToBytes
    Aes.decryptAes128ECB cipherBytes key

let challenge8 () = 
    let data = File.ReadAllLines("data/set1challenge8")
    Array.maxBy (fun arr ->
        let blockGroups = (Convert.hexToBytes >> Aes.splitBlocks 16) arr
        let distinctBlocks = blockGroups |> List.distinct
        List.length blockGroups - List.length distinctBlocks) data

challenge1 ()
challenge2 ()
challenge3 ()
challenge4 ()
challenge5 ()
challenge6 ()
challenge7 ()
challenge8 ()
