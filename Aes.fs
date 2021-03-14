module Aes

open System.IO
open System.Security.Cryptography

let decryptAes128ECB (encrypted : byte []) (key : byte []) =
    use aes = Aes.Create()
    aes.Mode <- CipherMode.ECB
    aes.BlockSize <- 128
    aes.Key <- key
    let dcp = aes.CreateDecryptor()
    use ms = new MemoryStream(encrypted)
    use cs  = new CryptoStream(ms, dcp, CryptoStreamMode.Read)
    use sr = new StreamReader(cs)
    sr.ReadToEnd()

let splitBlocks (bts : byte[]) =
    let mutable ptr = 0
    let mutable blocks : byte [] list = []
    while ptr < bts.Length do
        let offset = if ptr + 15 >= bts.Length then bts.Length else ptr + 15
        blocks <- (bts.[ptr..offset])::blocks
        ptr <- ptr + 16
    blocks
