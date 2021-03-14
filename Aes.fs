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
