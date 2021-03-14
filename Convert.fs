module Convert

open System

let hexToBytes (str : string) = Convert.FromHexString(str)
let base64Encode bytes = Convert.ToBase64String(bytes)
let base64ToBytes str = Convert.FromBase64String(str)
let bytesToHex (bytes : byte []) = Convert.ToHexString(bytes)
let bytesToAscii (bytes : byte []) = Text.Encoding.ASCII.GetString(bytes)
let asciiToBytes (str : string) = Text.Encoding.ASCII.GetBytes(str)
