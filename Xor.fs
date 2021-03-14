module Xor

open System

let getFrequency ch = 
    let charFrequency =
        [('A', 0.0834); ('B', 0.0154); ('C', 0.0273); ('D', 0.0414); ('E', 0.1260);
         ('F', 0.0203); ('G', 0.0192); ('H', 0.0611); ('I', 0.0671); ('J', 0.0023);
         ('K', 0.0087); ('L', 0.0424); ('M', 0.0253); ('N', 0.0680); ('O', 0.0770);
         ('P', 0.0166); ('Q', 0.0009); ('R', 0.0568); ('S', 0.0611); ('T', 0.0937);
         ('U', 0.0285); ('V', 0.0106); ('W', 0.0234); ('X', 0.0020); ('Y', 0.0204);
         ('Z', 0.0006); (' ', 0.2000)]
        |> Map.ofList

    let upperChar = Char.ToUpper ch
    if Map.containsKey upperChar charFrequency then charFrequency.[upperChar]
    else float 0

let xor (bts1 : byte []) (bts2 : byte []) = 
    Array.map2 (^^^) bts1 bts2
