// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Roots.fs"
open Roots
open System

let f1 x = 6.0*(System.Math.Pow(System.Math.E,x) - x)
let f2 x = 6.5 + 3.0*x*x + 2.0*x*x*x
let f3 x = f1 x - f2 x

let printIterations (iteration, lower, upper, guess, fLower, fUpper, fGuess) = printfn "%d: Guess: f(%f) = %f | Lower: f(%f) = %f | Upper: f(%f) = %f" iteration guess fGuess lower fLower upper fUpper

f3 (Roots.bisectionDebug printIterations f3 -1.0 1.0 1e-4)