// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Roots.fs"
open Roots
open System

let pow x n = System.Math.Pow(x,n)
let E = System.Math.E

let f1 x = 6.0*(pow E x - x)
let f1' x = 6.0*(pow E x - 1.0)
let f2 x = 6.5 + 2.0*(1.5 + x)*x*x
let f2' x = 6.0*(1.0 + x)*x
let f3 x = f1 x - f2 x
let f3' x = f1' x - f2' x

let printResult f x = printfn "f(%f) = %f" x (f x)

///////////////////////////////////////////////////////////////
////////    TESTING BISECTION   ///////////////////////////////
///////////////////////////////////////////////////////////////

let printBisection (iteration, lower, upper, guess, fLower, fUpper, fGuess) =
    printfn "[a_%d, b_%d] = [%f, %f] | [f(a_%d), f(b_%d)] = [%f, %f]" iteration iteration lower upper iteration iteration fLower fUpper
    printfn "c_%d = %f | f(c_%d) = %f" iteration guess iteration fGuess

let bisection = Roots.bisectionDebug printBisection

bisection f3 -1.0 1.0 1e-4 |> printResult f3

///////////////////////////////////////////////////////////////
////////    TESTING NEWTON  ///////////////////////////////////
///////////////////////////////////////////////////////////////

let printNewton (iteration, p1, p0, fP1, fP0, f'Prior) = 
    printfn "p_%d = %f | f(p_%d) = %f" (iteration+1) p1 (iteration+1) fP1

let newton = Roots.newtonDebug printNewton

let p33 p0 =
    printf "\n\n"
    printfn "p_0 = %f | f(p_0) = %f" p0 (f3 p0)
    newton f3 f3' p0 1e-4 50 |> ignore

p33 1.0
p33 10.0
try
    p33 50.0
with
    | :? Roots.ExhaustedIterationsError as ex -> printfn "%s" ex.Message

///////////////////////////////////////////////////////////////
////////    TESTING SECANT  ///////////////////////////////////
///////////////////////////////////////////////////////////////

let printSecant (iteration, p2, p1, p0, fP2, fP1, fP0, f'Prior) =
    let currIter = iteration + 2
    printfn "p_%d = %f | f(p_%d) = %f" currIter p2 currIter fP2

let secant = Roots.secantDebug printSecant

let p34 p1 p0 =
    printf "\n\n"
    printfn "p_0 = %f | f(p_0) = %f" p0 (f3 p0)
    printfn "p_1 = %f | f(p_1) = %f" p1 (f3 p1)
    secant f3 (p1,p0) 1e-4 50 |> ignore

p34 0.99 1.0
p34 9.9 10.0
try
    p34 49.5 50.0
with
    | :? Roots.ExhaustedIterationsError as ex -> printfn "%s" ex.Message