#Roots

Implementation of root finding algorithms using Bisection method, Newton's method, and the Secant method.

##Usage

```fs
open NumericalAnalysis

let myFunction x = x*x - 1.0
let lowerBound = 0.0
let upperBound = 5.0
let accuracy = 1e-4

let bisectionRoot = 
  try
    Roots.bisection myFunction lowerBound upperBound accuracy
  with
    | :? Roots.RootNotFoundError as ex -> printfn "%s" ex.Message; raise(ex);

let myFunction' x = 2.0*x
let firstGuess = 10.0
let maximumIterations = 50

let newtonRoot =
  try
    Roots.newton myFunction myFunction' firstGuess accuracy maximumIterations
  with
    | :? Roots.ExhaustedIterationsError as ex -> printfn "%s" ex.Message; raise(ex);

let p1 = 9.9
let p0 = firstGuess

let secantRoot =
  try
    Roots.secant myFunction (p1,p0) accuracy maximumIterations
  with
    | :? Roots.ExhaustedIterationsError as ex -> printfn "%s" ex.Message; raise(ex);
```
