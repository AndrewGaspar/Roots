namespace Roots

module Roots =

    // An exception type that is raised when it is not possible to find a root
    type RootNotFoundError(lower: float, upper: float, fLower: float, fUpper: float, iterations: int) =
        inherit System.Exception(sprintf "No root between f(%f) = %f and f(%f) = %f in %d iterations" lower fLower upper fUpper iterations)
        let lower = lower 
        let upper = upper 
        let fLower = fLower 
        let fUpper = fUpper
        let iterations = iterations

    // An exception type that is raised when the method used takes more iterations than the specified max iterations
    type ExhaustedIterationsError(guess: float, fGuess: float, iterations: int) =
        inherit System.Exception(sprintf "No root found in %d iterations - Final guess: f(%f) = %f" iterations guess fGuess)
        let guess = guess
        let fGuess = fGuess
        let iterations = iterations

    // Returns new bounds that reflect the values of lower bound, the upper bound, and the calculated guess value
    // Raises an exception if the bounds aren't on either side of the 
    let private updateBounds lower upper guess fLower fUpper fGuess iteration =
        if fLower <= 0.0 && fUpper >= 0.0 then // lower bound is negative and upper bound is positive
            if fGuess < 0.0 then (guess, upper, fGuess, fUpper) // guess is the new lower bound
            else (lower, guess, fLower, fGuess) // guess is the new upper bound
        else if fLower >= 0.0 && fUpper <= 0.0 then // lower bound is positive and upper bound is negative
            if fGuess < 0.0 then (lower, guess, fLower, fGuess) // guess is the new upper bound
            else (guess, upper, fGuess, fUpper) // guess is the new lower bound
        else raise(new RootNotFoundError(lower, upper, fLower, fUpper, iteration)) // both bounds are on the same side of 0

    // Searches for the roots of a function by bisecting a range with each iteration until the function
    // returns a value for the midpoint of that range within a certain accuracy from 0.
    let bisectionDebug debug f lower upper accuracy =
        let rec bisectionCalc lower upper fLower fUpper iter =
            let midpoint = (lower+upper)/2.0 // calculate the midpoint
            let acc = (upper-lower)/2.0 // calculate the accuracy

            let fMidpoint = f midpoint
            debug (iter,lower,upper,midpoint,fLower,fUpper,fMidpoint); // <- for Numerical Analysis assignment

            if acc < accuracy || fMidpoint = 0.0 then midpoint // return the midpoint if the desired accuracy is achieved
            else
                // Update the bounds based on current boundsd and calculations
                let (newLower, newUpper, newFLower, newFUpper) =
                    updateBounds lower upper midpoint fLower fUpper fMidpoint iter

                // Recursively call bisectionCalc with new bounds
                bisectionCalc newLower newUpper newFLower newFUpper (iter + 1)

        let (fLower, fUpper) = (f lower, f upper)
        // If the function evaluates the initial values on the same side of zero, immediately raise an exception
        if (fUpper < 0.0 && fLower < 0.0) || (fUpper > 0.0 && fLower > 0.0) then raise(new RootNotFoundError(lower, upper, fLower, fUpper, 0));
        // Initiate first call to recursive function bisectionCalc
        bisectionCalc lower upper fLower fUpper 0

    let bisection = bisectionDebug (fun _ -> ())

    // Used for both Newton's method and Secant method
    let newtonGeneral debug f f' (p1,p0) accuracy maxIters =
        let rec newtonCalc p1 p0 fP1 fP0 iter =
            if iter = maxIters then raise(new ExhaustedIterationsError(p1, f p1, iter));
            let f'Prior = f' p1 p0 fP1 fP0 // f' takes both prior guesses to support secant method
            let p2 = p1 - fP1 / f'Prior // The next guess is calculated by drawing a tangent line to the previous guess and using the root of that line
            let fP2 = f p2
            debug (iter, p2, p1, p0, fP2, fP1, fP0, f'Prior); // Mmmm.... toasty side effects - for the inspection... FOR THE INSPECTION
            if System.Math.Abs (p2 - p1) < accuracy then p2 // If the new guess falls within a certain accuracy, return that guess
            else newtonCalc p2 p1 fP2 fP1 (iter + 1) // Else recursive call as final statement for DAT TCO

        newtonCalc p1 p0 (f p1) (f p0) 0 // Initiate recursion with starting values

    // Newton's method uses the derivative function to more quickly discover the next root
    // It draws a tangent line to the current guess and uses the root of the line as the next guess
    // This repeats until a value within a certain accuracy is found
    let newtonDebug debug f f' p0 
        = newtonGeneral (fun (iter, p1, p0, _, fP1, fP0, _, f'Prior) -> debug(iter, p1, p0, fP1, fP0, f'Prior)) f (fun p1 _ _ _ -> f' p1) (p0,0.0)

    let newton = newtonDebug (fun _ -> ())

    // Like Newton's method, but rather than using the derivative of the function, a secant line
    // is drawn between the two prior values to get an approximation of the derivative. Usually
    // not as fast as Newton's, but doesn't require knowledge of the input function - nor does the
    // function need to differentiable
    let secantDebug debug f = newtonGeneral debug f (fun p1 p0 fP1 fP0 -> (fP1 - fP0)/(p1 - p0))

    let secant = secantDebug (fun _ -> ())