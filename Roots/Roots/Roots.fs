namespace NumericalAnalysis

module Roots =

    type ValueEval = { value: float; eval: float }
    let evaluate f x = { value = x; eval = f x }

    // An exception type that is raised when it is not possible to find a root
    type RootNotFoundError(lower: ValueEval, upper: ValueEval, iterations: int) =
        inherit System.Exception(sprintf "No root between f(%f) = %f and f(%f) = %f in %d iterations" lower.value lower.eval upper.value upper.eval iterations)
        let lower = lower 
        let upper = upper
        let iterations = iterations

    // An exception type that is raised when the method used takes more iterations than the specified max iterations
    type ExhaustedIterationsError(guess: ValueEval, iterations: int) =
        inherit System.Exception(sprintf "No root found in %d iterations - Final guess: f(%f) = %f" iterations guess.value guess.eval)
        let guess = guess
        let iterations = iterations

    
    let (|Negative|Positive|Zero|) pair = if pair.eval < 0.0 then Negative else if pair.eval > 0.0 then Positive else Zero;

    let (|GuessLower|GuessUpper|Invalid|) (lower,upper,guess)
        = match (lower,upper,guess) with
          | ((Negative|Zero),(Positive|Zero),(Negative|Zero)) | ((Positive|Zero),(Negative|Zero),(Positive|Zero)) -> GuessLower
          | ((Positive|Zero),(Negative|Zero),(Negative|Zero)) | ((Negative|Zero),(Positive|Zero),(Positive|Zero)) -> GuessUpper
          | _ -> Invalid

    // Returns new bounds that reflect the values of lower bound, the upper bound, and the calculated guess value
    // Raises an exception if the bounds aren't on either side of the 
    let private updateBounds (lower: ValueEval) (upper: ValueEval) (guess: ValueEval) iteration =
        match (lower,upper,guess) with
        | GuessLower -> (guess,upper)
        | GuessUpper -> (lower,guess)
        | _ -> raise(new RootNotFoundError(lower, upper, iteration)) // both bounds are on the same side of 0

    // Searches for the roots of a function by bisecting a range with each iteration until the function
    // returns a value for the midpoint of that range within a certain accuracy from 0.
    let bisectionDebug debug f lower upper accuracy =
        let rec bisectionCalc lower upper iter =
            let midpoint = (lower.value+upper.value)/2.0 // calculate the midpoint
            let acc = (upper.value-lower.value)/2.0 // calculate the accuracy

            let mid = { value = midpoint; eval = f midpoint }
            debug (iter,lower,upper,mid); // <- for Numerical Analysis assignment

            if acc < accuracy || mid.eval = 0.0 then mid.value // return the midpoint if the desired accuracy is achieved
            else
                // Update the bounds based on current boundsd and calculations
                let (newLower, newUpper) = updateBounds lower upper mid iter

                // Recursively call bisectionCalc with new bounds
                bisectionCalc newLower newUpper (iter + 1)

        let (l, u) = (evaluate f lower,evaluate f upper)
        bisectionCalc l u 0

    let bisection = bisectionDebug (fun _ -> ())

    // Used for both Newton's method and Secant method
    let newtonGeneral debug f f' (p1,p0) accuracy maxIters =
        let rec newtonCalc root1 root0 iter =
            if iter = maxIters then raise(new ExhaustedIterationsError(root1, iter));
            let f'Prior = f' root1 root0 // f' takes both prior guesses to support secant method
            let p2 = root1.value - root1.eval / f'Prior // The next guess is calculated by drawing a tangent line to the previous guess and using the root of that line
            let root2 = (evaluate f p2)
            debug (iter, root2, root1, root0, f'Prior); // Mmmm.... toasty side effects - for the inspection... FOR THE INSPECTION
            if System.Math.Abs (root2.value - root1.value) < accuracy then root2.value // If the new guess falls within a certain accuracy, return that guess
            else newtonCalc root2 root1 (iter + 1) // Else recursive call as final statement for DAT TCO

        newtonCalc (evaluate f p1) (evaluate f p0) 0 // Initiate recursion with starting values

    // Newton's method uses the derivative function to more quickly discover the next root
    // It draws a tangent line to the current guess and uses the root of the line as the next guess
    // This repeats until a value within a certain accuracy is found
    let newtonDebug debug f f' p0 
        = newtonGeneral (fun (iter, root2, root1, _, f'Prior) -> debug(iter, root2, root1, f'Prior)) f (fun root1 _ -> f' root1.value) (p0,0.0)

    let newton = newtonDebug (fun _ -> ())

    // Like Newton's method, but rather than using the derivative of the function, a secant line
    // is drawn between the two prior values to get an approximation of the derivative. Usually
    // not as fast as Newton's, but doesn't require knowledge of the input function - nor does the
    // function need to differentiable
    let secantDebug debug f = newtonGeneral debug f (fun root1 root0 -> (root1.eval - root0.eval)/(root1.value - root0.value))

    let secant = secantDebug (fun _ -> ())