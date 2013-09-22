namespace Roots

module Roots =

    type RootNotFoundError(lower: float, upper: float, fLower: float, fUpper: float, iterations: int) =
        inherit System.Exception(sprintf "No root between f(%f) = %f and f(%f) = %f in %d iterations" lower fLower upper fUpper iterations)
        let lower = lower 
        let upper = upper 
        let fLower = fLower 
        let fUpper = fUpper
        let iterations = iterations

    let private updateBounds lower upper middle fLower fUpper fMiddle iteration =
        if fLower < 0.0 && fUpper > 0.0 then
            if fMiddle < 0.0 then (middle, upper, fMiddle, fUpper)
            else (lower, middle, fLower, fMiddle)
        else if fLower > 0.0 && fUpper < 0.0 then
            if fMiddle < 0.0 then (lower, middle, fLower, fMiddle)
            else (middle, upper, fMiddle, fUpper)
        else raise(new RootNotFoundError(lower, upper, fLower, fUpper, iteration))

    let bisectionDebug debug f lower upper accuracy =
        let rec bisectionCalc lower upper fLower fUpper iter =
            let midpoint = (lower+upper)/2.0 // calculate the midpoint
            let acc = (upper-lower)/2.0 // calculate the accuracy

            if acc < accuracy then midpoint // return the midpoint if the desired accuracy is achieved
            else
                let fMidpoint = f midpoint
                debug (iter,lower,upper,midpoint,fLower,fUpper,fMidpoint);
                if fMidpoint = 0.0 then midpoint
                else
                    let (newLower, newUpper, newFLower, newFUpper) =
                        updateBounds lower upper midpoint fLower fUpper fMidpoint iter

                    bisectionCalc newLower newUpper newFLower newFUpper (iter + 1)

        bisectionCalc lower upper (f lower) (f upper) 0

    let bisection = bisectionDebug (fun _ -> ())