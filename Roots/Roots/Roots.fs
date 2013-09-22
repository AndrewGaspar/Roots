namespace Roots

module Roots =
    let bisection lower0 upper0 f =
        let bounds = (lower0,upper0);
        let (lower,upper) = bounds;
        (lower+upper)/2