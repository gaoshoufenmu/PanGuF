#nowarn "0040"
#nowran "0042"
namespace Foundation


module Adhoc =
    [<NoDynamicInvocation>]
    let inline retype (x: 'a) : 'b = (# "" x : 'b #)

    let inline multiplyByTwo (x: 'a) = x * (retype 2 : 'a)
