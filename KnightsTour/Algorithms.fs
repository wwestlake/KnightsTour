module Algorithms

let combinations list1 list2 =
    seq {
        for e1 in list1 do
            for e2 in list2 do
                yield (e1, e2)
    }





