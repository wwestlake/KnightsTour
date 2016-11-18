
module GeneticKnightsTour
open MathNet.Numerics.Distributions

let rec randDist max =
    let dist = Normal.Sample(0.0,1.0)
    let absdist = System.Math.Abs( dist )
    let result = (int)(System.Math.Floor(absdist * (float)max))
    if result >= max then (randDist max) else result

　
let offsets = [
                 (1,2); (2,1);
                 (1,-2); (2,-1);
                 (-1,2); (-2,1)
                 (-1,-2); (-2, -1)
               ]

let validMoves (x,y) = offsets |> List.map (fun (ox,oy) -> (ox+x, oy+y)) 

let board n = [|
                    for j in 1..n do
                        for k in 1..n do
                            yield (j,k)
              |]

　
let rand = new System.Random((int)System.DateTime.Now.Ticks)
let next n = rand.Next(n)

let shuffle n (b: array<'a>) =
    let rec innerShuffle n (b: array<'a>) =
        if n = 0 then b 
        else
            let idx = next (n-1)
            let value = b.[n]
            b.[n] <- b.[idx]
            b.[idx] <- value
            innerShuffle (n-1) b
    innerShuffle n b

// shuffle 16 (board 8)

　
let fitness board =
    let isValid h1 h2 =
        validMoves h1 |> List.exists ((=) h2)

    let rec innerFitness b (score: int) =
        match b with
        | [] -> score
        | head1 :: tail1 -> match tail1 with
                            | [] -> score
                            | head2 :: tail2 -> if isValid head1 head2 
                                                then innerFitness tail1 (score + 1) 
                                                else innerFitness tail1 score
    innerFitness board 0

let sort (l: ( (int * int) array * int) list) =
    l |> List.sortWith (fun (_,s1) (_,s2) -> s2.CompareTo(s1) )

let initPopulation size n =
    [
        for i in 1..size do
            let b = board n
            let c = shuffle 8 b
            let score = fitness (Array.toList c)
            yield (c, score)
    ] |> sort

　
let growPopulation pop board =
    (board :: pop) |>  sort

　
let pick2 (pop: (array<(int * int)> * int) list) =
    let n = List.length pop
    let p1 = randDist n
    let p2 = randDist n
    (pop.[p1], pop.[p2])

let breed2 ((child1: array<(int*int)>*int) , (child2: array<(int*int)> * int)) =
    let (c1,_) = child1
    let (c2,_) = child2
    let n = Array.length c1
    let splitPoint = next n
    let result = [| 
                        for i in 0..(splitPoint - 1) do
                            yield c1.[i]
                        for i in splitPoint..(n-1) do
                            yield c2.[i]
                 |] 
    (result, (Array.toList result) |> fitness) 

let breed pop =

    (pick2 pop) |> breed2 |> (growPopulation pop)

　
let test () =
    let rec inner pop =
        let (_,score) = List.head pop
        let len = List.length pop
        if (score = 63) then List.head pop
                        else
                            printfn "%d - %d\n%A" len score (List.head pop)
                            inner (breed pop)

　
    let pop1 = initPopulation 20 8
    inner pop1

test ()
