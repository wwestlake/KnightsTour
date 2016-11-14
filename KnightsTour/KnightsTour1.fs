module KnightsTour1

let offsets = [|(-2,-1);(-2,1);(-1,-2);(-1,2);(1,-2);(1,2);(2,-1);(2,1)|];

let squareToPair sqr = 
    (sqr % 8, sqr / 8)
    
let pairToSquare (col, row) = 
    row * 8 + col

// Memoizing function taken from Don Syme (http://blogs.msdn.com/b/dsyme/archive/2007/05/31/a-sample-of-the-memoization-pattern-in-f.aspx)
let memoize f =
    let cache = ref Map.empty
    fun x ->
        match (!cache).TryFind(x) with
        | Some res -> res
        | None ->
             let res = f x
             cache := (!cache).Add(x,res)
             res

let getNextMoves square = 
    let (col, row) = squareToPair square
    offsets 
    |> Seq.map    (fun (colOff, rowOff) -> (col + colOff, row + rowOff))
    |> Seq.filter (fun (c, r) -> c >= 0 && c < 8 && r >= 0 && r < 8) // make sure we don't include squares out of the board
    |> Seq.map    (fun (c, r) -> pairToSquare (c, r))
    
let getNextMovesMemoized = memoize getNextMoves

let squareToBoard square = 
    1L <<< square

let squareToBoardMemoized = memoize squareToBoard

let getValidMoves square board =
    getNextMovesMemoized square 
    |> Seq.filter (fun sqr -> ((squareToBoardMemoized sqr) &&& board) = 0L)
    
// gets all valid moves from a particular square and board state sorted by moves which have less next possible moves
let getValidMovesSorted square board =
    getValidMoves square board
    |> Seq.sortBy (fun sqr -> (getValidMoves sqr board) |> Seq.length ) 

let nextMoves = getValidMovesSorted
let sqrToBoard = squareToBoardMemoized

let findPath square = 
    let board = sqrToBoard square
    let rec findPathRec brd sqr sequence = seq {
        match brd with 
            | -1L -> yield sequence
            |   _ -> for m in (nextMoves sqr brd) do yield! findPathRec (brd ||| (sqrToBoard m)) m m::sequence
    }
    
    findPathRec board square [square]

//let solution = findPath ((4,4) |> pairToSquare) |> Seq.take 1
//solution.Dump()