open System
open System.Threading


let row,col = 50,25

type CellState =
    | alive = 0
    | dead = 1
    | left = 2
    | right = 3
    | up = 4
    | down = 5

let next_state current_state alive_cells =
    match current_state with
        | CellState.alive -> if List.contains alive_cells [2..3] then CellState.alive else CellState.dead
        | CellState.dead -> if alive_cells = 3 then CellState.alive else CellState.dead
        | z -> z


let count predicate source = Seq.length <| Seq.filter predicate source

let update (u : CellState[,]) = 
    let initializer i j = 
        let u_ij = match u.[i,j] with
                    | CellState.alive -> 1 
                    | CellState.dead -> 0 
                    | _ -> -1 
        if u_ij = -1 then 0
        else count (fun x -> x = CellState.alive) (Seq.cast<CellState> u.[i-1..i+1,j-1..j+1]) - u_ij
    let q = Array2D.init<int> (col+2) (row+2) initializer 
    
    Array2D.init<CellState> (col+2) (row+2) (fun i j -> next_state u.[i,j] q.[i,j])
    

let make_grid (u : CellState[,]) =
    let mapping x =
        match x with
        | CellState.alive -> "+"
        | CellState.dead -> "-"
        | CellState.right -> "\n"
        | _ -> ""

    let list = Seq.map mapping <| Seq.cast<CellState> u
    Seq.fold (fun x y -> x + y) "" list


let rec game u =
    let grid = make_grid u
    Console.Clear()
    Console.Write grid
    Thread.Sleep 200
    game <| update u


[<EntryPoint>]
let main argv = 
    let initialize = 
        let r = Random()
        let initializer i j = 
            if i = 0 then CellState.up
            elif i = col+1 then CellState.down
            elif j = 0 then CellState.left
            elif j = row+1 then CellState.right
            elif r.Next 2 = 1 then CellState.alive
            else CellState.dead
        Array2D.init<CellState> (col+2) (row+2) initializer
    
    game initialize