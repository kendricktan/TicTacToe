namespace QUT

    module FSharpPureTicTacToeModel =
        open System.Xml.Xsl
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { Row: int; Col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { Turn: Player; Size: int; Board: Map<Move, Player> }

            member this.getPiece(row, col) = (this :> ITicTacToeGame<Player>).getPiece(row, col)

            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = 
                    let key = { Row = row; Col = col }
                    if Map.containsKey key this.Board
                    then match Map.find { Row = row; Col = col } this.Board with
                            | Nought -> "O"
                            | Cross -> "X"
                    else ""


        let CreateMove row col = { Row = row; Col = col }

        let ApplyMove (oldState:GameState) (move: Move) =
            let newTurn = match oldState.Turn with
                            | Nought -> Cross
                            | Cross -> Nought
            let newBoard = Map.add move oldState.Turn oldState.Board
            { Turn = newTurn; Size = oldState.Size; Board = newBoard }

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [
        //          seq[(0,0);(0,1)];
        //          seq[(1,0);(1,1)];
        //          seq[(0,0);(1,0)];
        //          seq[(0,1);(1,1)];
        //          seq[(0,0);(1,1)];
        //          seq[(0,1);(1,0)]
        //          ]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> =
            let size2 = size - 1
            let hor (r: int): array<(int*int)> = [| for c in 0 .. size2 -> (r, c) |]  // Horizontal line helper
            let ver (c: int): array<(int*int)> = [| for r in 0 .. size2 -> (r, c) |]  // Vertical line helper

            let hors: seq<seq<int*int>> = seq { for i in 0 .. size2 -> seq (hor i) }
            let vers: seq<seq<int*int>> = seq { for i in 0 .. size2 -> seq (ver i) }

            let diag1: seq<int*int> = seq [| for i in 0 .. size2 -> (i, i) |]  // Diagonal Line 1
            let diag2: seq<int*int> = seq [| for i in 0 .. size2 -> (size2 - i, i) |] //Diagonal Line 2
            let diags: seq<seq<int*int>> = seq [| diag1; diag2 |]

            Array.fold Seq.append Seq.empty [| hors; vers; diags |]
            

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> =
            let pieces = Seq.map (fun x -> game.getPiece(fst x, snd x)) line

            if Seq.forall (fun x -> x = "O") pieces
            then Win(Nought, line)
            else if Seq.forall (fun x -> x = "X") pieces
            then Win(Cross, line)
            else if (Seq.contains "X" pieces && Seq.contains "O" pieces)
            then Draw
            else Undecided

        let GetPossibleMoves (gameState: GameState): seq<Move> =
            seq { for x in 0 .. (gameState.Size - 1) do for y in 0 .. (gameState.Size - 1) -> { Row=x; Col=y } }
            |> Seq.fold (fun acc (k: Move) -> if Map.containsKey k gameState.Board then acc else Seq.append acc (Seq.singleton k)) Seq.empty

        let GameOutcome (game: GameState): TicTacToeOutcome<Player> =
            let gameFilled = Map.count game.Board >= game.Size * game.Size

            let gameOutcomeReducer (acc: TicTacToeOutcome<Player>) (line: seq<int*int>): TicTacToeOutcome<Player> =
                match acc with
                    | Win _ -> acc
                    | _ -> CheckLine game line
            
            Lines game.Size
            |> Seq.fold gameOutcomeReducer Undecided
            |> fun x -> match (x, gameFilled) with
                            | Win _, _ -> x
                            | Draw, false -> Undecided
                            | Draw, true -> Draw
                            | _, _ -> Undecided
                            

        let GameOver (game: GameState): bool =
            if Map.count game.Board >= game.Size * game.Size then true
            else
                match GameOutcome game with
                    | Win _ -> true
                    | _ -> false

        let HeuristicScore (game: GameState) (player: Player): int =
            match GameOutcome game with
                | Win (winner, _) -> if winner = player then 1 else -1
                | _ -> 0

        let GetPlayer (game: GameState) = game.Turn

        let GameStart (firstPlayer: Player) size: GameState = { Turn=firstPlayer; Size=size; Board=Map.empty }

        let MiniMax game = 
            let mm = GameTheory.MiniMaxGenerator HeuristicScore GetPlayer GameOver GetPossibleMoves ApplyMove
            mm game game.Turn

        let MiniMaxWithPruning game =
            let mm = GameTheory.MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetPlayer GameOver GetPossibleMoves ApplyMove
            mm -1 1 game game.Turn

        // plus other helper functions 

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = 
                let oMove = fst <| MiniMax game
                match oMove with
                    | Some m -> m
                    | None -> { Row = -1; Col = -1 }


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = 
                let oMove = fst <| MiniMaxWithPruning game
                match oMove with
                    | Some m -> m
                    | None -> { Row = -1; Col = -1 }