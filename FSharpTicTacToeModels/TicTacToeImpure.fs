namespace QUT

    module FSharpImpureTicTacToeModel =
        open System
    
        type Player = Cross | Nought

        type Move = 
            { mutable Row: int; mutable Col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Col

        type GameState = 
            { mutable Turn: Player; mutable Size: int; mutable Board: Map<Move, Player> }
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

        let Lines (size:int) : seq<seq<int*int>> =
            let mutable s: seq<seq<int * int>> = Seq.empty
            let size2: int = size - 1
            let hor (r: int): array<(int*int)> = [| for c in 0 .. size2 -> (r, c) |]  // Horizontal line helper
            let ver (c: int): array<(int*int)> = [| for r in 0 .. size2 -> (r, c) |]  // Vertical line helper

            for i in seq { for (i: int) in 0 .. size2 -> seq (hor i) } do
                s <- Seq.append s (Seq.singleton i)

            for i in seq { for i in 0 .. size2 -> seq (ver i) } do
                s <- Seq.append s (Seq.singleton i)

            s <- Seq.append s (Seq.singleton (seq [| for (i: int) in 0 .. size2 -> (i, i) |]))
            s <- Seq.append s (Seq.singleton (seq [| for i in 0 .. size2 -> (size2 - i, i) |]))

            seq s

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

        let ApplyMove (game: GameState) (move: Move)  =
            game.Board <- Map.add move game.Turn game.Board
            game.Turn <- match game.Turn with
                            | Nought -> Cross
                            | Cross -> Nought
            game

        let CreateMove row col   = { Row=row; Col=col }


        let GameStart first size =
            { Turn=first; Size=size; Board=Map.empty }

        let GameOutcome game     = 
            let gameFilled = Map.count game.Board >= game.Size * game.Size
            let mutable ret = Undecided

            try
                for line in (Lines game.Size) do
                    ret <- CheckLine game line

                    // Throw error when complete
                    // Break loop
                    ignore <| match (ret, gameFilled) with
                                | Win _, _ -> 0 / 0
                                | Draw, true -> 0 / 0
                                | _ -> 0
                        
            with
                | _ -> ()
            
            ret

        let GameOver (game: GameState): bool =
            if Map.count game.Board >= game.Size * game.Size then true
            else
                match GameOutcome game with
                    | Win _ -> true
                    | Draw  -> true
                    | _ -> false

        let HeuristicScore (game: GameState) (player: Player): int =
            match GameOutcome game with
                | Win (winner, _) -> if winner = player then 1 else -1
                | _ -> 0

        let GetPlayer (game: GameState) = game.Turn
        
        // plus other helper functions ...
        let FindBestMove game    =
            let mm = GameTheory.MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetPlayer GameOver GetPossibleMoves ApplyMove
            let oMove = fst <| mm -1 1 game game.Turn
            match oMove with
                    | Some m -> m
                    | None -> { Row = -1; Col = -1 }



        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game