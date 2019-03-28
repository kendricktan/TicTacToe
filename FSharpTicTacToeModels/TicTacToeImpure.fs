
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
            let moves = seq { for x in 0 .. (gameState.Size - 1) do 
                                  for y in 0 .. (gameState.Size - 1) -> { Row=x; Col=y } }

            let mutable validMoves = Seq.empty

            for i in moves do
                validMoves <- if Map.containsKey i gameState.Board
                              then validMoves
                              else Seq.append validMoves (Seq.singleton i)
            
            validMoves

        let ApplyMove (game: GameState) (move: Move)  =
            game.Board <- Map.add move game.Turn game.Board
            game.Turn <- match game.Turn with
                            | Nought -> Cross
                            | Cross -> Nought
            game

        let RevertMove (game: GameState) (move: Move) = 
            game.Board <- Map.remove move game.Board
            game.Turn <- match game.Turn with
                            | Nought -> Cross
                            | Cross -> Nought
            game

        let CreateMove row col   = { Row=row; Col=col }


        let GameStart first size =
            { Turn=first; Size=size; Board=Map.empty }

        let GameOutcome game = 
            let outcomes = seq { for l in (Lines game.Size) -> CheckLine game l }

            let getWin = fun x -> match x with 
                                    | Win _ -> true
                                    | _ -> false
            
            let getDraw = fun x -> match x with
                                    | Draw -> true
                                    | _ -> false
            
            if Seq.exists getWin outcomes
            then Seq.filter getWin outcomes |> Seq.head
            else if Seq.forall getDraw outcomes
            then Draw
            else Undecided

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

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta game perspective =
                NodeCounter.Increment()

                // Game over, get score
                if gameOver game then (None, heuristic game perspective) else 
                
                let moves: seq<'Move> = moveGenerator game

                // Max score (User's perspective)
                if getTurn game = perspective
                then
                    let mutable acc = (None, alpha)

                    for m in moves do
                        let curAlpha = acc |> snd

                        acc <- if curAlpha >= beta
                               then acc
                               else
                                   // Get new score
                                   let newAlpha = MiniMax curAlpha beta (applyMove game m) perspective |> snd
                                   let _ = RevertMove game m

                                   if newAlpha > curAlpha
                                   then (Some m, newAlpha)
                                   else acc

                    acc

                // Min score (Not User's perspective)
                else
                    let mutable acc = (None, beta)

                    for m in moves do
                        let curBeta = acc |> snd

                        acc <- if alpha >= curBeta then acc
                               else
                                   // New score
                                   let newBeta = MiniMax alpha curBeta (applyMove game m) perspective |> snd
                                   let _ = RevertMove game m

                                   if newBeta < curBeta then (Some m, newBeta)
                                   else acc
                    
                    acc
                    
            NodeCounter.Reset()
            MiniMax
        
        let FindBestMove game    =
            let mm = MiniMaxWithAlphaBetaPruningGenerator HeuristicScore GetPlayer GameOver GetPossibleMoves ApplyMove
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