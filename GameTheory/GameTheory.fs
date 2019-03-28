namespace QUT

    module GameTheory =
        open System.IO
        open System.IO
        open System.Diagnostics
        open System.IO

        let MiniMaxGenerator (heuristic: 'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver: 'Game -> bool) (moveGenerator: 'Game -> seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax (game : 'Game) (perspective: 'Player) =
                NodeCounter.Increment()

                // Game over, get score
                if gameOver game
                then (None, heuristic game perspective)
                else  
                    let movesAndScores: seq<'Move * int> = 
                        game
                        |> moveGenerator
                        |> Seq.map (fun (m: 'Move) -> (m, MiniMax (applyMove game m) perspective |> snd))

                    // Max score (User's perspective)
                    if getTurn game = perspective
                    then
                        movesAndScores
                        |> Seq.maxBy snd
                        |> fun x -> (x |> fst |> Some, x |> snd)

                    // Min score (Not User's perspective)
                    else
                        movesAndScores
                        |> Seq.minBy snd
                        |> fun x -> (x |> fst |> Some, x |> snd)

            NodeCounter.Reset()
            MiniMax

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
 
                    let bestScorerWithPrune (acc: Option<'Move> * int) (move: 'Move): Option<'Move> * int =
                        let curAlpha = acc |> snd

                        // Prune (short circuting)
                        if curAlpha >= beta then acc
                        else
                            // Get new score
                            let newAlpha = MiniMax curAlpha beta (applyMove game move) perspective |> snd

                            if newAlpha > curAlpha then (Some move, newAlpha)
                            else acc

                    moves
                    |> Seq.fold bestScorerWithPrune (None, alpha)

                // Min score (Not User's perspective)
                else
                    let worseScorerWithPrune (acc: Option<'Move> * int) (move: 'Move): Option<'Move> * int =
                        let curBeta = acc |> snd

                        // Prune (short circuting)
                        if alpha >= curBeta then acc
                        else
                            // New score
                            let newBeta = MiniMax alpha curBeta (applyMove game move) perspective |> snd

                            if newBeta < curBeta then (Some move, newBeta)
                            else acc
                    
                    moves
                    |> Seq.fold worseScorerWithPrune (None, beta)
                    
            NodeCounter.Reset()
            MiniMax
