namespace QUT

    module GameTheory =
        open System.IO
        open System.IO
        open System.Diagnostics

        let MiniMaxGenerator (heuristic: 'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver: 'Game -> bool) (moveGenerator: 'Game -> seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax (game : 'Game) (perspective: 'Player) =
                NodeCounter.Increment()

                // Game over, get score
                if gameOver game then (None, heuristic game perspective) else 
                
                let moves: seq<'Move> = moveGenerator game
                let futureGames: seq<'Game> = Seq.map (fun (m: 'Move) -> applyMove game m) moves
                let futureScores: seq<Option<'Move> * int> = Seq.map (fun (g: 'Game) -> MiniMax g perspective) futureGames
                let movesWithFutureScores: seq<'Move * (Option<'Move> * int)> = Seq.zip moves futureScores

                // Max score (User's perspective)
                if getTurn game = perspective
                then
                    let bestMoveScore: ('Move * (Option<'Move> * int)) = Seq.maxBy (fun x -> snd x |> snd) movesWithFutureScores
                    (Some <| fst bestMoveScore, snd <| snd bestMoveScore)

                // Min score (Not User's perspective)
                else
                    let worseMoveScore: ('Move * (Option<'Move> * int)) = Seq.minBy (fun x -> snd x |> snd) movesWithFutureScores
                    (Some <| fst worseMoveScore, snd <| snd worseMoveScore)
                

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
                    let rec bestScore (acc: Option<'Move> * int) (ms: seq<'Move>) (rAlpha: int) =
                        if Seq.isEmpty ms then acc
                        else
                            // Extract stuff from gs
                            let curMove = Seq.head ms
                            let curGame = applyMove game curMove
                            
                            let newScore = MiniMax rAlpha beta curGame perspective |> snd

                            // Get new alpha and acc
                            let newAlpha = max rAlpha newScore
                            let newAcc = if newScore > rAlpha then (Some curMove, newScore) else acc

                            if newAlpha >= beta then newAcc
                            else
                                bestScore newAcc (Seq.skip 1 ms) newAlpha
                    
                    bestScore (None, alpha) moves alpha

                // Min score (Not User's perspective)
                else
                    let rec worseScore (acc: Option<'Move> * int) (ms: seq<'Move>) (rBeta: int) =
                        if Seq.isEmpty ms then acc
                        else
                            // Extract stuff from gs
                            let curMove = Seq.head ms
                            let curGame = applyMove game curMove
                            
                            let newScore = MiniMax alpha rBeta curGame perspective |> snd

                            // Get new beta and acc
                            let newBeta = min rBeta newScore
                            let newAcc = if newScore < rBeta then (Some curMove, newScore) else acc

                            if alpha >= newBeta then newAcc
                            else
                                worseScore newAcc (Seq.skip 1 ms) newBeta
                    
                    worseScore (None, beta) moves beta
                    
            NodeCounter.Reset()
            MiniMax
