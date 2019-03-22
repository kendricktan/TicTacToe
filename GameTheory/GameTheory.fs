namespace QUT

    module GameTheory =

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
                    (Some <| fst bestMoveScore, snd bestMoveScore |> snd)

                // Min score (Not User's perspective)
                else
                    let worseMoveScore: ('Move * (Option<'Move> * int)) = Seq.minBy (fun x -> snd x |> snd) movesWithFutureScores
                    (Some <| fst worseMoveScore, snd worseMoveScore |> snd)
                

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
