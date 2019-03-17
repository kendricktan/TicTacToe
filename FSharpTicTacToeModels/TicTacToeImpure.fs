namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Something (* implement type *)

        type GameState = 
            { something: int (* implement type *) } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = raise (System.NotImplementedException("getTurn"))
                member this.Size with get()    = raise (System.NotImplementedException("getSize"))
                member this.getPiece(row, col) = raise (System.NotImplementedException("getPiece"))

        type Move = 
            { something: int (* implement type *) }
            interface ITicTacToeMove with
                member this.Row with get() = raise (System.NotImplementedException("getRow"))
                member this.Col with get() = raise (System.NotImplementedException("getCol"))



        let GameOutcome game     = raise (System.NotImplementedException("GameOutcome"))

        let ApplyMove game move  = raise (System.NotImplementedException("ApplyMove"))

        let CreateMove row col   = raise (System.NotImplementedException("CreateMove"))

        let FindBestMove game    = raise (System.NotImplementedException("FindBestMove"))

        let GameStart first size = raise (System.NotImplementedException("GameStart"))

        // plus other helper functions ...




        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = raise (System.NotImplementedException("getCross"))
                member this.Nought with get()            = raise (System.NotImplementedException("getNought"))
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game