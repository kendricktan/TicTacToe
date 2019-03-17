namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => throw new System.NotImplementedException("getCross");
        public Player Nought => throw new System.NotImplementedException("getNought");
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            throw new System.NotImplementedException("ApplyMove");
        }
        public Move CreateMove(int row, int col)
        {
            throw new System.NotImplementedException("CreateMove");
        }
        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            throw new System.NotImplementedException("GameOutcome");
        }
        public Game GameStart(Player first, int size)
        {
            throw new System.NotImplementedException("GameStart");
        }
    }
}