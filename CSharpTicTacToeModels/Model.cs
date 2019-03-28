using System;
using System.Collections.Generic;
using Microsoft.FSharp.Core;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;

        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            game.Board.Add((move.Row, move.Col), game.Turn);
            if (game.Turn == Player.Cross)
            {
                game.Turn = Player.Nought;
            }
            else
            {
                game.Turn = Player.Cross;
            }
            return game;
        }
        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }
        public List<List<Move>> Lines(int size) {
            List<List<Move>> lines = new List<List<Move>>();

            for (int i = 0; i < size; i++)
            {
                List<Move> hor = new List<Move>();
                List<Move> ver = new List<Move>();
                for (int j = 0; j < size; j++)
                {
                    hor.Add(CreateMove(i, j));
                    ver.Add(CreateMove(j, i));
                }
                lines.Add(hor);
                lines.Add(ver);
            }

            List<Move> diag1 = new List<Move>();
            List<Move> diag2 = new List<Move>();
            for (int i = 0; i < size; i++)
            {
                diag1.Add(CreateMove(i, i));
                diag2.Add(CreateMove(size - 1 - i, i));
            }

            lines.Add(diag1);
            lines.Add(diag2);

            return lines;
        }
        public List<Move> GetPossibleMoves(Game game) {
            List<Move> moves = new List<Move>();

            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    Move cMove = CreateMove(i, j);
                    if (!game.Board.ContainsKey((i, j)))
                    {
                        moves.Add(cMove);
                    }
                }
            }

            return moves;
        }
        public TicTacToeOutcome<Player> CheckLine(Game game, List<Move> line)
        {
            List<Tuple<int, int>> winLine = new List<Tuple<int, int>>();
            string plays = "";
            foreach (var i in line)
            {
                plays += game.getPiece(i.Row, i.Col);
                winLine.Add(new Tuple<int, int>(i.Row, i.Col));
            }
            if (plays.Equals(new string('X', game.Size)))
            {
                return TicTacToeOutcome<Player>.NewWin(Cross, winLine);
            }
            else if (plays.Equals(new string('O', game.Size))) {
                return TicTacToeOutcome<Player>.NewWin(Nought, winLine);
            }
            else if (plays.Contains("O") && plays.Contains("X"))
            {
                return TicTacToeOutcome<Player>.Draw;
            }
            return TicTacToeOutcome<Player>.Undecided;
        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            var gameFilled = game.Board.Count >= game.Size * game.Size;
            TicTacToeOutcome<Player> ret = TicTacToeOutcome<Player>.Undecided;

            foreach (var line in Lines(game.Size)) {
                ret = CheckLine(game, line);

                if (ret.IsWin)
                {
                    break;
                }
                if (ret.IsDraw && gameFilled)
                {
                    break;
                }
            }

            if (ret.IsDraw && !gameFilled)
            {
                ret = TicTacToeOutcome<Player>.Undecided;
            }

            return ret;
        }
        public bool GameOver(Game game) {
            if (game.Board.Count >= game.Size * game.Size)
            {
                return true;
            }
            var outcome = GameOutcome(game);
            if (outcome.IsWin || outcome.IsDraw)
            {
                return true;
            }
            return false;
        }
        public int HeuristicScore (Game game, Player p) {
            TicTacToeOutcome<Player> outcome = GameOutcome(game);
            var win = outcome as TicTacToeOutcome<Player>.Win;
            if (outcome.IsDraw || outcome.IsUndecided)
            {
                return 0;
            }
            if (win.winner.Equals(p))
            {
                return 1;
            }
            return -1;
        }
        public Player GetTurn (Game game)
        {
            return game.Turn;
        }
        public Move FindBestMove(Game game)
        {
            return CreateMove(0, 0);
        }
        public Game GameStart(Player first, int size)
        {
            return new Game(first, size);
        }
    }
}