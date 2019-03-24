using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public Game(Player firstPlayer, int size) {
            Board = new Dictionary<Move, Player>();
            Size = size;
            Turn = firstPlayer;
        }

        public Dictionary<Move, Player> Board { set;  get; }

        public int Size { get; }
        public Player Turn { set;  get; }

        public string getPiece(int row, int col)
        {
            Move m = new Move(row, col);
            if (Board.ContainsKey(m)) {
                if (Board[m] == Player.Cross)
                {
                    return "X";
                }
                return "O";
            }

            return "";
        }
    }
}