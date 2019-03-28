using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public Game(Player firstPlayer, int size) {
            Board = new Dictionary<(int, int), Player>();
            Size = size;
            Turn = firstPlayer;
        }

        public Dictionary<(int, int), Player> Board { set;  get; }

        public int Size { get; }
        public Player Turn { set;  get; }

        public string getPiece(int row, int col)
        {
            if (Board.ContainsKey((row, col))) {
                if (Board[(row, col)] == Player.Cross)
                {
                    return "X";
                }
                return "O";
            }

            return "";
        }
    }
}