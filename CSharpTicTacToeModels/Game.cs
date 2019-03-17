using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size => throw new System.NotImplementedException("getSize");
        public Player Turn => throw new System.NotImplementedException("getTurn");
        public string getPiece(int row, int col)
        {
            throw new System.NotImplementedException("getPiece");
        }
    }
}