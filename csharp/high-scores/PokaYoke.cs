namespace PokaYoke
{
    using System;
    using System.Collections.Generic;
    using System.Linq;

    public class HighScores
    {
        private List<int> ListOfScores { get; }
        public HighScores(List<int> list)
        {
            ListOfScores = list;
        }

        public List<int> Scores()
        {
            return ListOfScores;
        }

        public int Latest()
        {
            return ListOfScores.LastOrDefault();    // will return incorrect value after a call to PersonalTopThree
        }

        public int PersonalBest()
        {
            return ListOfScores.Max();
        }

        public List<int> PersonalTopThree()
        {
            ListOfScores.Sort();
            ListOfScores.Reverse();
            return ListOfScores.Take(3).ToList();
        }
    }}