using System;
using System.Collections.Generic;
using System.Linq;

public class HighScores
{
    public List<int> list;
    public List<int> orderedList;
    public HighScores(List<int> list)
    {
        this.list = list;
        orderedList = list.OrderByDescending(n => n).ToList();
    }

    public List<int> Scores()
    {
        return list;
    }

    public int Latest()
    {
        return list.Last();
    }

    public int PersonalBest()
    {
        return orderedList.First();
    }

    public List<int> PersonalTop()
    {
        var count = orderedList.Count(); 
        if(count >3){
            orderedList.RemoveRange(3,count-3);
        }
        return orderedList;
    }

}