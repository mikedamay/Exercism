using System.Collections.Generic;
using System.Linq;

public enum Plant
{
    Violets,
    Radishes,
    Clover,
    Grass
}

public class KindergartenGarden
{
    private readonly IReadOnlyDictionary<string, int> children
        = new string[]
        {
            "Alice", "Bob", "Charlie", "David",
            "Eve", "Fred", "Ginny", "Harriet",
            "Ileana", "Joseph", "Kincaid", "Larry"
        }.Select((child, idx) => (child, idx)).ToDictionary(c => c.Item1, c => c.Item2);

    private readonly IReadOnlyDictionary<char, Plant> seedMap = new Dictionary<char, Plant>
    {
        { 'V', Plant.Violets},
        { 'R', Plant.Radishes},
        { 'C', Plant.Clover},
        { 'G', Plant.Grass},
    };
    private readonly string[] diagram;
    
    
    public KindergartenGarden(string diagram)
    {
        this.diagram = diagram.Split("\n");
    }

    public IEnumerable<Plant> Plants(string student)
    {
        if (!children.ContainsKey(student)) return null;
        return diagram[0].Skip(children[student] * 2).Take(2)
            .Concat(diagram[1].Skip(children[student] * 2).Take(2))
            .Select(s => seedMap[s]);
    }
}