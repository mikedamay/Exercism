using System.Collections.Generic;
using System.Linq;

public static class BookStore
{
    private static readonly IReadOnlyDictionary<int, decimal> discounts = new Dictionary<int, decimal>
    {
        {1, (decimal)0.0}, {2, (decimal)0.05}, {3, (decimal)0.1}, {4, (decimal)0.2}, {5, (decimal)0.25}
    };
        
    public static double Total(IEnumerable<int> books)
    {
        return (double)CalculateCost(books);
    }

    private static decimal CalculateCost(IEnumerable<int> uniquebooks)
    {
        IList<ISet<int>> piles = new List<ISet<int>>();
        foreach (var book in uniquebooks)
        {
            AddBookToPile(book, piles);
        }

        return piles.Sum(pile => SeriesCost(pile.Count));
    }

    private static void AddBookToPile(int book, IList<ISet<int>> piles)
    {
        var pile = piles.Where(p => !p.Contains(book))
            .FirstOrDefault(p => MarginalCost(p) 
                                 == piles.Where(p2 => !p2.Contains(book))
                                     .Min(p2 => MarginalCost(p2)));
        if (pile == null)
            piles.Add(new HashSet<int>{book});
        else
            pile.Add(book);
    }

    private static decimal MarginalCost(ISet<int> pile) =>
        SeriesCost(pile.Count + 1) - SeriesCost(pile.Count);
    
    private static decimal SeriesCost(int numBooks) =>
        numBooks * 8 * (1 - discounts[numBooks]);
}