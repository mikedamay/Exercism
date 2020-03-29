using System;
using System.Linq;

public static class Store
{
    private static int[] processingRates = new int[] { 45, 30, 12 };
        
    public static int QueueWithMinimalWaitingTime(int customersInQueue1, int customersInQueue2, int customersInQueue3)
    {
        Enumerable.Min()
        return new int[] {customersInQueue1, customersInQueue2, customersInQueue3}
            .Select((c, idx) => (idx, c * processingRates[idx]))
            .OrderBy(c => c.Item2).Select(c => c.idx + 1).First();
    }
}
