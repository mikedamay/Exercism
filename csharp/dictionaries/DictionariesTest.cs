using Dictionaries_Code;
using Xunit;

public class DictionariesTest
{
    [Fact]
    public void TestEmptyDictionary()
    {
        Assert.Empty(Example.GetEmptyDiectionary());
    }
    [Fact(Skip = "")]
    public void TestExistingDictionary()
    {
        var ed = Example.GetExistingDictionary();
        Assert.Equal(3, ed.Count);
        Assert.Equal("United States of America", ed[1]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
    [Fact(Skip = "")]
    public void TestAddCountryToEmptyDictionary()
    {
        var ed = Example.AddCountryToEmptyDictionary(44, "United Kingdom");
        Assert.Equal(1, ed.Count);
        Assert.Equal("United Kingdom", ed[44]);
    }
    [Fact(Skip = "")]
    public void TestAddCountryToExistingDictionary()
    {
        var ed = Example.AddCountryToExistingDictionary(
            Example.GetExistingDictionary(), 44, "United Kingdom");
        Assert.Equal(4, ed.Count);
        Assert.Equal("United States of America", ed[1]);
        Assert.Equal("United Kingdom", ed[44]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
}
