using Dictionaries_Code;
using Xunit;

public class DictionariesTest
{
    [Fact]
    public void Empty_dictionary()
    {
        Assert.Empty(Example.GetEmptyDiectionary());
    }
    [Fact(Skip = "")]
    public void Existing_dictionary()
    {
        var ed = Example.GetExistingDictionary();
        Assert.Equal(3, ed.Count);
        Assert.Equal("United States of America", ed[1]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
    [Fact(Skip = "")]
    public void Add_country_to_empty_dictionary()
    {
        var ed = Example.AddCountryToEmptyDictionary(44, "United Kingdom");
        Assert.Equal(1, ed.Count);
        Assert.Equal("United Kingdom", ed[44]);
    }
    [Fact(Skip = "")]
    public void Add_country_to_existing_dictionary()
    {
        var ed = Example.AddCountryToExistingDictionary(
            Example.GetExistingDictionary(), 44, "United Kingdom");
        Assert.Equal(4, ed.Count);
        Assert.Equal("United States of America", ed[1]);
        Assert.Equal("United Kingdom", ed[44]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
    [Fact(Skip = "")]
    public void Get_country_name_from_dictionary()
    {
        var countryName = Example.GetCountryNameFromDictionary(
            Example.GetExistingDictionary(), 55);
        Assert.Equal("Brazil", countryName);
    }
    [Fact(Skip = "")]
    public void Try_get_non_existent_country_name_from_dictionary()
    {
        var countryName = Example.GetCountryNameFromDictionary(
            Example.GetExistingDictionary(), 999);
        Assert.Equal(string.Empty, countryName);
    }
    [Fact(Skip = "")]
    public void Update_country_name_in_dictionary()
    {
        var ed = Example.UpdatgeDictionary(
            Example.GetExistingDictionary(), 1, "Les États-Unis");
        Assert.Equal(3, ed.Count);
        Assert.Equal("Les États-Unis", ed[1]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
    [Fact(Skip = "")]
    public void Try_to_update_country_name_in_dictionary_for_non_existent_country()
    {
        var ed = Example.UpdatgeDictionary(
            Example.GetExistingDictionary(), 999, "Newlands");
        Assert.Equal(3, ed.Count);
        Assert.Equal("United States of America", ed[1]);
        Assert.Equal("Brazil", ed[55]);
        Assert.Equal("India", ed[91]);
    }
}
