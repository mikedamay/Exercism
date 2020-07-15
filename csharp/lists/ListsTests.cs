using Xunit;

public class ListsTests
{
    [Fact]
    public void NewList()
    {
        Assert.Empty(Languages.NewList());
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ExistingList()
    {
        Assert.Equal(new string[] {"C#", "Clojure", "Elm"}, Languages.GetExistingLanguages());
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void AddLanguage()
    {
        Assert.Equal(new string[] {"C#", "Clojure", "Elm", "Bash"},
            Languages.AddLanguage(Languages.GetExistingLanguages(), "Bash"));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ReverseList()
    {
        Assert.Equal(new string[] {"Elm", "Clojure", "C#"},
            Languages.ReverseList(Languages.GetExistingLanguages()));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ContainsStar_yes()
    {
        Assert.True(Languages.ContainsStar(Languages.GetExistingLanguages()));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ContainsStar_too_many()
    {
        var languages = Languages.GetExistingLanguages();
        languages.Insert(0, "VBA");
        Assert.False(Languages.ContainsStar(languages));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ContainsStar_empty()
    {
        var languages = Languages.NewList();
        Assert.False(Languages.ContainsStar(languages));
    }

    [Fact/*(Skip = "Remove this Skip property to run this test")*/]
    public void ContainsStar_single_star()
    {
        var languages = Languages.GetExistingLanguages();
        languages.RemoveAt(2);
        languages.RemoveAt(1);
        Assert.True(Languages.ContainsStar(languages));
    }
}