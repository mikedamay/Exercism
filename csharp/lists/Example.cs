using System.Collections.Generic;

public static class Languages
{
    public static List<string> NewList()
    {
        return new List<string>();
    }

    public static List<string> GetExistingLanguages()
    {
        var languages = new List<string>();
        languages.Add("C#");
        languages.Add("Clojure");
        languages.Add("Elm");
        return languages;
    }

    public static List<string> AddLanguage(List<string> languages, string language)
    {
        languages.Add(language);
        return languages;
    }

    public static int CountLanguages(List<string> languages)
    {
        return languages.Count;
    }

    public static List<string> ReverseList(List<string> languages)
    {
        languages.Reverse();
        return languages;
    }

    public static bool Match(List<string> languages)
    {
        if (languages.Count > 0)
        {
            if (languages[0] == "C#")
            {
                return true;
            }
            if (languages.Count > 1 && languages.Count < 4 && languages[1] == "C#")
            {
                return true;
            }
        }

        return false;
    }
}