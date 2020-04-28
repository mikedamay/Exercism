using System.Collections.Generic;

namespace Dictionaries_Code
{
    public static class Example
    {
        public static Dictionary<int, string> GetEmptyDiectionary()
        {
            return new Dictionary<int, string>();
        }

        public static Dictionary<int, string> GetExistingDictionary()
        {
            return new Dictionary<int, string>
            {
                {1, "United States of America"},
                {55, "Brazil"},
                {91, "India"}
            };
        }

        public static Dictionary<int, string> AddCountryToEmptyDictionary(int CountryCode, string CountryName)
        {
            return new Dictionary<int, string>() { { CountryCode, CountryName } };
        }

        public static Dictionary<int, string> AddCountryToExistingDictionary(
            Dictionary<int, string> existingDictiopnary, int countryCode, string CountryName)
        {
            existingDictiopnary[countryCode] = CountryName;
            return existingDictiopnary;
        }
    }
}