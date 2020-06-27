using System;
using Xunit;
using example;

public class ObjectInitializationTests
{
    [Fact]
    public void GetAdmin()
    {
        var authenticator = new Authenticator();
        var admin = authenticator.Admin;
        string[] actual =
        {
            admin.Email,
            admin.FacialFeatures.EyeColor,
            admin.FacialFeatures.PhiltrumWidth.ToString(),
            admin.NameAndAddress[0]
        };
        string[] expected =
        {
            "admin@exerc.ism", 
            "green", 
            "0.9", 
            "Chanakya Niti Kautilya Arthashastra"
        };
        Assert.Equal(expected, actual);
    }

    [Fact /*(Skip = "Remove this Skip property to run this test")*/]
    public void GetDevelopers()
    {
        var authenticator = new Authenticator();
        var developers = authenticator.Developers;
        string[] actual =
        {
            developers["bert"].Email,
            developers["bert"].FacialFeatures.EyeColor,
            developers["anders"].FacialFeatures.PhiltrumWidth.ToString(),
            developers["anders"].NameAndAddress[1]
        };
        string[] expected =
        {
            "bert@exerc.ism",
            "green",
            "0.9",
            "Plausible Address"
        };
        Assert.Equal(expected, actual);
    }
}