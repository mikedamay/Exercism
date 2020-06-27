namespace example
{
using System;
using System.Collections.Generic;

//**** please do not modify the FacialFeatures class ****
public class FacialFeatures
{
    public string EyeColor { get; set; }
    public decimal PhiltrumWidth { get; set; }

    public FacialFeatures()
    {
    }
}

//**** please do not modify the Identity class ****
public class Identity
{
    public string Email { get; set; }
    public FacialFeatures FacialFeatures { get; set; }
    public IList<string> NameAndAddress { get; set; }

    public Identity()
    {
    }
}

public class Authenticator
{
    public Identity Admin { get; } = new Identity
    {
        Email = "admin@exerc.ism",
        FacialFeatures = new FacialFeatures
        {
            EyeColor = "green",
            PhiltrumWidth = 0.9m
        },
        NameAndAddress = new List<string>{"Chanakya Niti Kautilya Arthashastra", "Plausible Address", "Mombai"}
    };

    public IDictionary<string, Identity> Developers { get; }
        = new Dictionary<string, Identity>
        {
            ["bert"] = new Identity
            {
                Email = "bert@exerc.ism",
                FacialFeatures = new FacialFeatures
                {
                    EyeColor = "green",
                    PhiltrumWidth = 0.9m
                },
                NameAndAddress = new List<string>{"Bertrand Meyer", "Avenue des Champs-Élysées", "Paris"}
            },

            ["anders"] = new Identity
            {
                Email = "anders@exerc.ism",
                FacialFeatures = new FacialFeatures
                {
                    EyeColor = "green",
                    PhiltrumWidth = 0.9m
                },
                NameAndAddress = new List<string>{"Anders Hejlsberg", "Plausible Address", "Redmond"}
            }
        };

    public void Foo()
    {
        var aa = new Identity{NameAndAddress = {"mike", "jon"}};
    }
}
}