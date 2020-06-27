namespace template
{
    
using System;
using System.Collections.Generic;

//**** please do not modify the FacialFeatures class ****
public class FacialFeatures
{
    public string EyeColor { get; set;  }
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
    public Identity GetAdmin()
    {
        throw new NotImplementedException("Please implement the Authenticator.GetAdmin() method");
    }

    public IDictionary<string, Identity> GetDevelopers()
    {
        throw new NotImplementedException("Please implement the Authenticator.GetDevelopers() method");
    }
}
}
