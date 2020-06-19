namespace template
{

    using System;

    public class FacialFeatures
    {
        public string EyeColor { get; }
        public decimal PhiltrumWidth { get; }

        public FacialFeatures(string eyeColor, decimal filtrumWidth)
        {
            EyeColor = eyeColor;
            PhiltrumWidth = filtrumWidth;
        }
        // TODO: implement equality and GetHashCode() methods
    }

    public class Identity
    {
        public string Email { get; }
        public FacialFeatures FacialFeatures { get; }

        public Identity(string email, FacialFeatures facialFeatures)
        {
            Email = email;
            FacialFeatures = facialFeatures;
        }
        // TODO: implement equality and GetHashCode() methods
    }

    public class Authenticator
    {
        public bool IsAdmin(Identity identity)
        {
            throw new NotImplementedException("Please implement the Authenticator.IsAdmin() method");
        }

        public bool Register(Identity identity)
        {
            throw new NotImplementedException("Please implement the Authenticator.Register() method");
        }

        public bool IsRegistered(Identity identity)
        {
            throw new NotImplementedException("Please implement the Authenticator.IsRegistered() method");
        }

        public static bool AreSameObject(Identity identityA, Identity identityB)
        {
            throw new NotImplementedException("Please implement the Authenticator.AreSameObject() method");
        }
    }
}