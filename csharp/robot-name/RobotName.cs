using System;
using System.Collections.Generic;
using System.Text;

public class Robot
{
    private string name = null;
    private readonly Namer namer = new Namer();
    public string Name
    {
        get
        {
            if (name == null)
            {
                name = namer.NewName();
            }

            return name;
        }
    }

    public void Reset()
    {
        name = null;
    }


    private class Namer
    {
        private static Random generator = new Random((int)(DateTime.Now.Ticks % Int64.MaxValue));
        private static ISet<string> existingNames = new HashSet<string>();
        internal string NewName()
        {
            string candidateName;
            do
            {
                candidateName = GenerateCandidateName();
            } while (!IsUnique(candidateName));

            existingNames.Add(candidateName);
            return candidateName;
        }

        private string GenerateCandidateName()
        {
            var sb = new StringBuilder();
            sb.Append((char)(generator.Next() % 26 + 'A'));
            sb.Append((char)(generator.Next() % 26 + 'A'));
            sb.Append((generator.Next() % 1000).ToString("000"));
            return sb.ToString();
        }

        private bool IsUnique(string name) => !existingNames.Contains(name);
    }
}

