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

/*
I hold the opinion (I did say the comments were for your consideration - which of course includes ignoring them) that you should favour expressive code over non-expressive code and that this applies at all levels, other things being equal.

Expressiveness:

One aspect of expression is the relationship of the code to the business rules.  In this case we have been told to generate 2 letters and 3 numbers. If the maintainer sees 2 lines of code relating to letters followed by 3 lines of code relating to numbers they won’t even stop to consider it.  With your (perfectly good - but not meeting my expressiveness criterion - solution) the number 5 is involved which is good but then there is some question of a special condition of less than 2 that they will have to stop and parse.  They also have to recollect their ascii codes to parse the rest of it.

Something like:
```
  newName += (char)rnd.Next(‘A’, ‘Z’);
  newName += (char)rnd.Next(‘A’, ‘Z’);
  newName += (char)rnd.Next(‘0’, ‘9’);
  newName += (char)rnd.Next(‘0’, ‘9’);
  newName += (char)rnd.Next(‘0’, ‘9’);
```
is worth considering.

At all Levels:

Conventional wisdom is that variable and member names should be expressive and that operations can be abstracted in a well named method.  This is, of course, true but I see no reason not to extend this to implementation details. 

Other Things Being Equal:

On the other side of the equation is the amount of reading and (to a lesser extent) writing that coders have to do.  If the code becomes too verbose or noisy then it ceases to be expressive and can become burdensome.

Interesting code is written once but read many times.
 */
