using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography.X509Certificates;
using System.Xml.Schema;
using System.Xml.Serialization;
using Microsoft.VisualStudio.TestPlatform.ObjectModel.Client;
using Xunit.Sdk;

internal struct Color {public const byte Red = 1 << 0, Green = 1 << 1, Ivory = 1 << 2, Yellow = 1 << 3, Blue = 1 << 4, All = 0x1f;}
internal struct Nationality {public const byte Englishman = 1 << 0, Spaniard = 1 << 1, Ukranian = 1 << 2, Japanese = 1 << 3, Norwegian = 1 << 4, All = 0x1f;}
internal struct Pet {public const byte Dog = 1 << 0, Snails = 1 << 1, Fox = 1 << 2, Horse = 1 << 3, Zebra = 1 << 4, All = 0x1f;}
internal struct Drink {public const byte Coffee = 1 << 0, Tea = 1 << 1, Milk = 1 << 2, OrangeJuice = 1 << 3, Water = 1 << 4, All = 0x1f;}
internal struct Smoke {public const byte OldGold = 1 << 0, Kools = 1 << 1, Chesterfields = 1 << 2, LuckyStrike = 1 << 3, Parliaments = 1 << 4, All = 0x1f;}
internal struct Position {public const byte One = 1 << 0, Two = 1 << 1, Three = 1 << 2, Four = 1 << 3, Five = 1 << 4, All = 0x1f;}
internal struct AttributeType {public const byte Color = 0, Nationality = 1, Pet = 2, Drink = 3, Smoke = 4, Position = 5;}


internal enum Relation {Direct, ToRightOf, NextTo}

internal class Clue : IComparable
{
    public byte[] attributes = new byte[AttributeType.Position + 1];

    public Clue Clone()
    {
        Clue other = new Clue();
        Array.Copy(this.attributes, other.attributes, this.attributes.Length);
        return other;
    }

    public int CompareTo(object obj)
    {
        if (obj == null)
        {
            return 1;
        }

        Clue other = obj as Clue;
        MyDebug.Assert(this.attributes.Length == other.attributes.Length);
        for (int ii = 0; ii < this.attributes.Length; ii++)
        {
            if (this.attributes[ii] < other.attributes[ii])
            {
                return -1;
            }
            else if (this.attributes[ii] > other.attributes[ii])
            {
                return 1;
            }
        }
        return 0;
    }

    protected bool Equals(Clue other)
    {
        MyDebug.Assert(this.attributes.Length == other.attributes.Length);
        for (int ii = 0; ii < this.attributes.Length; ii++)
        {
            if (this.attributes[ii] != other.attributes[ii])
            {
                return false;
            }
        }

        return true;
    }

    public override bool Equals(object obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Clue) obj);
    }

    public override int GetHashCode()
    {
        int hashCode = this.attributes[0];
        for (int ii = 1; ii < this.attributes.Length; ii++)
        {
            hashCode = (hashCode * 397) ^ this.attributes[ii];
        }

        return hashCode;
    }
}

internal class Neighbour
{
    public byte attributeTypeA;
    public byte attrivubuteValueA;
    public byte attributeTypeB;
    public byte attrivubuteValueB;
    public Relation Relation { get; set; }

    public static ISet<Neighbour> neighbours = new HashSet<Neighbour>
    {
        new Neighbour{attributeTypeA = AttributeType.Color, attrivubuteValueA = Color.Green
          ,attributeTypeB = AttributeType.Color, attrivubuteValueB = Color.Ivory
          ,Relation = global::Relation.ToRightOf},
        new Neighbour{attributeTypeA = AttributeType.Pet, attrivubuteValueA = Pet.Fox
          ,attributeTypeB = AttributeType.Smoke, attrivubuteValueB = Smoke.Chesterfields
          ,Relation = global::Relation.NextTo},
        new Neighbour{attributeTypeA = AttributeType.Pet, attrivubuteValueA = Pet.Horse
          ,attributeTypeB = AttributeType.Smoke, attrivubuteValueB = Smoke.Kools
          ,Relation = global::Relation.NextTo},
        new Neighbour{attributeTypeA = AttributeType.Color, attrivubuteValueA = Color.Blue
          ,attributeTypeB = AttributeType.Nationality, attrivubuteValueB = Nationality.Norwegian
          ,Relation = global::Relation.NextTo},
    };
}

public static class ZebraPuzzle
{
    private static ISet<Clue> GivenClues = new HashSet<Clue>
    {
        new Clue{attributes = new byte[]{Color.Red, Nationality.Englishman, Pet.All, Drink.All, Smoke.All, Position.All}},
        new Clue{attributes = new byte[]{Color.All, Nationality.Spaniard, Pet.Dog, Drink.All, Smoke.All, Position.All}},
        new Clue{attributes = new byte[]{Color.Green, Nationality.All, Pet.All, Drink.Coffee, Smoke.All, Position.All}},
        // green house is to the right of the ivory house
        // the ivory house cannot be in position 5
        // the green house cannot be in position 1
        new Clue{attributes = new byte[]{Color.All, Nationality.Ukranian, Pet.All, Drink.Tea, Smoke.All, Position.All}},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Snails, Drink.All, Smoke.OldGold, Position.All}},
        new Clue{attributes = new byte[]{Color.Yellow, Nationality.All, Pet.All, Drink.All, Smoke.Kools, Position.All}},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.All, Drink.Milk, Smoke.All, Position.Three}},
        new Clue{attributes = new byte[]{Color.All, Nationality.Norwegian, Pet.All, Drink.All, Smoke.All, Position.One}},
//        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Fox, Drink.All, Smoke.Chesterfields, Position.All}, relation = Relation.NextTo},
//        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Horse, Drink.All, Smoke.Kools, Position.All}, relation = Relation.NextTo},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.All, Drink.OrangeJuice, Smoke.LuckyStrike, Position.All}},
        new Clue{attributes = new byte[]{Color.All, Nationality.Japanese, Pet.All, Drink.All, Smoke.Parliaments, Position.All}},
//        new Clue{attributes = new byte[]{Color.Blue, Nationality.Norwegian, Pet.All, Drink.All, Smoke.All, Position.All}, relation = Relation.NextTo},
    };
 
    
    private static int CountAttributeBits(byte b) => (b & 0x1) + (b >> 1 & 0x1) + (b >> 2 & 0x1) + (b >> 3 & 0x1) + (b >> 4 & 0x1);
    private static bool IsResolvedAttribute(byte b) => CountAttributeBits(b) == 1;

    private static byte TurnOffBit(byte bitmap, byte bit ) =>
        (byte) (bitmap & ~bit);
    
    static ZebraPuzzle()
    {
        MyDebug.Assert(
          Color.All == Nationality.All
          && Color.All == Pet.All
          && Color.All == Drink.All
          && Color.All == Smoke.All
          && Color.All == Position.All
            );
    }


    public static byte DrinksWater()
    {
        ISet<Clue> initialClues = FillInBlankClues(GivenClues);
        var initialClues2 = EliminateNeighbours(initialClues);
        Infer(initialClues2);
        return Nationality.Englishman;
    }

    private static ISet<Clue> FillInBlankClues(ISet<Clue> givenClues)
    {
        var initialClues = new HashSet<Clue>(givenClues);
        byte[] blankAttributes = {Color.All, Nationality.All, Pet.All, Drink.All, Smoke.All, Position.All};
        byte[] cumulativeAttributes = {Color.All, Nationality.All, Pet.All, Drink.All, Smoke.All, Position.All};
        foreach (var clue in givenClues)
        {
            for (int ii = 0; ii <= AttributeType.Position; ii++)
            {
                if (IsResolvedAttribute(clue.attributes[ii]))
                {
                    cumulativeAttributes[ii] -= clue.attributes[ii];                    
                }
            }
        }

        for (int jj = 0; jj <= AttributeType.Position; jj++)
        {
            for (int kk = 0; kk < CountAttributeBits(Color.All); kk++)
            {
                if (((cumulativeAttributes[jj] >> kk) & 0x1) == 0x1)
                {
                    var clueAttributes = new byte[AttributeType.Position + 1];
                    Array.Copy(blankAttributes, clueAttributes, blankAttributes.Length);
                    clueAttributes[jj] = (byte)(0x1 << kk);
                    initialClues.Add(new Clue {attributes = clueAttributes});
                }
            }
        }
        return initialClues;
    }

    enum Change {Enhanced, Combined, None}
    private static void Infer(ISet<Clue> clues)
    {
        ISet<Clue> cluesIn = clues;
        ISet<Clue> cluesOut = new HashSet<Clue>();
        ISet<Clue> combined = new HashSet<Clue>();
        bool somethingGoodHappened = true;

        while (somethingGoodHappened)
        {
            somethingGoodHappened = false;
            combined.Clear();
            cluesOut.Clear();
            foreach (Clue clueA in cluesIn)
            {
/*
                if (combined.Contains(clueA))
                    break;
*/
                Clue resultClue = new Clue();
                foreach (var clueB in cluesIn)
                {
                    Change modified;
                    (modified, resultClue) = MakeDirectInference(clueA, clueB);
                    if (modified != Change.None)
                    {
                        if (modified == Change.Combined)
                        {
                            combined.Add(clueB);
                        }
                        somethingGoodHappened = true;
                        break;
                    }
                }
                cluesOut.Add(resultClue);
                // we allow this to be updated twice with the same clue
                // we rely on the commutative property of MakeDirectInference
            }

            bool modifiedn = false;
            (modifiedn, cluesOut) = InferNeighbours(cluesOut);
            if (modifiedn)
            {
                somethingGoodHappened = true;
            }

            (_, cluesOut) = CombineMatchingClues(cluesOut);
            cluesIn = new HashSet<Clue>(cluesOut);
        }
    }

    /// <summary>
    /// This relies on the commutative property of this operation.
    /// clueA, clueB must have the same effect as clueB, clueA
    /// </summary>
    /// <param name="clueA">kind of the principal clue</param>
    /// <param name="clueB">kind of the secondary clue</param>
    /// <returns></returns>
    private static (Change, Clue) MakeDirectInference(Clue clueA, Clue clueB)
    {
        bool AreIncompatibleClues(Clue a, Clue b)
        {
            foreach (int idx in new[]
            {
                AttributeType.Color, AttributeType.Nationality, AttributeType.Pet, AttributeType.Drink,
                AttributeType.Smoke, AttributeType.Position
            })
            {
                if (IsResolvedAttribute(a.attributes[idx]) && IsResolvedAttribute(b.attributes[idx])
                                                            && a.attributes[idx] != b.attributes[idx])
                    return true;
            }

            return false;
        }

        if (clueA == clueB)
        {
            return (Change.None, clueA);
        }

        for (int ii = 0; ii < clueA.attributes.Length; ii++)
        {
            if (IsResolvedAttribute(clueA.attributes[ii]) && clueA.attributes[ii] == clueB.attributes[ii])
            {    // hurah! 2 different clues with the same attribute
                continue;
            }

            if (IsResolvedAttribute(clueA.attributes[ii]) && IsResolvedAttribute(clueB.attributes[ii]))
            {    // 2 different clues where each has already resolved this attribute
                continue;
            }
            else if ( IsResolvedAttribute(clueB.attributes[ii]) && AreIncompatibleClues(clueA, clueB))
            {
                // one of the attributes is confirmed so we can eliminate that from the other clue
                (bool modified, Clue resultClue) = CombineAttributes(clueA, clueB, ii);
                if (modified)
                {
                    return (Change.Enhanced, resultClue);
                }
            }
        }

        return (Change.None, clueA);
    }

    /// <summary>
    /// Leverage the logic that a clue cannot have its neighbour's attribute
    /// and eliminate that attribute from the clue.
    /// </summary>
    /// <param name="clues">initial set of clues</param>
    /// <returns>a newly generated set of clues</returns>
    private static ISet<Clue> EliminateNeighbours(ISet<Clue> clues)
    {
        var cluesOut = new HashSet<Clue>();
        foreach (var clue in clues)
        {
            var clueOut = clue.Clone();
            cluesOut.Add(clueOut);
            foreach (var neighbour in Neighbour.neighbours)
            {
                if (IsResolvedAttribute(clue.attributes[neighbour.attributeTypeA] )
                  && clue.attributes[neighbour.attributeTypeA] == neighbour.attrivubuteValueA)
                {
                    clueOut.attributes[neighbour.attributeTypeB] = TurnOffBit(
                        clueOut.attributes[neighbour.attributeTypeB],
                        neighbour.attrivubuteValueB);
                }
                if (IsResolvedAttribute(clue.attributes[neighbour.attributeTypeB])
                  && clue.attributes[neighbour.attributeTypeB] == neighbour.attrivubuteValueB)
                {
                    clueOut.attributes[neighbour.attributeTypeA] = TurnOffBit(
                        clueOut.attributes[neighbour.attributeTypeA],
                        neighbour.attrivubuteValueA);
                }
                // TODO edge case where both A and B apply to same clue - presumably an error
            }
        }

        return cluesOut;
    }

    /// <summary>
    /// Two cases:
    ///   1) ToRightOf.  we can exclude the known left hand attribute from house 5 and the right hand attribute from house 1
    ///   2) NextTo.  If we get lucky and one of the neighbours has a known house we can fix the other
    /// TODO: ToRightOf should take advantage of similar logic to NextTo.
    /// </summary>
    /// <param name="clues">this will change if the method is called multiple times</param>
    /// <returns>a new cloned list of clues</returns>
    private static (bool, ISet<Clue>) InferNeighbours(ISet<Clue> clues)
    {
        var cluesOut = new HashSet<Clue>();
        bool modified = false;
        foreach (var clue in clues)
        {
            var clueOut = clue.Clone();
            cluesOut.Add(clueOut);
        }
        foreach (var neighbour in Neighbour.neighbours)
        {
            // assert there are clues for all attributes
            // assert that setter is accustomed to a left-to-right culture
            Clue clueA, clueB;
            try
            {
                clueA = cluesOut.First(c => c.attributes[neighbour.attributeTypeA] == neighbour.attrivubuteValueA);
                clueB = cluesOut.First(c => c.attributes[neighbour.attributeTypeB] == neighbour.attrivubuteValueB);
            }
            catch (Exception e)
            {
                throw;
            }
            if (neighbour.Relation == Relation.ToRightOf)
            {
                var clueC = cluesOut.FirstOrDefault(c => c.attributes[AttributeType.Position] == Position.One);
                                    // left most house is to the right of nothing
                var clueD = cluesOut.FirstOrDefault(c => c.attributes[AttributeType.Position] == Position.Five);
                                    // right most house is to the left of nothing
                byte? oldC = clueC?.attributes[neighbour.attributeTypeA], oldD = clueD?.attributes[neighbour.attributeTypeB];
                if (clueC != null)
                {
                    clueC.attributes[neighbour.attributeTypeA]
                        = TurnOffBit(clueC.attributes[neighbour.attributeTypeA], neighbour.attrivubuteValueA);
                }

                if (clueD != null)
                {
                    clueD.attributes[neighbour.attributeTypeB]
                        = TurnOffBit(clueD.attributes[neighbour.attributeTypeB], neighbour.attrivubuteValueB); 
                }
                modified = oldC != clueC?.attributes[neighbour.attributeTypeA] || oldD != clueD?.attributes[neighbour.attributeTypeB];

            }
            else if (neighbour.Relation == Relation.NextTo)
            {
                if (IsResolvedAttribute(clueA.attributes[AttributeType.Position]))
                {
                    // we know the position of clue A we may be able to infer the position of clue B
                    if (clueA.attributes[AttributeType.Position] == Position.One)
                    {
                        if (clueB.attributes[AttributeType.Position] != Position.Two)
                        {
                            MyDebug.Assert(!IsResolvedAttribute(clueB.attributes[AttributeType.Position]));
                            clueB.attributes[AttributeType.Position] = Position.Two;
                            modified = true;
                        }
                    }
                    else if (clueA.attributes[AttributeType.Position] == Position.Five)
                    {
                        if (clueB.attributes[AttributeType.Position] != Position.Four)
                        {
                            MyDebug.Assert(!IsResolvedAttribute(clueB.attributes[AttributeType.Position]));
                            clueB.attributes[AttributeType.Position] = Position.Four;
                            modified = true;
                        }
                    }
                }
                if (IsResolvedAttribute(clueB.attributes[AttributeType.Position]))
                {
                    // we know the position of clue B we may be able to infer the position of clue B
                    if (clueB.attributes[AttributeType.Position] == Position.One)
                    {
                        if (clueA.attributes[AttributeType.Position] != Position.Two)
                        {
                            MyDebug.Assert(!IsResolvedAttribute(clueA.attributes[AttributeType.Position]));
                            clueA.attributes[AttributeType.Position] = Position.Two;
                            modified = true;
                        }
                    }
                    else if (clueB.attributes[AttributeType.Position] == Position.Five)
                    {
                        if (clueA.attributes[AttributeType.Position] != Position.Four)
                        {
                            MyDebug.Assert(!IsResolvedAttribute(clueA.attributes[AttributeType.Position]));
                            clueA.attributes[AttributeType.Position] = Position.Four;
                            modified = true;
                        }
                    }
                }
            }
        }

        return (modified, cluesOut);
    }
    
    /// <summary>
    /// 
    /// </summary>
    /// <param name="clueA">The clue that will be enhanced by this</param>
    /// <param name="clueB">The clue providing useful information - will remain unchanged</param>
    /// <param name="idx">The specific attribute being enhanced</param>
    /// <returns>An enhanced clue</returns>
    private static (bool, Clue) CombineAttributes(Clue clueA, Clue clueB, int idx)
    {
        Clue enhancedClue = new Clue();
        Array.Copy(clueA.attributes, enhancedClue.attributes, clueA.attributes.Length);
        enhancedClue.attributes[idx] = (byte)(clueA.attributes[idx] ^ (clueA.attributes[idx] & clueB.attributes[idx]));
        return (enhancedClue.attributes[idx] != clueA.attributes[idx], enhancedClue);
    }


    private static (bool, ISet<Clue>) CombineMatchingClues(ISet<Clue> clues)
    {
        var clueCombos = clues.OrderBy(c => c).SelectMany(
                c => clues.OrderBy(c3 => c3).Select(c2 => (c, c2)))
            .Where(grp => grp.Item1.CompareTo(grp.Item2) < 0) //.OrderBy(grp => grp.Item1).ThenBy(grp => grp.Item2)
            .ToList();
        var grouped = clueCombos.GroupBy(grp => grp.Item1).ToList();
        
        var cluesOut = new HashSet<Clue>();
        var modified = false;
        foreach (var grp in grouped.Append(grouped.Last().Select(grp => (grp.Item2, grp.Item1))))
        {
            bool matched = false;
            foreach ((var clueA, var clueB) in grp)
            {
                for (int ii = 0; ii < clueA.attributes.Length; ii++)
                {
                    if (IsResolvedAttribute(clueA.attributes[ii]) && clueA.attributes[ii] == clueB.attributes[ii])
                    {    // hurah! 2 different clues with the same attribute
                        var clueResult = CombineClues(clueA, clueB);
                        cluesOut.Add(clueResult);
                        matched = true;
                        modified = true;
                        goto outer;
                    }
                }
            }
outer:
            if (!matched)
            {
                cluesOut.Add(grp.First().Item1);             
            }
        }
         
        return (modified, cluesOut);
    }
    
    /// <summary>
    /// there is a precondition that at least on attribute in clueA
    /// will be equal to the corresponding attribute in clueB
    /// i.e. these two clues relate to the same house
    /// </summary>
    /// <param name="clueA">relates to the same house as clueB</param>
    /// <param name="clueB"></param>
    /// <returns>a newly minted clue combining the best of the two inputs</returns>
    private static Clue CombineClues(Clue clueA, Clue clueB)
    {
        Clue combinedClue = new Clue{attributes = new byte[AttributeType.Position + 1]};
        for (int ii = 0; ii < clueA.attributes.Length; ii++)
        {
            combinedClue.attributes[ii]
                = CountAttributeBits(clueA.attributes[ii])
                  < CountAttributeBits(clueB.attributes[ii])
                    ? clueA.attributes[ii]
                    : clueB.attributes[ii];
        }

        return combinedClue;
    }

    public static byte OwnsZebra()
    {
        return Nationality.Englishman;
    }
}

internal static class MyDebug
{
    public static void Assert(bool b)
    {
        if (!b)
            throw new Exception("what the hey!");
    }
}

