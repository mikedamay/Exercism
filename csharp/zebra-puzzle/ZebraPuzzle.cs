using System;
using System.Collections.Generic;
using System.Security.Cryptography.X509Certificates;
using System.Xml.Schema;
using Xunit.Sdk;

internal struct Color {public const byte Red = 1 << 0, Green = 1 << 1, Ivory = 1 << 2, Yellow = 1 << 3, Blue = 1 << 4, All = 0x1f;}
internal struct Nationality {public const byte Englishman = 1 << 0, Spaniard = 1 << 1, Ukranian = 1 << 2, Japanese = 1 << 3, Norwegian = 1 << 4, All = 0x1f;}
internal struct Pet {public const byte Dog = 1 << 0, Snails = 1 << 1, Fox = 1 << 2, Horse = 1 << 3, Zebra = 1 << 4, All = 0x1f;}
internal struct Drink {public const byte Coffee = 1 << 0, Tea = 1 << 1, Milk = 1 << 2, OrangeJuice = 1 << 3, Water = 1 << 4, All = 0x1f;}
internal struct Smoke {public const byte OldGold = 1 << 0, Kools = 1 << 1, Chesterfields = 1 << 2, LuckyStrike = 1 << 3, Parliaments = 1 << 4, All = 0x1f;}
internal struct Position {public const byte One = 1 << 0, Two = 1 << 1, Three = 1 << 2, Four = 1 << 3, Five = 1 << 4, All = 0x1f;}
internal struct AttributeType {public const byte Color = 0, Nationality = 1, Pet = 2, Drink = 3, Smoke = 4, Position = 5;}


internal enum Relation {Direct, ToRightOf, NextTo, Position}

internal class Clue
{
    public byte[] attributes = new byte[AttributeType.Position + 1];
    public Relation relation;
    
}



public static class ZebraPuzzle
{
    private static ISet<Clue> initialClues = new HashSet<Clue>
    {
        new Clue{attributes = new byte[]{Color.Red, Nationality.Englishman, Pet.All, Drink.All, Smoke.All, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.Spaniard, Pet.Dog, Drink.All, Smoke.All, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.Green, Nationality.All, Pet.All, Drink.Coffee, Smoke.All, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.Ukranian, Pet.All, Drink.Tea, Smoke.All, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Snails, Drink.All, Smoke.OldGold, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.Yellow, Nationality.All, Pet.All, Drink.All, Smoke.Kools, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.All, Drink.Milk, Smoke.All, Position.Three}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.Norwegian, Pet.All, Drink.All, Smoke.All, Position.One}, relation = Relation.Direct},
//        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Fox, Drink.All, Smoke.Chesterfields, Position.All}, relation = Relation.NextTo},
//        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.Horse, Drink.All, Smoke.Kools, Position.All}, relation = Relation.NextTo},
        new Clue{attributes = new byte[]{Color.All, Nationality.All, Pet.All, Drink.OrangeJuice, Smoke.LuckyStrike, Position.All}, relation = Relation.Direct},
        new Clue{attributes = new byte[]{Color.All, Nationality.Japanese, Pet.All, Drink.All, Smoke.Parliaments, Position.All}, relation = Relation.Direct},
//        new Clue{attributes = new byte[]{Color.Blue, Nationality.Norwegian, Pet.All, Drink.All, Smoke.All, Position.All}, relation = Relation.NextTo},
    };
    
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
        Infer(initialClues);
        return Nationality.Englishman;
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
                if (clueA.relation != Relation.Direct || combined.Contains(clueA))
                {
                    continue;
                }

                Clue resultClue = new Clue();
                foreach (var clueB in cluesIn)
                {
                    if (clueB.relation != Relation.Direct || combined.Contains(clueB))
                    {
                        continue;
                    }
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

            cluesIn = new HashSet<Clue>(cluesOut);
        }
    }

    private static int CountAttributeBits(byte b) => (b & 0x1) + (b >> 1 & 0x1) + (b >> 2 & 0x1) + (b >> 3 & 0x1) + (b >> 4 & 0x1);
    /// <summary>
    /// This relies on the commutative property of this operation.
    /// clueA, clueB must have the same effect as clueB, clueA
    /// </summary>
    /// <param name="clueA">kind of the principal clue</param>
    /// <param name="clueB">kind of the secondary clue - although I suspect there is no real difference</param>
    /// <returns></returns>
    private static (Change, Clue) MakeDirectInference(Clue clueA, Clue clueB)
    {
        bool IsConfirmed(byte b) => CountAttributeBits(b) == 1;
        if (clueA == clueB)
        {
            return (Change.None, clueA);
        }

        for (int ii = 0; ii < clueA.attributes.Length; ii++)
        {
            bool AreIncompatibleClues(Clue a, Clue b)
            {
                foreach (int idx in new[]
                {
                    AttributeType.Color, AttributeType.Nationality, AttributeType.Pet, AttributeType.Drink,
                    AttributeType.Smoke, AttributeType.Position
                })
                {
                    if (IsConfirmed(a.attributes[idx]) && IsConfirmed(b.attributes[idx])
                                                       && a.attributes[idx] != b.attributes[idx])
                        return true;
                }

                return false;
            }

            if (IsConfirmed(clueA.attributes[ii]) && clueA.attributes[ii] == clueB.attributes[ii])
            {
                continue;
                return (Change.Combined, CombineClues(clueA, clueB));
            }

            if (IsConfirmed(clueA.attributes[ii]) && IsConfirmed(clueB.attributes[ii]))
            {
                continue;    // two houses with different attributes which we have identified
            }
            else if ( IsConfirmed(clueB.attributes[ii]) && AreIncompatibleClues(clueA, clueB))
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

    private static (bool, Clue) CombineAttributes(Clue clueA, Clue clueB, int idx)
    {
        Clue enhancedClue = new Clue();
        enhancedClue.relation = clueA.relation;
        for (int ii = 0; ii < clueA.attributes.Length; ii++)
        {
            enhancedClue.attributes[ii] = clueA.attributes[ii];
        }
        
        enhancedClue.attributes[idx] = (byte)(clueA.attributes[idx] ^ (clueA.attributes[idx] & clueB.attributes[idx]));
        return (enhancedClue.attributes[idx] != clueA.attributes[idx], enhancedClue);
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
        bool HasNoClue(byte b) => b == Color.All;
        Clue combinedClue = new Clue{relation = Relation.Direct, attributes = new byte[AttributeType.Position + 1]};
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

