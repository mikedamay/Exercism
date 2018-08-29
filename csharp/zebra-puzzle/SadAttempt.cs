namespace SadAttempt
{
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata.Ecma335;
using System.Xml.Schema;

public enum Color { Red , Green , Ivory , Yellow , Blue }
public enum Nationality { Englishman , Spaniard , Ukranian , Japanese , Norwegian }
public enum Pet { Dog , Snails , Fox , Horse , Zebra }
public enum Drink { Coffee , Tea , Milk , OrangeJuice , Water }
public enum Smoke { OldGold , Kools , Chesterfields , LuckyStrike , Parliaments }
internal enum Relation {Direct, ToRightOf, NextTo, Position}
internal enum Position {One, Two, Three, Four, Five}
internal enum AttributeType {Color = 0, Nationality = 1, Pet = 2, Drink = 3, Smoke = 4, Position = 5}

internal class Attribute
{
    public static readonly IReadOnlyDictionary<AttributeType, Attribute[]> AvailableAttributes
        = new Dictionary<AttributeType, Attribute[]>
        {
            {AttributeType.Color, MakeAttributeArray<Color>(t => new Attribute(t))},
            {AttributeType.Nationality, MakeAttributeArray<Nationality>(t => new Attribute(t))},
            {AttributeType.Pet, MakeAttributeArray<Pet>(t => new Attribute(t))},
            {AttributeType.Drink, MakeAttributeArray<Drink>(t => new Attribute(t))},
            {AttributeType.Smoke, MakeAttributeArray<Smoke>(t => new Attribute(t))},
            {AttributeType.Position, MakeAttributeArray<Position>(t => new Attribute(t))},
        };

    static Attribute()
    {
        MyDebug.Assert(
            Enum.GetValues(typeof(Color)).Length == Enum.GetValues(typeof(Nationality)).Length
              && Enum.GetValues(typeof(Color)).Length == Enum.GetValues(typeof(Pet)).Length
              && Enum.GetValues(typeof(Color)).Length == Enum.GetValues(typeof(Drink)).Length
              && Enum.GetValues(typeof(Color)).Length == Enum.GetValues(typeof(Smoke)).Length
              && Enum.GetValues(typeof(Color)).Length == Enum.GetValues(typeof(Position)).Length
            );
    }
    static Attribute[] MakeAttributeArray<T>(Func<T, Attribute> makeAttribute)
    {
        var arr = new Attribute[(int) global::Position.Five + 1];
        foreach (var at in Enum.GetValues(typeof(T)))
        {
            arr[(int)at] = makeAttribute((T)at);
        }

        return arr;
    }
    
    public Color? Color { get; }
    public Nationality? Nationality { get; }
    public Pet? Pet { get; }
    public Drink? Drink { get; }
    public Smoke? Smoke { get; }
    public Position? Position { get; }

    public AttributeType AttributeType =>
        this.Color.HasValue ? AttributeType.Color 
        : this.Nationality.HasValue ? AttributeType.Nationality
        : this.Pet.HasValue ? AttributeType.Pet
        : this.Drink.HasValue ? AttributeType.Drink
        : this.Smoke.HasValue ? AttributeType.Smoke
        : this.Position.HasValue ? AttributeType.Position
        : throw new Exception($"Unknown attribute type encountered")
        ;
    
    public Attribute(Color Color) {this.Color = Color;}
    public Attribute(Nationality Nationality) {this.Nationality = Nationality;}
    public Attribute(Pet Pet) {this.Pet = Pet;}
    public Attribute(Drink Drink) {this.Drink = Drink;}
    public Attribute(Smoke Smoke) {this.Smoke = Smoke;}
    public Attribute(Position Position) {this.Position = Position;}

    protected bool Equals(Attribute other)
    {
        Array aa = Enum.GetValues(typeof(Color));
        foreach (var a in aa)
        {
            
        }
        return Color == other.Color && Nationality == other.Nationality && Pet == other.Pet && Drink == other.Drink && Smoke == other.Smoke && Position == other.Position;
    }

    public override bool Equals(object obj)
    {
        if (ReferenceEquals(null, obj)) return false;
        if (ReferenceEquals(this, obj)) return true;
        if (obj.GetType() != this.GetType()) return false;
        return Equals((Attribute) obj);
    }

    public override int GetHashCode()
    {
        unchecked
        {
            var hashCode = Color.GetHashCode();
            hashCode = (hashCode * 397) ^ Nationality.GetHashCode();
            hashCode = (hashCode * 397) ^ Pet.GetHashCode();
            hashCode = (hashCode * 397) ^ Drink.GetHashCode();
            hashCode = (hashCode * 397) ^ Smoke.GetHashCode();
            hashCode = (hashCode * 397) ^ Position.GetHashCode();
            return hashCode;
        }
    }
}

internal class Clue
{
    public Attribute Principal { get; } 
    public Relation Relation { get; } 
    public Attribute Other { get; }

    public Clue(Attribute Prinicipal, Relation Relation, Attribute Other)
    {
        this.Principal = Prinicipal;
        this.Relation = Relation;
        this.Other = Other;
    }

    protected bool Equals(Clue other)
    {
        return Equals(Principal, other.Principal) && Relation == other.Relation && Equals(Other, other.Other);
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
        unchecked
        {
            var hashCode = (Principal != null ? Principal.GetHashCode() : 0);
            hashCode = (hashCode * 397) ^ (int) Relation;
            hashCode = (hashCode * 397) ^ (Other != null ? Other.GetHashCode() : 0);
            return hashCode;
        }
    }
}

internal class House
{
    public ISet<Attribute> Attributes = new HashSet<Attribute>();
}


public static class ZebraPuzzle
{
    static ZebraPuzzle()
    {
        var clueMap = InitClueMap();
        IList<House> houses = BuildHouses(clueMap);
        FixMissingDirectRelations(clueMap, houses);
    }

    /// <summary>
    /// To the extent possible assigns attributes to the houses based on direct relationships
    /// </summary>
    /// <param name="clueMap">All the clues previded in the question</param>
    /// <returns>incomplete houses.  Attributes are assigned for direct relationships only.
    /// Neighbour relationships are ignored so far</returns>
    private static IList<House> BuildHouses(IReadOnlyDictionary<Attribute, ISet<Clue>> clueMap)
    {
        IList<House> houses = new List<House>();
        var uniquer = new HashSet<Attribute>();
        House house = null;
        foreach (var houseClues in clueMap)
        {
            if (!uniquer.Contains(houseClues.Key))
            {
                house = new House();
                houses.Add(house);
            }

            uniquer.Add(houseClues.Key);
            foreach (var clue in houseClues.Value)
            {
                if (clue.Relation != Relation.Direct)
                {
                    continue;
                }
                house.Attributes.Add(clue.Principal);
                house.Attributes.Add(clue.Other);
                GatherAttributes(
                  houseClues.Key.Equals(clue.Principal) ? clue.Other : clue.Principal
                  ,uniquer, clueMap, house);
            }
        }

        return houses;
    }

    private static void GatherAttributes(Attribute attribute, HashSet<Attribute> uniquer
      ,IReadOnlyDictionary<Attribute, ISet<Clue>> clueMap, House house)
    {
        if (uniquer.Contains(attribute))
            return;
        uniquer.Add(attribute);
        if (clueMap.ContainsKey(attribute))
        {
            foreach (var clue in clueMap[attribute])
            {
                house.Attributes.Add(clue.Principal);
                house.Attributes.Add(clue.Other);
                GatherAttributes(
                  attribute.Equals(clue.Principal) ? clue.Other : clue.Principal
                  ,uniquer, clueMap, house);
            }
        }
    }

    private static void FixMissingDirectRelations(IReadOnlyDictionary<Attribute, ISet<Clue>> clueMap, IList<House> houses)
    {
        var directClueWrappers = clueMap.Where(kv => kv.Value.Any(clue => clue.Relation == Relation.Direct));
        var attributeCounts = directClueWrappers
            .GroupBy(kv => kv.Key.AttributeType).Select(kv => new { attributeType = kv.Key, count = kv.Count()})
            .Where(kv => kv.count == (int)Position.Five).ToArray();
                    // ie. if there 5 avaiable values for an attribute we want those that have 4 accounted for
                    // and one missing
        foreach (var ac in attributeCounts)
        {
            AssignAttributeToHouse(houses, directClueWrappers, ac.attributeType);
        }
        
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="houses">all the houses in the system</param>
    /// <param name="clueWrappers">wrappers round clues which involve a direct relation - approx 20 of themt</param>
    /// <param name="attributeType">Color, Nationality etc. whichever one is
    ///  under consideration</param>
    private static void AssignAttributeToHouse(IList<House> houses
      ,IEnumerable<KeyValuePair<Attribute, ISet<Clue>>> clueWrappers, AttributeType attributeType)
    {
        var clueAttributes = clueWrappers.Select(kv => kv.Key)
            .Where(att => att.AttributeType == attributeType);
        var availableAttributes = Attribute.AvailableAttributes[attributeType];
        var missingAttribute = availableAttributes.Except(clueAttributes).First();
        var houseWithMissingAttribute = houses.Where(h => h.Attributes.All(att => att != missingAttribute)).First();
        houseWithMissingAttribute.Attributes.Add(missingAttribute);
    }

    private static IReadOnlyDictionary<Attribute, ISet<Clue>> InitClueMap()
    {
        Clue[] clues =
        {
            new Clue(new Attribute(Nationality.Englishman), Relation.Direct, new Attribute(Color.Red)),
            
            new Clue(new Attribute(Nationality.Spaniard), Relation.Direct, new Attribute(Pet.Dog)),
            new Clue(new Attribute(Drink.Coffee), Relation.Direct, new Attribute(Color.Green)),
            // green house not in position 1 (because it is to the right of something)
            // green house not in position 2 because the blue house is
            // green house not in position 3 because Milk is drunk there
            // green house is in position 4 or 5  (next to the fox and to right of ivory)
            // ukranian is not in position 3
            // japanese/parliaments or englishman/red could be at position 3
            // japanese drinks either milk or water
            // spaniard smokes Chesterfields
            // norwegian is in position 2
            // blue house is in position 2
            // kools are smoked in 1 or 2
            new Clue(new Attribute(Nationality.Ukranian), Relation.Direct, new Attribute(Drink.Tea)),          
            new Clue(new Attribute(Color.Green), Relation.ToRightOf, new Attribute(Color.Ivory)),
            new Clue(new Attribute(Smoke.OldGold), Relation.Direct, new Attribute(Pet.Snails)),            
            new Clue(new Attribute(Smoke.Kools), Relation.Direct, new Attribute(Color.Yellow)),            
            new Clue(new Attribute(Drink.Milk), Relation.Position, new Attribute(Position.Three)),
            new Clue(new Attribute(Nationality.Norwegian), Relation.NextTo, new Attribute(Position.One)),            
            new Clue(new Attribute(Smoke.Chesterfields), Relation.NextTo, new Attribute(Pet.Fox)),
            new Clue(new Attribute(Smoke.Kools), Relation.NextTo, new Attribute(Pet.Horse)),            
            new Clue(new Attribute(Smoke.LuckyStrike), Relation.Direct, new Attribute(Drink.OrangeJuice)),            
            new Clue(new Attribute(Nationality.Japanese), Relation.Direct, new Attribute(Smoke.Parliaments)),            
            new Clue(new Attribute(Nationality.Norwegian), Relation.NextTo, new Attribute(Color.Blue)),
        };
        Dictionary<Attribute, ISet<Clue>> clueMap = new Dictionary<Attribute, ISet<Clue>>();
            // clue map should end up with approx 11 sets of clues (consisting of 1 or 2 entries)
            // pointed to by a
            // couple of dozen keys (attributes)
        foreach (var clue in clues)
        {
            if (clueMap.ContainsKey(clue.Principal) && clueMap.ContainsKey(clue.Other))
            {
                // just in case this happens - they could have different relationships
                clueMap[clue.Principal].Add(clue);
            }
            else if (!clueMap.ContainsKey(clue.Principal) && !clueMap.ContainsKey(clue.Other))
            {
                var houseClues = new HashSet<Clue> {clue};
                clueMap[clue.Principal] = houseClues;
                clueMap[clue.Other] = houseClues;
            }
            else if (clueMap.ContainsKey(clue.Other))
            {
                var houseClues = clueMap[clue.Other];
                houseClues.Add(clue);
                clueMap[clue.Principal] = houseClues;
            }
            else // clue.Other has not been previously encountered
            {
                var houseClues = clueMap[clue.Principal];
                houseClues.Add(clue);
                clueMap[clue.Other] = houseClues;
            }
        }

        return clueMap;
    }

    public static Nationality DrinksWater()
    {
        return Nationality.Englishman;
    }

    public static Nationality OwnsZebra()
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

}