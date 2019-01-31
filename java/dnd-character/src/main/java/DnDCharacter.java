import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class DnDCharacter {
    private int constitution = 0;
    private int strength = 0;
    private int dexterity = 0;
    private int intelligence = 0;
    private int wisdom = 0;
    private int charisma = 0;

    int ability() {
        var rnd = new Random();
        List<Integer> s1 = rnd.ints(4, 1, 7).boxed().collect(Collectors.toList());
        Stream<Integer> s2 = s1.stream().sorted(Integer::compareTo).skip(1);
        var sum = s2.reduce(0, (a, b) -> a + b);
        return sum;
    }

    int modifier(int input) {
        var res = (int)Math.floor(((float)input - 10) / 2) ;
        return res;
    }

    public int getStrength() {
        if (strength == 0)
            strength = ability();
        return strength;
    }

    public int getDexterity() {
        if (dexterity == 0)
            dexterity = ability();
        return dexterity;
    }

    public int getConstitution() {
        if (constitution == 0)
            constitution = ability();
        return constitution;
    }
    public int getIntelligence() {
        if (intelligence == 0)
            intelligence = ability();
        return intelligence;
    }
    public int getWisdom() {
        if (wisdom == 0)
            wisdom = ability();
        return wisdom;
    }
    public int getCharisma() {
        if (charisma == 0)
            charisma = ability();
        return charisma;
    }

    public int getHitpoints() {
        return 10 + modifier(getConstitution());
    }
}
