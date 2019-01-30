import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class DnDCharacter {

    int ability() {
        throw new UnsupportedOperationException("Delete this statement and write your own implementation.");
    }

    int modifier(int input) {
        throw new UnsupportedOperationException("Delete this statement and write your own implementation.");
    }

    private int best3of4Score() {
        List<Integer> aaa = Stream.of(1,2,3).collect(Collectors.toList());
        int sum = 0;
        var rnd = new Random();
        var _4throws = rnd.ints().filter(t -> t > 0 && t < 7 ).limit(4).boxed();
        var bbb = _4throws.collect(Collectors.toList());
        for (int ii = 0; ii < 4; ii++ ) {
        }
        return 0;
    }

    public int getStrength() {
        return 0;
    }

    public int getDexterity() {
        return 0;
    }

    public int getConstitution() {
        return 0;
    }
    public int getIntelligence() {
        return 0;
    }
    public int getWisdom() {
        return 0;
    }
    public int getCharisma() {
        return 0;
    }

    public int getHitpoints() {
        return 0;
    }
}
