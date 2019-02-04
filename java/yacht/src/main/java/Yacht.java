import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Yacht {

    private final Map<YachtCategory, Function<IntStream, Integer >> funs = Map.of(
            YachtCategory.ONES
            , (arr) -> arr
                    .filter(ii -> ii == 1).reduce(0, (a, b) -> a + b)
            ,YachtCategory.TWOS
            , (arr) -> arr
                    .filter(ii -> ii == 2).reduce(0, (a, b) -> a + b)
            ,YachtCategory.THREES
            , (arr) -> arr
                    .filter(ii -> ii == 3).reduce(0, (a, b) -> a + b)
            ,YachtCategory.FOURS
            , (arr) -> arr
                    .filter(ii -> ii == 4).reduce(0, (a, b) -> a + b)
            ,YachtCategory.FIVES
            , (arr) -> arr
                    .filter(ii -> ii == 5).reduce(0, (a, b) -> a + b)
            ,YachtCategory.SIXES
            , (arr) -> arr
                    .filter(ii -> ii == 6).reduce(0, (a, b) -> a + b)
            ,YachtCategory.FULL_HOUSE
            ,Yacht::doFullHouse
            ,YachtCategory.FOUR_OF_A_KIND
            ,Yacht::doFourOfAKind
            ,YachtCategory.LITTLE_STRAIGHT
            ,(dice) -> Yacht.doStraight(dice, 1)
            ,YachtCategory.BIG_STRAIGHT
            ,(dice) -> Yacht.doStraight(dice, 2)
    );
    private final int[] dice;
    private final YachtCategory yachtCategory;

    Yacht(int[] dice, YachtCategory yachtCategory) {
        this.dice = dice;
        this.yachtCategory = yachtCategory;
    }

    int score() {
        if (yachtCategory == YachtCategory.YACHT)
            return doYacht();
        if (yachtCategory == YachtCategory.CHOICE)
            return doChoice();
        return funs.get(yachtCategory).apply(IntStream.of(dice).sorted());

    }
    private int doYacht() {
        for ( int ii = 1; ii < dice.length; ii++ ) {
            if (dice[ii] != dice[ii -1])
                return 0;
        }
        return 50;
    }
    private static int doFullHouse(IntStream orderedDice) {
        var map = mapDice(orderedDice);
        if (map.size() == 2) {
            int sum = 0;
            for ( var entry : map.entrySet()) {
                if (entry.getValue() == 1)
                    return 0;
                else
                    sum += entry.getKey() * entry.getValue();
            }
            return sum;
        }
        return 0;

    }
    private static int doStraight(IntStream orderedDice, int start) {
        int prev = start - 1;
            for ( var d : orderedDice.boxed().collect(Collectors.toList())) {
                if (d - prev != 1) {
                    return 0;
                }
                prev = d;
            }
        return 30;
    }
    private static int doFourOfAKind(IntStream orderedDice) {
        var map = mapDice(orderedDice);
        if (map.size() <= 2) {
            for (var entry : map.entrySet()) {
                if ( entry.getValue() >= 4 ) {
                    return entry.getKey() * 4;
                }
            }
        }
        return 0;

    }
    private static Map<Integer, Integer> mapDice(IntStream orderedDice) {
        Map<Integer, Integer> map = new HashMap<>();
        orderedDice.boxed().forEach(
                d -> {
                    if (map.containsKey(d)) map.put(d, map.get(d) + 1);
                else
                    map.put(d, 1);
                }
        );
        return map;
    }
    private int doChoice() {
        Arrays.sort(dice);
        for ( int ii = 1; ii < dice.length; ii++) {
            if (dice[ii] == dice[ii -1 ])
                return Arrays.stream(dice).reduce(0, (a,b) -> a + b);
        }
        return 0;
    }
}
