import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class Florian {

    private static final List<Integer> BIG_STRAIGHT = List.of(1, 2, 3, 4, 5);
    private static final List<Integer> LITTLE_STRAIGHT = List.of(2, 3, 4, 5, 6);
    private final int[] dice;
    private final YachtCategory yachtCategory;

    Florian(int[] dice, YachtCategory yachtCategory) {
        this.dice = dice;
        this.yachtCategory = yachtCategory;
    }

    int score() {
        switch (yachtCategory) {
            case ONES:
                return throwScore(1);
            case TWOS:
                return throwScore(2);
            case THREES:
                return throwScore(3);
            case FOURS:
                return throwScore(4);
            case FIVES:
                return throwScore(5);
            case SIXES:
                return throwScore(6);
            case FULL_HOUSE:
                return fullHouseScore();
            case FOUR_OF_A_KIND:
                return fourOfAKindScore();
            case LITTLE_STRAIGHT:
                return littleStraightScore();
            case BIG_STRAIGHT:
                return bigStraightScore();
            case CHOICE:
                return scoreAll();
            case YACHT:
                return yachtScore();
        }
        return 0;
    }

    private int bigStraightScore() {
        if (isStraightOf(LITTLE_STRAIGHT)) {
            return 30;
        }
        return 0;
    }

    private int littleStraightScore() {
        if (isStraightOf(BIG_STRAIGHT)) {
            return 30;
        }
        return 0;
    }

    private boolean isStraightOf(List<Integer> elements) {
        return Arrays
                .stream(dice)
                .boxed()
                .collect(Collectors.toList())
                .containsAll(elements);
    }

    private int fourOfAKindScore() {
        return Arrays
                .stream(dice)
                .boxed()
                .collect(Collectors.toMap(c -> c, c -> 1, Integer::sum))
                .entrySet()
                .stream()
                .filter(e -> e.getValue() >= 4)
                .map(e -> e.getKey() * 4)
                .findFirst()
                .orElse(0);
    }

    private int fullHouseScore() {
        if (isFullHouse()) {
            return scoreAll();
        }
        return 0;
    }

    private boolean isFullHouse() {
        int[] distinctValues = Arrays
                .stream(dice)
                .distinct()
                .toArray();
        if (distinctValues.length == 2) {
            int[] twosAndThreeOfAKind = Arrays
                    .stream(distinctValues)
                    .map(c -> (int) Arrays
                            .stream(dice)
                            .filter(v -> c == v)
                            .count())
                    .filter(v -> v == 2 || v == 3)
                    .toArray();
            return Arrays.binarySearch(twosAndThreeOfAKind, 2) >= 0
                    && Arrays.binarySearch(twosAndThreeOfAKind, 3) >= 0;

        }
        return false;
    }

    private int throwScore(int side) {
        return Arrays
                .stream(dice)
                .filter(c -> c == side)
                .sum();
    }

    private int yachtScore() {
        if (isYacht()) {
            return 50;
        } else {
            return 0;
        }
    }

    private boolean isYacht() {
        return Arrays
                .stream(dice)
                .filter(current -> current != dice[0])
                .findAny()
                .isEmpty();
    }

    private int scoreAll() {
        return Arrays
                .stream(dice)
                .sum();
    }
}