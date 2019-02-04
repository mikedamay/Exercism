import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

class Scrabble {

    private final Map<String, Integer> rawLetters = Map.of(
            "AEIOULNRST  ",1,
            "DG",2,
            "BCMP",3,
            "FHVWY",4,
            "K", 5,
            "JX",8,
            "QZ", 10

    );
    private final Map<Character, Integer> letters;

    private final String word;
    Scrabble(String word) {
        this.word = word.toUpperCase();
        this.letters = generateLetterMap(rawLetters);
    }

    int getScore() {
        return  word.chars().mapToObj(ii -> (char)ii).map(letters::get).reduce(0, (a, b) -> a + b);
    }
    private Map<Character, Integer> generateLetterMap(Map<String, Integer> rawLetters) {
        var map = new HashMap<Character, Integer>();
        for (var entry : rawLetters.entrySet()) {
            for (int ii = 0; ii < entry.getKey().length(); ii++) {
                map.put(entry.getKey().charAt(ii), entry.getValue());
            }
        }
        return map;
    }
}
