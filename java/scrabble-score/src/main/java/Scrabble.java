import java.util.HashMap;
import java.util.Map;

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
        int sum = 0;
        for ( int ii = 0; ii < word.length(); ii++) {
            sum = sum + letters.get(word.charAt(ii));
        }
        return sum;
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
