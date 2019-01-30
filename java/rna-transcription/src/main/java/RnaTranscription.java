import java.util.Map;

class RnaTranscription {
    private final Map<Character, Character> map = Map.of(
            'G', 'C'
            ,'C', 'G'
            ,'T', 'A'
            ,'A', 'U'
    );

    String transcribe(String dnaStrand) {
        StringBuilder sb = new StringBuilder();
        for (int ii = 0; ii < dnaStrand.length(); ii++) {
            var c = map.get(dnaStrand.charAt(ii));
            if ( c != null)
                sb.append(c);
        }
        return sb.toString();
    }

}
