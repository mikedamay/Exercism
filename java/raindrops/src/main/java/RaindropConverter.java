import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class RaindropConverter {

    String convert2(int number) {

        StringBuilder sb = new StringBuilder();

        if (number % 3 == 0)
            sb.append("Pling");
        if (number % 5 == 0)
            sb.append("Plang");
        if (number % 7 == 0)
            sb.append("Plong");
        if (sb.length() == 0)
            sb.append(number);
        return sb.toString();
    }
    static String convert(int number) {
        Optional<String> result = Stream.of(3, 5, 7).mapToObj(RaindropConverter::toSound).reduce((sounds, sound) -> sounds + sound);
        return result.get().length() == 0 ? result.get() : String.valueOf(number);
    }
    static String toSound(int n) {
        switch(n) {
            case 3:
                return "Pling";
            case 5:
                return "Plong";
            case 7:
                return "Plang";
        }
        return "";
    }
}
