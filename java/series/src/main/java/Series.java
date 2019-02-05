import org.checkerframework.checker.nullness.qual.NonNull;

import java.util.ArrayList;
import java.util.List;

public class Series {

    private final String subject;

    public Series(@NonNull String subject) {
        this.subject = subject;
    }

    public List<String> slices(int numLetters) {
        if (numLetters > subject.length()) {
            throw new IllegalArgumentException("Slice size is too big.");
        }
        if (numLetters <= 0) {
            throw new IllegalArgumentException("Slice size is too small.");
        }
        var list = new ArrayList<String>();
        for (int ii = 0; ii < subject.length() - numLetters + 1; ii++) {
            list.add(subject.substring(ii, ii + numLetters));
        }
        return list;
    }
}
