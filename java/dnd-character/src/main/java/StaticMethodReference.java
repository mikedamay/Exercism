import java.util.List;

public class StaticMethodReference {

    public static  void doneWith() {
        System.out.println("done!");
    }

    @FunctionalInterface
    public interface Doer {
        void doit();
    }
    public static void mainer() {
        var dd = new Doer() {
            public void doit() {
                StaticMethodReference.doneWith();
            }
        };
        dd.doit();

    }

}
