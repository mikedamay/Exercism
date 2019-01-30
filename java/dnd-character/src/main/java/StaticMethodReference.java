import java.util.List;

public class StaticMethodReference {

    public static  void doneWith(int ii) {
        System.out.println("done!");
    }

    @FunctionalInterface
    public interface Doer {
        void doit(int ii);
    }
    public static void mainer() {
        var dd = new Doer() {
            public void doit(int ii) {
                StaticMethodReference.doneWith(ii);
            }
        };
        dd.doit(8);
    }

}
