public class ConstructorReference {

    @FunctionalInterface
    public interface Creator {
        void createIt();
    }
    public static void mainer() {
        var dd = new Creator() {
            public void createIt() {
                new Object();
            }
        };
        dd.createIt();

        Creator cc2 = () -> new Object();
        Creator cc3 = Object::new;
        cc2.createIt();
        cc3.createIt();
    }

}
