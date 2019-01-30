public class InstanceMethodReference {

    @FunctionalInterface
    private interface Doer {
        void doit();
    }
    public InstanceMethodReference() {
        var exp = new Exploited();

        var dd = new Doer() {
            public void doit() {
                exp.performExploit();
            }
        };
        Doer dd2 = () -> exp.performExploit();
        Doer dd3 = exp::performExploit;

        dd.doit();
        dd2.doit();
        dd3.doit();


    }
}
