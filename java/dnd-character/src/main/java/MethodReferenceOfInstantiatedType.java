public class MethodReferenceOfInstantiatedType {

    @FunctionalInterface
    private interface Doer {
        void doit(Abc o);
    }
    public MethodReferenceOfInstantiatedType() {

        var exp = new Exploited();

        var dd = new Doer() {
            public void doit(Abc abc) {
               abc.doSomething();
            }
        };
        Doer dd2 = (abc) -> abc.doSomething();
        Doer dd3 = Abc::doSomething;

        dd.doit(new Abc());
        dd2.doit(new Abc());
        dd3.doit(new Abc());


    }
}
