public interface Namespace {
    enum Bob {
        Shouts,
        Yells
    }

    interface Inner
    {
        static String DoIt() { return ""; }
        default String SayYes() {
            return "yes";
        }

        String SayNo();

        String SayMaybe();
    }

    class John {
        private static void doInnerStuff(Inner inner) {
            inner.SayMaybe();
            inner.SayNo();
            Inner.DoIt();
        }

        public static void Foo() {
            doInnerStuff(new Inner() {
                public String SayNo() { return "no";}
                public String SayMaybe() { return "maybe";}
            });
        }

    }
    class Fred {
        void foo() {
            John.Foo();
        }
    }


}
