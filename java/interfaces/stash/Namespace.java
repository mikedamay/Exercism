public interface Namespace {
    public interface Language {
        String getLanguageName();
        String speak();
    }

    public class ItalianTaveller implements Language, Cloneable {

        // from Language interface
        public String getLanguageName() {
            return  "Italiano";
        }

        // from Language interface
        public String speak() {
            return "Ciao mondo";
        }

        // from Cloneable interface
        public Object Clone() {
            ItalianTaveller it = new ItalianTaveller();
            return it;
        }
    }
}
