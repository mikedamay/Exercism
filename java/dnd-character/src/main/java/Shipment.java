import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

public class Shipment {

    private class Abc implements Consumer<Integer> {

        @Override
        public void accept(Integer integer) {

        }
    }
    public double calculateWeight() {
        double weight = 0;
        // Calculate weight
        return weight;
    }


    public static List<Double> calculateOnShipments(
            List<Shipment> l, Function<Shipment, Double> f) {
        List<Double> results = new ArrayList<>();
        for(Shipment s : l) {
            results.add(f.apply(s));
        }
        return results;
    }

}
