import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.assertSame;

public class RemoteControlCarTest {
    @Test
    public void race() {
        ProductionRemoteControlCar productionCar = new ProductionRemoteControlCar();
        ExperimentalRemoteControlCar experimentalCar = new ExperimentalRemoteControlCar();
        TestTrack.Race(productionCar);
        TestTrack.Race(productionCar);
        TestTrack.Race(experimentalCar);
        TestTrack.Race(experimentalCar);
        assertSame(20, experimentalCar.getDistanceTravelled() - productionCar.getDistanceTravelled());
    }

//    @Ignore("Remove to run test")
    @Test
    public void rankCars() {
        ProductionRemoteControlCar prc1 = new ProductionRemoteControlCar();
        ProductionRemoteControlCar prc2 = new ProductionRemoteControlCar();
        prc1.setNumberOfVictories(3);
        prc2.setNumberOfVictories(2);
        List<ProductionRemoteControlCar> rankings = TestTrack.GetRankedCars(prc1, prc2);
        assertSame(prc1, rankings.get(1));
    }

//    @Ignore("Remove to run test")
    @Test
    public void ensureCarsAreComparable() {
        ProductionRemoteControlCar fast = new ProductionRemoteControlCar();
        ProductionRemoteControlCar medium = new ProductionRemoteControlCar();
        ProductionRemoteControlCar slow = new ProductionRemoteControlCar();
        fast.setNumberOfVictories(3);
        medium.setNumberOfVictories(2);
        slow.setNumberOfVictories(1);
        List<ProductionRemoteControlCar> cars = Arrays.asList(fast, slow, medium);
        Collections.sort(cars);
        assertSame(1, cars.get(0).getNumberOfVictories());
        assertSame(2, cars.get(1).getNumberOfVictories());
        assertSame(3, cars.get(2).getNumberOfVictories());
    }
}