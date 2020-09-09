import org.junit.Assert;
import org.junit.Test;

public class RemoteControlCarTest {

    [Fact]

    public void Race() {
        ProductionRemoteControlCar productionCar = new ProductionRemoteControlCar();
        ProductionRemoteControlCar experimentalCar = new ExperimentalRemoteControlCar();
        TestTrack.Race(productionCar);
        TestTrack.Race(productionCar);
        TestTrack.Race(experimentalCar);
        TestTrack.Race(experimentalCar);
        Assert.Equal(20, experimentalCar.getDistanceTravelled() - productionCar.getDistanceTravelled());
    }

        [

    Fact(Skip ="Remove this Skip property to run this test")]

    public void RankCars() {
        var prc1 = new ProductionRemoteControlCar();
        var prc2 = new ProductionRemoteControlCar();
        prc1.NumberOfVictories = 3;
        prc2.NumberOfVictories = 2;
        var rankings = TestTrack.GetRankedCars(prc1, prc2);
        Assert.Same(prc1, rankings[1]);
    }

        [

    Fact(Skip ="Remove this Skip property to run this test")]

    public void EnsureCarsAreComparable() {
        var fast = new ProductionRemoteControlCar();
        var medium = new ProductionRemoteControlCar();
        var slow = new ProductionRemoteControlCar();
        fast.NumberOfVictories = 3;
        medium.NumberOfVictories = 2;
        slow.NumberOfVictories = 1;
        var cars = new List<ProductionRemoteControlCar> {
            fast, slow, medium
        } ;
        cars.Sort();
        Assert.Equal(new ProductionRemoteControlCar[]{slow, medium, fast}, cars);
    }
}