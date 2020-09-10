public class ExperimentalRemoteControlCar implements RemoteControlCar {
    int distanceTravelled;

    public void drive() {
        distanceTravelled += 20;
    }

    public int getDistanceTravelled() {
        return distanceTravelled;
    }
}
