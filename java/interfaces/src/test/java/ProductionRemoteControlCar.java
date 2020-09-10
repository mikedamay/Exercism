class ProductionRemoteControlCar implements RemoteControlCar, Comparable<ProductionRemoteControlCar>
{
    int distanceTravelled;
    int numberofFictories;

    public int compareTo(ProductionRemoteControlCar other) {
        return Integer.compare(this.getNumberOfVictories(), other.getNumberOfVictories());
    }

    public void drive() {
        distanceTravelled += 10;
    }

    public int getDistanceTravelled() {
        return distanceTravelled;
    }

    public int getNumberOfVictories() {
        return numberofFictories;
    }

    public void setNumberOfVictories(int numberofFictories) {
        this.numberofFictories = numberofFictories;
    }

}
