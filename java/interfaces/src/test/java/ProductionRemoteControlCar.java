class ProductionRemoteControlCar implements RemoteControlCar, Comparable<ProductionRemoteControlCar>
{
    int distanceTravelled;
    int numberofFictories;

    @Override
    public int compareTo(ProductionRemoteControlCar other) {
        return Integer.compare(this.getDistanceTravelled(), other.getDistanceTravelled());
    }

    @Override
    public void drive() {
        distanceTravelled += 10;
    }

    @Override
    public int getDistanceTravelled() {
        return distanceTravelled;
    }

    public int getNumberofFictories() {
        return numberofFictories;
    }

    public void setNumberofFictories(int numberofFictories) {
        this.numberofFictories = numberofFictories;
    }

}
