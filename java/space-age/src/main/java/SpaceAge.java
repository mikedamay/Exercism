class SpaceAge {

    private final double DAYS_IN_EARTH_YEAR = 365.25d;
    private final double SECONDS_IN_EARTH_YEAR = 31557600d;
    private final double MERCURY_RATIO = 0.2408467;
    private final double VENUS_RATIO = 0.61519726;
    private final double MARS_RATIO = 1.8808158;
    private final double JUPITER_RATIO = 11.862615;
    private final double SATURN_RATIO = 29.447498;
    private final double URANUS_RATIO = 84.016846;
    private final double NEPTUNE_RATIO = 164.79132;

    private final double seconds;

    SpaceAge(double seconds) {
        this.seconds = seconds;
    }

    double getSeconds() {
        return seconds;
    }

    double onEarth() {
        return seconds / SECONDS_IN_EARTH_YEAR;
    }

    double onMercury() {
        return (seconds / MERCURY_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onVenus() {
        return (seconds / VENUS_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onMars() {
        return (seconds / MARS_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onJupiter() {
        return (seconds / JUPITER_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onSaturn() {
        return (seconds / SATURN_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onUranus() {
        return (seconds / URANUS_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

    double onNeptune() {
        return (seconds / NEPTUNE_RATIO) / SECONDS_IN_EARTH_YEAR;
    }

}
