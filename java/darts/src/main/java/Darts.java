class Darts {

    private final double x;
    private final double y;
    Darts(double x, double y) {
        this.x = x;
        this.y = y;
    }

    int score() {
        double hype = Math.sqrt(x*x + y*y);
        if (hype > 10)
            return 0;
        if (hype > 5)
            return 1;
        if (hype > 1)
            return 5;
        return 10;
    }

}
