class ArmstrongNumbers {

    boolean isArmstrongNumber(int numberToCheck) {

        var numDigits = (int) Math.log10(Math.abs(numberToCheck)) + 1;
        var result = String.valueOf(numberToCheck)
                .chars()
                .map(c -> (int) Math.pow(Character.digit(c, 10), numDigits))
                .reduce((a, b) -> a + b);

        return result.getAsInt() == numberToCheck;
    }

}
