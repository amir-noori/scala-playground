package code.playground.challanges.projectEuler;

import java.util.Arrays;

public class JavaUtils {

    public static int[] getPrimeNumbersArrayBySieveMethod(int n) {

        int[] primes = new int[n + 1];
        int[] numbers = new int[n + 1];
        numbers[0] = 0;
        numbers[1] = 0;
        for (int i = 2; i <= n; i++) {
            numbers[i] = i;
        }

        int currentPrime = 2;
        int currentIndex = 2;
        int nextPrime = 0;
        int primeIndex = 0;
        boolean nextChosen = false;
        boolean done = true;
        while (done) {
            for (int i = currentIndex + 1; i <= n; i++) {
                if (numbers[i] != 0) {
                    if (numbers[i] % currentPrime == 0) {
                        numbers[i] = 0;
                    } else if (!nextChosen) {
                        nextPrime = numbers[i];
                        currentIndex = i;
                        nextChosen = true;
                    }
                }
            }
            if (nextPrime != 0 && nextPrime != currentPrime) {
                primes[primeIndex] = currentPrime;
                currentPrime = nextPrime;
                primeIndex++;
                nextChosen = false;
            } else {
                primes[primeIndex] = currentPrime;
                done = false;
            }
        }

        return primes;
    }


}
