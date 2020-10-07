package clojure_practice;

import java.util.ArrayList;
import java.util.List;

public class Loopy {
    public static void main(String[] args) {
        for (int i = 2; i < 3; i++)
            System.out.println (i);

        System.out.println(Loopy.factorsOf(51));
    }

    public static List<Integer> factorsOf (int n) {
        ArrayList<Integer> factors = new ArrayList<>();
        for (int d = 2; n > 1; d++) {
            for (; n % d == 0; n /= d) 
                factors.add(d);
        }
        return factors;
    }
    
    public static List<Integer> factorsOf2 (int n) {

        return null;
    }

    
}