import java.util.*;


public class Solution {

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        char[] tabA = in.nextLine().toCharArray();
        char[] tabB = in.nextLine().toCharArray();
        
        int i = 0, j = 0;
        while (i < tabA.length && j < tabB.length) {
            if (tabA[i] != tabB[j]) {
                tabA[i] = '-';
            } else {
                j++;
            }
            i++;
        }
        // Right trailing
        while (i < tabA.length) tabA[i++] = '-';

        System.out.println(new String(tabA));
    }
}