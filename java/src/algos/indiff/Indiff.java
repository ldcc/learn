package org.ldccc.algos.indiff;

public class Indiff {


    /* All char "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567980.,:;-?! '()$%&" + "" = 77 chars;

     char1 = char0 - char1, new char1 replace the old char1, until the end;
     char0 = 77 - char0, new char1 replace the old char1;
     for example:
     Input:  "Business"
     Step 1: "BUsInEsS"
     Step 2: "B61kujla"
     B -> U
     B (1) - U (20) = -19
     -19 + 77 = 58
     Region[58] = "6"
     U -> s
     U (20) - s (44) = -24
     -24 + 77 = 53
     Region[53] = "1"
     Step 3: "&61kujla"
     */

    public static String Encrypt(String text) {
        return text;
    }

    public static String Decrypt(String encryptedText) {
        return encryptedText;
    }
}
