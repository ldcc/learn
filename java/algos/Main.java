import abbreviator.Abbreviator;
import altsplit.AltSplit;
import anagrams.Anagrams;
import bindservice.BindService;
import bitcounting.BitCounting;
import breadcrumb.Generating;
import camelcase.CameCaseMethod;
import columntitle.ColumnTitle;
import compiler.tiny.three.pass.CompilerTest;
import conwaylife.ConwayLife;
import deltabits.DeltaBits;
import dictionary.Dictionary;
import escaping.MatrixTest;
import esolang.interpreters.Paintfuck;
import esolang.interpreters.Smallfuck;
import factorials.LargeFactorials;
import fibonacci.Fibonacci;
import fibonacci.SumFct;
import histogram.Dinglemouse;
import interactive.interpreter.InterpreterTest;
import intpartitions.IntPart;
import ischeating.Cheating;
import morse.MorseCodeDecoder;
import multiples.Multiples;
import resistorcolor.ResistorColor;
import secret.detective.SecretDetective;
import sis.SquareIntoSquare;
import smallernumber.Smaller;
import srot.Srot;
import stringsmix.StringsMix;
import thsync.IOStuff;
import thsync.MyInput;
import thsync.MyOutput;
import twicelinear.TwiceLinear;
import unknowndigit.UnknownDigit;
import validbraces.ValidBraces;

import java.math.BigInteger;
import java.util.Arrays;

public class Main {
    public static void main(String[] args) {
//		for (int i = 1000; i < 10000; i++) fourCount(i);
//        testBindService();
//        testThsync();
//        testSortWord();
//        testAbbreviator();
//        testAltSplit();
//        testBitcounting();
//        testColumnTitle();
//        testMultiples();
//        testCameCase();
//        testDeltaBits();
//        testSquareIntoSquare();
//        testResistorColor();
//        testStringsMix();
//        testUnknownDigit();
//        testCheating();
//        testHistogram();
//        testAnagrams();
//        testDictionary();
//        testBitFactorials();
//        testMillionFib();
//        escapeTest();
//        testSmallerNum();
//        testGenerateBC();
//        testGlider();
//        testMorseAdv();
//        testValid();
//        testIntPart();
//        testTwiceLinear();
//        testSmallfuck();
//        testPaintfuck();
//        testBoolfuck();
        CompilerTest.testSimpleProg();
//        testInterpreter();
    }

    private static void testInterpreter() {
        InterpreterTest.basicTests();
        InterpreterTest.conflictsTests();
        InterpreterTest.functionsTests();
        InterpreterTest.variablesTests();
        InterpreterTest.interpreterTest();
    }

    private static void escapeTest() {
        new MatrixTest().test();
    }

    private static void fourCount(int num) {
        int[] arr = new int[4];
        arr[0] = num / 1000;
        arr[1] = num / 100 % 10;
        arr[2] = num / 10 % 100 % 10;
        arr[3] = num % 10;

        for (int i = 0; i < arr.length; i++) {
            for (int j = i + 1; j < arr.length; j++) {
                if (arr[i] < arr[j]) {
                    int t = arr[i];
                    arr[i] = arr[j];
                    arr[j] = t;
                }
            }
        }

        if (arr[0] * 1000 + arr[1] * 100 + arr[2] * 10 + arr[3] - arr[0] + arr[1] * 10 + arr[2] * 100 + arr[3] * 1000 == num) {
            System.out.println(num);
        }
    }

    private static void testBindService() {
        BindService.bind();
    }

    private static void testSortWord() {
        String words = "sort the inner content in descending order";
        String result = Srot.sortTheInnerContent(words);
        System.out.println(result);
    }

    private static void testAbbreviator() {
        Abbreviator abbr = new Abbreviator();
        String result;
        abbr.testAbbrTrim("elephant-rides are really fun!");
        result = abbr.testAbbr("elephant-rides are really fun!");
        System.out.println(result);
        result = abbr.abbreviate("elephant-rides are really fun!");
        System.out.println(result);
    }

    private static void testThsync() {
        IOStuff ios = new IOStuff();
        new Thread(new MyInput(ios)).start();
        new Thread(new MyOutput(ios)).start();
    }

    private static void testAltSplit() {
        System.out.println(AltSplit.encrypt("This is a reduce!", 0));
        System.out.println(AltSplit.encrypt("This is a reduce!", 1));
        System.out.println(AltSplit.encrypt("This is a reduce!", 2));
        System.out.println(AltSplit.encrypt("This is a reduce!", 3));
        System.out.println(AltSplit.encrypt("This is a reduce!", 4));
        System.out.println(AltSplit.encrypt("This is a reduce!", -1));
        System.out.println(AltSplit.decrypt("This is a reduce!", 0));
        System.out.println(AltSplit.decrypt("hsi  etTi sats!", 1));
        System.out.println(AltSplit.decrypt("s eT ashi tist!", 2));
        System.out.println(AltSplit.decrypt(" Tah itse sits!", 3));
        System.out.println(AltSplit.decrypt("This is a reduce!", 4));
        System.out.println(AltSplit.decrypt("This is a reduce!", -1));
    }

    private static void testBitcounting() {
        System.out.println(BitCounting.countBits(3));
        System.out.println(BitCounting.countBits(90));
        System.out.println(BitCounting.countBits(1234));
    }

    private static void testColumnTitle() {
        System.out.println(ColumnTitle.getColumnTitle(1));
        System.out.println(ColumnTitle.getColumnTitle(26));
        System.out.println(ColumnTitle.getColumnTitle(52));
        System.out.println(ColumnTitle.getColumnTitle(53));
        System.out.println(ColumnTitle.getColumnTitle(702));
        System.out.println(ColumnTitle.getColumnTitle(1337));
        System.out.println(ColumnTitle.getColumnTitle(432778));
    }

    private static void testMultiples() {
        System.out.println(new Multiples().solution(10));
        System.out.println(new Multiples().solution(50));
        System.out.println(new Multiples().solution(88));
    }

    private static void testCameCase() {
        System.out.println(CameCaseMethod.camelCase("reduce case"));
        System.out.println(CameCaseMethod.camelCase("camel case method"));
        System.out.println(CameCaseMethod.camelCase("say hello"));
        System.out.println(CameCaseMethod.camelCase("  hnn"));
        System.out.println(CameCaseMethod.camelCase(" jhemttil  hnt"));
        System.out.println(CameCaseMethod.camelCase(" fx w lh vk"));
        System.out.println(CameCaseMethod.camelCase(""));
    }

    private static void testDeltaBits() {
        System.out.println(DeltaBits.convertBits(7, 17));
        System.out.println(DeltaBits.convertBits(99, 45));
    }

    private static void testSquareIntoSquare() {
        System.out.println(new SquareIntoSquare().demo1("abc"));
        System.out.println(new SquareIntoSquare().demo1("kuyrqwisvbjs"));
        System.out.println(new SquareIntoSquare().demo2(7));
        System.out.println(new SquareIntoSquare().demo2(50));
        System.out.println(new SquareIntoSquare().demo2(625));
        System.out.println(new SquareIntoSquare().demo3(625));
        System.out.println(new SquareIntoSquare().demo2(7100));
        System.out.println(new SquareIntoSquare().demo3(7100));
    }

    private static void testResistorColor() {
        System.out.println(ResistorColor.encodeResistorColors2("10 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("47 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("100 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("220 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("330 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("470 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("680 ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("1k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("4.7k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("4.7M ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("10k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("22k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("47k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("100k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("330k ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("1M ohms"));
        System.out.println(ResistorColor.encodeResistorColors2("2M ohms"));
    }

    private static void testStringsMix() {
        System.out.println(StringsMix.mix("Are they here", "yes, they are here"));
        System.out.println(StringsMix.mix("looping is fun but dangerous", "less dangerous than coding"));
        System.out.println(StringsMix.mix(" In many languages", " there's a pair of functions"));
        System.out.println(StringsMix.mix("Lords of the Fallen", "gamekult"));
        System.out.println(StringsMix.mix("codewars", "codewars"));
        System.out.println(StringsMix.mix("A generation must confront the looming ", "codewarrs"));
    }

    private static void testUnknownDigit() {
        System.out.println(UnknownDigit.solveExpression("1+1=?"));
        System.out.println(UnknownDigit.solveExpression("123*45?=5?088"));
        System.out.println(UnknownDigit.solveExpression("-5?*-1=5?"));
        System.out.println(UnknownDigit.solveExpression("19--45=5?"));
        System.out.println(UnknownDigit.solveExpression("??*??=302?"));
        System.out.println(UnknownDigit.solveExpression("?*11=??"));
        System.out.println(UnknownDigit.solveExpression("123?45*?=?"));
        System.out.println(UnknownDigit.solveExpression("-7715?5--484?00=-28?9?5"));
    }

    private static void testCheating() {
        Cheating.removNb(26).forEach(longs -> Arrays.stream(longs).forEach(System.out::println));
    }

    private static void testHistogram() {
        System.out.println(Dinglemouse.histogram(new int[]{7, 3, 10, 1, 0, 5}));
        System.out.println(Dinglemouse.histogram(new int[]{8, 5, 10, 7, 9, 11}));
        System.out.println(Dinglemouse.histogram(new int[]{7, 10, 6, 8, 10, 9}));
        System.out.println(Dinglemouse.histogram(new int[]{0, 0, 0, 0, 0, 0}));
    }

    private static void testAnagrams() {
//        new Anagrams().listPosition("A");
//        new Anagrams().listPosition("ABAB");
//        System.out.println(new Anagrams().listPosition("BOOK"));
        System.out.println(new Anagrams().listPosition("ACBDE"));
//        System.out.println(new Anagrams().listPosition("BOOKKEEPERAHGSDJAGHSJAHGSD"));
    }

    private static void testDictionary() {
        Dictionary dictionary = new Dictionary(new String[]{
                "cherry", "pineapple", "melon", "strawberry", "raspberry",
                "javascript", "java", "ruby", "php", "python", "coffeescript",
                "wars", "codewars", "rkacypviuburk", "zqdrhpviqslik", "pdyjrkaylryr"});
        String[] inputs = new String[]{
                "strawbery",
                "berry",
                "heaven",
                "javascript",
                "coddwars",
                "rkacypviuburk"};
        for (String input : inputs) {
            String expectation = dictionary.findMostSimilar(input);
            System.out.println(input);
        }
//        Dictionary dictionary4 = new Dictionary(new String[]{"rkacypviuburk", "zqdrhpviqslik", "pdyjrkaylryr"});
//        dictionary4.debug();
    }

    private static void testBitFactorials() {
        System.out.println(LargeFactorials.Factorial(100));
        System.out.println(LargeFactorials.Factorial(10000));
    }

    private static void testMillionFib() {
        System.out.println(SumFct.perimeter(BigInteger.valueOf(5)));
        System.out.println(SumFct.perimeter(BigInteger.valueOf(7)));
//        System.out.println(SumFct.perimeter(BigInteger.valueOf(30)));
//        System.out.println(SumFct.perimeter(BigInteger.valueOf(14730)));
//        System.out.println(Fibonacci.fib(BigInteger.valueOf(7)));
//        System.out.println(Fibonacci.fib(BigInteger.valueOf(7)));
        System.out.println(Fibonacci.fib(BigInteger.valueOf(96)));
        System.out.println(Fibonacci.fib(BigInteger.valueOf(-96)));
        System.out.println(Fibonacci.fib(BigInteger.valueOf(1000000)));
    }

    private static void testSmallerNum() {
        Smaller.Smaller(21);
        Smaller.Smaller(907);
        Smaller.Smaller(123456789);
    }

    private static void testGenerateBC() {
        String[] urls = new String[]{
                "www.agcpartners.co.uk/",
                "www.agcpartners.co.uk",
                "https://pippi.pi/biotechnology-with-cauterization-to-and-by/index.htm?sortBy=year",
                "https://pippi.pi/biotechnology-with-cauterization-to-and-by/login.htm?sortBy=year",
                "http://www.facebook.fr/users/by-immunity-and-pippi-bioengineering/#bottom?previous=normalSearch&output=full"};
        String[] seps = new String[]{" : ", " / ", " * ", " > ", " + "};
        for (int i = 0; i < urls.length; i++) {
            String actual = Generating.generateBc(urls[i], seps[i]);
            System.out.println(actual);
        }
    }

    private static void testGlider() {
        int[][][] gliders = {{
                {1, 0, 0},
                {0, 1, 1},
                {1, 1, 0}
        }, {
                {0, 1, 0},
                {0, 0, 1},
                {1, 1, 1}}};
        int[][] res = ConwayLife.getGeneration(gliders[0], 1);
        Arrays.stream(res).forEach(System.out::println);
        Arrays.stream(gliders[1]).forEach(System.out::println);
    }

    private static void testMorseAdv() {
        String morseCode = MorseCodeDecoder.decodeBits("11111111111111100000000000000011111000001111100000111110000011111000000000000000111110000000000000000000000000000000000011111111111111100000111111111111111000001111100000111111111111111000000000000000111110000011111000001111111111111110000000000000001111100000111110000000000000001111111111111110000011111000001111111111111110000011111000000000000000111111111111111000001111100000111111111111111000000000000000000000000000000000001111111111111110000011111000001111100000111110000000000000001111100000111111111111111000001111100000000000000011111111111111100000111111111111111000001111111111111110000000000000001111100000111111111111111000001111111111111110000000000000001111111111111110000011111000000000000000000000000000000000001111100000111110000011111111111111100000111110000000000000001111111111111110000011111111111111100000111111111111111000000000000000111111111111111000001111100000111110000011111111111111100000000000000000000000000000000000111110000011111111111111100000111111111111111000001111111111111110000000000000001111100000111110000011111111111111100000000000000011111111111111100000111111111111111000000000000000111110000011111111111111100000111111111111111000001111100000000000000011111000001111100000111110000000000000000000000000000000000011111111111111100000111111111111111000001111111111111110000000000000001111100000111110000011111000001111111111111110000000000000001111100000000000000011111000001111111111111110000011111000000000000000000000000000000000001111111111111110000000000000001111100000111110000011111000001111100000000000000011111000000000000000000000000000000000001111100000111111111111111000001111100000111110000000000000001111100000111111111111111000000000000000111111111111111000001111111111111110000011111000001111100000000000000011111111111111100000111110000011111111111111100000111111111111111000000000000000000000000000000000001111111111111110000011111000001111100000000000000011111111111111100000111111111111111000001111111111111110000000000000001111111111111110000011111111111111100000111110000000000000001111100000111111111111111000001111100000111111111111111000001111100000111111111111111");
        String cipher = MorseCodeDecoder.decodeMorse(morseCode);
        System.out.println(cipher);
    }

    private static void testValid() {
//        System.out.println(ValidBraces.isValidBeta("({{[[]]}}"));
//        System.out.println(ValidBraces.isValidBeta("({[[]]})}"));
//        System.out.println(ValidBraces.isValidBeta("({()}"));
        System.out.println(ValidBraces.isValid("({{[[]]}}"));
        System.out.println(ValidBraces.isValid("([)]"));
    }

    private static void testIntPart() {
        System.out.println(IntPart.part(2));
        System.out.println(IntPart.part(3));
        System.out.println(IntPart.part(4));
//        System.out.println(IntPart.part(5));
    }

    private static void testTwiceLinear() {
        System.out.println(TwiceLinear.dblLinear(10));
        System.out.println(TwiceLinear.dblLinear(20));
        System.out.println(TwiceLinear.dblLinear(30));
        System.out.println(TwiceLinear.dblLinear(50));
    }

    private static void testSecretString() {
        char[][] triplets = {
                {'t', 'u', 'p'},
                {'w', 'h', 'i'},
                {'t', 's', 'u'},
                {'a', 't', 's'},
                {'h', 'a', 'p'},
                {'t', 'i', 's'},
                {'w', 'h', 's'}
        };
        new SecretDetective().recoverSecret(triplets);
    }

    private static void testSmallfuck() {
        System.out.println(Smallfuck.interpreter("*>*>*>*>*>*>*>*>*>*>*>*>*>*>*>*>*>*>", "00000000000000000000000000"));
        System.out.println(Smallfuck.interpreter("[[]*>*>*>]", "000"));
        System.out.println(Smallfuck.interpreter("[*>[>*>]>]", "11001"));
        System.out.println(Smallfuck.interpreter("[>[*>*>*>]>]", "10110"));
        System.out.println(Smallfuck.interpreter("*>*>>*>>>*>*", "00101100"));
        System.out.println(Smallfuck.interpreter("*[>*]", "000000000000000000000000000000000000000000000000000000000000000000000000000"));
        System.out.println(Smallfuck.interpreter("[>*]", "000000000000000000000000000000000000000000000000000000000000000000000000000"));
        System.out.println(Smallfuck.interpreter(("<<<*>*>*>*>*>>>*>>>>>*>*"), "0000000000000000"));
    }

    private static void testPaintfuck() {
        System.out.println(Paintfuck.interpreter("*[s[e]*]", 23, 5, 5));
    }

    private static void testBoolfuck() {

    }
}
