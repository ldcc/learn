package org.ldccc.algos.poker;

public class PokerHand {

    public enum Result {TIE, WIN, LOSS}

    PokerHand(String hand) {
    }

    public Result compareWith(PokerHand hand) {

        return Result.TIE;
    }
}
