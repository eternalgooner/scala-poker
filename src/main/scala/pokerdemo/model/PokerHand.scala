package pokerdemo.model

/**
 * @author David Mackessy
 * @date 17/05/2020
 **/
sealed abstract class PokerHand(val cards: List[Card])

case class RoyalFlush(override val cards: List[Card]) extends PokerHand(cards)
case class StraightFlush(override val cards: List[Card]) extends PokerHand(cards)
case class FourOfAKind(override val cards: List[Card]) extends PokerHand(cards)
case class FullHouse(override val cards: List[Card]) extends PokerHand(cards)
case class Flush(override val cards: List[Card]) extends PokerHand(cards)
case class Straight(override val cards: List[Card]) extends PokerHand(cards)
case class ThreeOfAKind(override val cards: List[Card]) extends PokerHand(cards)
case class TwoPair(override val cards: List[Card]) extends PokerHand(cards)
case class Pair(override val cards: List[Card]) extends PokerHand(cards)
case class HighCard(override val cards: List[Card]) extends PokerHand(cards)
