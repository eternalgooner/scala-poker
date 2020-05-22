package pokerdemo.model

/**
 * @author David Mackessy
 * @date 17/05/2020
 **/
sealed abstract class PokerHand(val cards: List[Card], val value: Int)

case class RoyalFlush(override val cards: List[Card], override val value: Int = 10) extends PokerHand(cards, value)
case class StraightFlush(override val cards: List[Card], override val value: Int = 9) extends PokerHand(cards, value)
case class FourOfAKind(override val cards: List[Card], override val value: Int = 8) extends PokerHand(cards, value)
case class FullHouse(override val cards: List[Card], override val value: Int = 7) extends PokerHand(cards, value)
case class Flush(override val cards: List[Card], override val value: Int = 6) extends PokerHand(cards, value)
case class Straight(override val cards: List[Card], override val value: Int = 5) extends PokerHand(cards, value)
case class ThreeOfAKind(override val cards: List[Card], override val value: Int = 4) extends PokerHand(cards, value)
case class TwoPair(override val cards: List[Card], override val value: Int = 3) extends PokerHand(cards, value)
case class Pair(override val cards: List[Card], override val value: Int = 2) extends PokerHand(cards, value)
case class HighCard(override val cards: List[Card], override val value: Int = 1) extends PokerHand(cards, value)
