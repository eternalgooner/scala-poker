import org.scalatest.{FlatSpec, Matchers}
import pokerdemo.model._
import pokerdemo.utils.CardUtils
import pokerdemo.utils.HandGenerator._

import scala.util.Random

/**
 * @author David Mackessy
 * @date 17/05/2020
 **/
class CardUtilsTest extends FlatSpec with Matchers{

  "A Royal Flush hand" should "be of type RoyalFlush" in {
    val royalFlush: List[Card] = getRoyalFlush(getRandomSuit())
    val validatedRoyalFlush = CardUtils.getBestHand(royalFlush)
    println(s"Royal Flush: $validatedRoyalFlush")

    assert(validatedRoyalFlush.isInstanceOf[RoyalFlush])

  }

  "A Royal Flush hand" should "contain only Face cards" in {
    val royalFlush: List[Card] = getRoyalFlush(getRandomSuit())
    val validatedRoyalFlush = CardUtils.getBestHand(royalFlush)
    println(s"Royal Flush: $validatedRoyalFlush")

    val result = validatedRoyalFlush.cards.filter(CardUtils.isFaceCard(_))
    assert(result.size === 5)

  }

  "A Straight Flush hand" should "be of type StraightFlush" in {
    val straightFlush: List[Card] = getStraightFlushStartingFrom(randomStartingStraightValue)
    val validatedStraightFlush = CardUtils.getBestHand(straightFlush)
    println(s"Straight: $validatedStraightFlush")

    assert(validatedStraightFlush.isInstanceOf[StraightFlush])
  }

  "A Four of a Kind hand" should "be of type FourOfAKind" in {
    val fourOfAKind: List[Card] = getFourOfAKind()
    val validatedFourOfAKind = CardUtils.getBestHand(fourOfAKind)
    println(s"Four of a Kind: $validatedFourOfAKind")

    assert(validatedFourOfAKind.isInstanceOf[FourOfAKind])
  }

  "A Full House hand" should "be of type FullHouse" in {
    val fullHouse: List[Card] = getFullHouse()
    val validatedFullHouse = CardUtils.getBestHand(fullHouse)
    println(s"Full House: $validatedFullHouse")

    assert(validatedFullHouse.isInstanceOf[FullHouse])
  }

  "A Flush hand" should "be of type Flush" in {
    val flush: List[Card] = getFlush(getRandomSuit())
    val validatedFlush = CardUtils.getBestHand(flush)
    println(s"Flush: $validatedFlush")

    assert(validatedFlush.isInstanceOf[Flush])
  }

  "A Straight hand" should "be of type Straight" in {
    val straight: List[Card] = getStraightStartingFrom(randomStartingStraightValue)
    val validatedStraight = CardUtils.getBestHand(straight)
    println(s"Straight: $validatedStraight")

    assert(validatedStraight.isInstanceOf[Straight])
  }

  "A Non Straight hand" should "be of type HighCard" in {
    val nonStraight: List[Card] = List(
      Card(Suit.Spades, 3),
      Card(Suit.Clubs, 4),
      Card(Suit.Diamonds, 5),
      Card(Suit.Hearts, 6),
      Card(getRandomSuit(), 8),
      Card(getRandomSuit(), 9),
      Card(getRandomSuit(), 10),
    )
    val validatedStraight = CardUtils.getBestHand(nonStraight)

    assert(validatedStraight.isInstanceOf[HighCard])
  }

  //TODO low straight bug - create test
  //when A,2,3,4 5,11,12

  "A Three of a Kind hand" should "be of type ThreeOfAKind" in {
    val threeOfAKind: List[Card] = getThreeOfAKind()
    val validatedThreeOfAKind = CardUtils.getBestHand(threeOfAKind)
    println(s"Three of a Kind: $validatedThreeOfAKind")

    assert(validatedThreeOfAKind.isInstanceOf[ThreeOfAKind])
  }

  "A Two of a Kind hand" should "be of type TwoOfAKind" in {
    val twoOfAKind: List[Card] = getTwoOfAKind()
    val validatedTwoOfAKind = CardUtils.getBestHand(twoOfAKind)
    println(s"Two of a Kind: $validatedTwoOfAKind")

    assert(validatedTwoOfAKind.isInstanceOf[Pair])
  }

  "A High Card hand" should "be of type HighCard" in {
    val highCard: List[Card] = getHighCard()
    val validatedHighCard = CardUtils.getBestHand(highCard)
    println(s"High Card: $validatedHighCard")

    assert(validatedHighCard.isInstanceOf[HighCard])
  }

  "All Face cards" should "be considered a Face card" in {
    val allCardValues: List[Card] = getAllCardValues()

    val result = allCardValues.filter(CardUtils.isFaceCard(_))
    assert(result.size === 5)
  }

  val randomStartingStraightValue = Random.between(2, 10)

  //test full house with different combos to make sure choosing highest card
  //e.g. 33 35666
  //e.g. 22 44666
}
