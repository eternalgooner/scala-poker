import org.scalatest.{FlatSpec, FunSuite, Matchers}
import pokerdemo.model.{Card, CardValue, Flush, FourOfAKind, FullHouse, HighCard, Pair, RoyalFlush, Straight, StraightFlush, Suit, ThreeOfAKind}
import pokerdemo.utils.{CardUtils, HandGenerator}

import scala.util.Random

/**
 * @author David Mackessy
 * @date 17/05/2020
 **/
class CardUtilsTest extends FlatSpec with Matchers{

  "A Royal Flush hand" should "be of type RoyalFlush" in {
    val royalFlush: List[Card] = HandGenerator.getRoyalFlush(HandGenerator.getRandomSuit())
    val validatedRoyalFlush = CardUtils.getBestHand(royalFlush)
    println(s"Royal Flush: $validatedRoyalFlush")

    assert(validatedRoyalFlush.isInstanceOf[RoyalFlush])

  }

  "A Royal Flush hand" should "contain only Face cards" in {
    val royalFlush: List[Card] = HandGenerator.getRoyalFlush(HandGenerator.getRandomSuit())
    val validatedRoyalFlush = CardUtils.getBestHand(royalFlush)
    println(s"Royal Flush: $validatedRoyalFlush")

    val result = validatedRoyalFlush.cards.filter(CardUtils.isFaceCard(_))
    assert(result.size === 5)

  }

  "A Straight Flush hand" should "be of type StraightFlush" in {
    val straightFlush: List[Card] = HandGenerator.getStraightFlushStartingFrom(randomStartingStraightValue)
    val validatedStraightFlush = CardUtils.getBestHand(straightFlush)
    println(s"Straight: $validatedStraightFlush")

    assert(validatedStraightFlush.isInstanceOf[StraightFlush])
  }

  "A Four of a Kind hand" should "be of type FourOfAKind" in {
    val fourOfAKind: List[Card] = HandGenerator.getFourOfAKind()
    val validatedFourOfAKind = CardUtils.getBestHand(fourOfAKind)
    println(s"Four of a Kind: $validatedFourOfAKind")

    assert(validatedFourOfAKind.isInstanceOf[FourOfAKind])
  }

  "A Full House hand" should "be of type FullHouse" in {
    val fullHouse: List[Card] = HandGenerator.getFullHouse()
    val validatedFullHouse = CardUtils.getBestHand(fullHouse)
    println(s"Full House: $validatedFullHouse")

    assert(validatedFullHouse.isInstanceOf[FullHouse])
  }

  "A Flush hand" should "be of type Flush" in {
    val flush: List[Card] = HandGenerator.getFlush(HandGenerator.getRandomSuit())
    val validatedFlush = CardUtils.getBestHand(flush)
    println(s"Flush: $validatedFlush")

    assert(validatedFlush.isInstanceOf[Flush])
  }

  "A Straight hand" should "be of type Straight" in {
    val straight: List[Card] = HandGenerator.getStraightStartingFrom(randomStartingStraightValue)
    val validatedStraight = CardUtils.getBestHand(straight)
    println(s"Straight: $validatedStraight")

    assert(validatedStraight.isInstanceOf[Straight])
  }

  //straight bug
  //when 5,6,7,8 10,11

  "A Three of a Kind hand" should "be of type ThreeOfAKind" in {
    val threeOfAKind: List[Card] = HandGenerator.getThreeOfAKind()
    val validatedThreeOfAKind = CardUtils.getBestHand(threeOfAKind)
    println(s"Three of a Kind: $validatedThreeOfAKind")

    assert(validatedThreeOfAKind.isInstanceOf[ThreeOfAKind])
  }

  "A Two of a Kind hand" should "be of type TwoOfAKind" in {
    val twoOfAKind: List[Card] = HandGenerator.getTwoOfAKind()
    val validatedTwoOfAKind = CardUtils.getBestHand(twoOfAKind)
    println(s"Two of a Kind: $validatedTwoOfAKind")

    assert(validatedTwoOfAKind.isInstanceOf[Pair])
  }

  "A High Card hand" should "be of type HighCard" in {
    val highCard: List[Card] = HandGenerator.getHighCard()
    val validatedHighCard = CardUtils.getBestHand(highCard)
    println(s"High Card: $validatedHighCard")

    assert(validatedHighCard.isInstanceOf[HighCard])
  }

  "All Face cards" should "be considered a Face card" in {
    val allCardValues: List[Card] = HandGenerator.getAllCardValues()

    val result = allCardValues.filter(CardUtils.isFaceCard(_))
    assert(result.size === 5)
  }

  val randomStartingStraightValue = Random.between(2, 10)

  //test full house with different combos to make sure choosing highest card
  //e.g. 33 35666
  //e.g. 22 44666
}
