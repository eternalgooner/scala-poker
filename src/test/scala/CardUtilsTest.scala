import org.scalatest.{FlatSpec, Matchers}
import pokerdemo.GameState
import pokerdemo.model._
import pokerdemo.utils.{CardUtils, HandGenerator}
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

  //TODO check for higher straight flush

  "A Four of a Kind hand" should "be of type FourOfAKind" in {
    val fourOfAKind: List[Card] = getFourOfAKind()
    val validatedFourOfAKind = CardUtils.getBestHand(fourOfAKind)
    println(s"Four of a Kind: $validatedFourOfAKind")

    assert(validatedFourOfAKind.isInstanceOf[FourOfAKind])
  }

  //TODO test for higher full house
  //e.g. 44455 vv 55522

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

  //TODO check for higher flush

  "A Straight hand" should "be of type Straight" in {
    val straight: List[Card] = getStraightStartingFrom(randomStartingStraightValue)
    val validatedStraight = CardUtils.getBestHand(straight)
    println(s"Straight: $validatedStraight")

    assert(validatedStraight.isInstanceOf[Straight])
  }

  //TODO check for higher straight

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
  "An Ace Low Straight hand" should "be of type Straight" in {
    val straight: List[Card] = getStraightStartingFrom(14)
    val validatedStraight = CardUtils.getBestHand(straight)
    println(s"Straight: $validatedStraight")

    assert(validatedStraight.isInstanceOf[Straight])
  }

  "A Three of a Kind hand" should "be of type ThreeOfAKind" in {
    val threeOfAKind: List[Card] = getThreeOfAKind()
    val validatedThreeOfAKind = CardUtils.getBestHand(threeOfAKind)
    println(s"Three of a Kind: $validatedThreeOfAKind")

    assert(validatedThreeOfAKind.isInstanceOf[ThreeOfAKind])
  }

  //TODO check for higher 3 of a kind

  "A Two of a Kind hand" should "be of type TwoOfAKind" in {
    val twoOfAKind: List[Card] = getTwoOfAKind()
    val validatedTwoOfAKind = CardUtils.getBestHand(twoOfAKind)
    println(s"Two of a Kind: $validatedTwoOfAKind")

    assert(validatedTwoOfAKind.isInstanceOf[Pair])
  }

  "A Two Pair hand" should "be of type TwoPair" in {
    val twoPair: List[Card] = getTwoPair()
    val validatedTwoPair = CardUtils.getBestHand(twoPair)
    println(s"Two Pair: $validatedTwoPair")

    assert(validatedTwoPair.isInstanceOf[TwoPair])
  }

  //TODO check for higher 2 of a kind

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

  "RoyalFlush" should "beat StraightFlush" in {
    val playerHand = RoyalFlush(List())
    val computerHand = StraightFlush(List())

    val result = CardUtils.compareBestHands(playerHand, computerHand)
    assert(result == 1)
  }

  "Pair" should "be equal to a Pair" in {
    val playerHand = Pair(List())
    val computerHand = Pair(List())

    val result = CardUtils.compareBestHands(playerHand, computerHand)
    assert(result == 0)
  }

  "Player High Card" should "beat computer lower card" in {
    val playerHand = List(Card(getRandomSuit(), 4),Card(getRandomSuit(), 9))
    val computerHand = List(Card(getRandomSuit(), 5), Card(getRandomSuit(), 8))

    val result = CardUtils.playerWithHighCard(playerHand, computerHand)
    assert(result.contains("Player"))
  }

  "Player Low Card" should "lose to Computer High card" in {
    val playerHand = List(Card(getRandomSuit(), 4),Card(getRandomSuit(), 9))
    val computerHand = List(Card(getRandomSuit(), 5), Card(getRandomSuit(), 13))

    val result = CardUtils.playerWithHighCard(playerHand, computerHand)
    assert(result.contains("Computer"))
  }

  "Three of a Kind" should "lose to a Full House" in {
    val playerHand = ThreeOfAKind(List())
    val computerHand = FullHouse(List())

    val result = CardUtils.compareBestHands(playerHand, computerHand)
    assert(result == -1)
  }

  val randomStartingStraightValue = Random.between(2, 10)

}
