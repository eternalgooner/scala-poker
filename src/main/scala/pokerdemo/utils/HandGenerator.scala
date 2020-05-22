package pokerdemo.utils

import pokerdemo.model.{Card, CardValue, Suit}
import pokerdemo.model.Suit.Suit

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author David Mackessy
 * @date 17/05/2020
 **/
object HandGenerator {

  def getRoyalFlush(suit: Suit):List[Card] = {
    List(
      Card(suit, CardValue.A),
      Card(suit, CardValue.K),
      Card(suit, CardValue.Q),
      Card(suit, CardValue.J),
      Card(suit, CardValue.Ten),
      Card(getDifferentSuit(suit), CardValue.Three),
      Card(getDifferentSuit(suit), CardValue.Five)
    )
  }

  def getFlush(suit: Suit):List[Card] = {
    List(
      Card(suit, CardValue.Four),
      Card(suit, CardValue.Ten),
      Card(suit, CardValue.Q),
      Card(suit, CardValue.Nine),
      Card(suit, CardValue.Two),
      Card(getDifferentSuit(suit), CardValue.Three),
      Card(getDifferentSuit(suit), CardValue.Five)
    )
  }

  def getDifferentSuit(suit: Suit): Suit = {
    val allSuits = getAllSuits()
    val differentSuit = allSuits.filter(_ != suit)
    differentSuit(Random.nextInt(3))
  }

  def getAllSuits(): List[Suit] = List(Suit.Clubs, Suit.Diamonds, Suit.Hearts, Suit.Spades)

  def getFaceCards(): List[Card] = {
    List(
      Card(getRandomSuit, CardValue.Ten),
      Card(getRandomSuit, CardValue.J),
      Card(getRandomSuit, CardValue.Q),
      Card(getRandomSuit, CardValue.K),
      Card(getRandomSuit, CardValue.A)
    )
  }

  def getRandomSuit():Suit = {
    val random = Random.nextInt(4)
    val suits = getAllSuits()
    suits(random)
  }

  def getNonFaceCards(): List[Card] = {
    List(
      Card(getRandomSuit, CardValue.Two),
      Card(getRandomSuit, CardValue.Three),
      Card(getRandomSuit, CardValue.Four),
      Card(getRandomSuit, CardValue.Five),
      Card(getRandomSuit, CardValue.Six),
      Card(getRandomSuit, CardValue.Seven),
      Card(getRandomSuit, CardValue.Eight),
      Card(getRandomSuit, CardValue.Nine),
    )
  }

  def getAllCardValues(): List[Card] = List(getFaceCards(), getNonFaceCards()).flatten

  def getStraightFlushStartingFrom(cardValue: Int): List[Card] = {
    var straight = ArrayBuffer[Card]()
    val flushSuit = getRandomSuit();

    cardValue to (cardValue + 4) foreach {
      cardValue => straight += Card(flushSuit, cardValue)
    }

    straight.toList
  }

  def getStraightStartingFrom(cardValue: Int): List[Card] = {
    cardValue match {
      case 14 => return List(
        Card(getRandomSuit(), 14),
        Card(getRandomSuit(), 2),
        Card(getRandomSuit(), 3),
        Card(getRandomSuit(), 4),
        Card(getRandomSuit(), 5),
        Card(getRandomSuit(), 9),
        Card(getRandomSuit(), 11),
      )
      case _ =>
    }

    var straight = ArrayBuffer[Card]()

    cardValue to (cardValue + 4) foreach {
      cardValue => straight += Card(getRandomSuit(), cardValue)
    }

    val randomCard = Card(Suit.Hearts, 4)
    val randomCard2 = Card(Suit.Diamonds, 13)
    straight.toList ::: randomCard :: randomCard2 :: Nil
  }

  def getFullHouse(): List[Card] = {
    val val1 = Random.between(2,15)
    val val2 = Random.between(2,15)
    List(
      Card(Suit.Diamonds, val1),
      Card(Suit.Hearts, val1),
      Card(Suit.Clubs, val1),
      Card(Suit.Spades, val2),
      Card(Suit.Clubs, val2),
    )
  }

  def getFourOfAKind(): List[Card] = {
    val val1 = Random.between(2, 15)
    List(
      Card(Suit.Diamonds, val1),
      Card(Suit.Hearts, val1),
      Card(Suit.Clubs, val1),
      Card(Suit.Spades, val1),
    )
  }

  def getThreeOfAKind(): List[Card] = {
    val val1 = Random.between(2,15)
    List(
      Card(Suit.Diamonds, val1),
      Card(Suit.Hearts, val1),
      Card(Suit.Clubs, val1),
    )
  }

  def getTwoPair(): List[Card] = {
    List(getTwoOfAKind(), getTwoOfAKind()).flatten
  }

  def getTwoOfAKind(): List[Card] = {
    val val1 = Random.between(2, 15)
    List(
      Card(Suit.Diamonds, val1),
      Card(Suit.Hearts, val1),
    )
  }

  def getHighCard(): List[Card] = {
    val val1 = Random.between(2, 15)
    List(Card(getRandomSuit(), val1))
  }
}
