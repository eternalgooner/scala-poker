package pokerdemo.utils

import pokerdemo.model
import pokerdemo.model.Suit.Suit
import pokerdemo.model._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
 * @author David Mackessy
 * @date 12/05/2020
 **/
object CardUtils {

  def createSuit(suit: Suit): List[Card] = {
    //for each card value create a card for suit
    var suitCards: ArrayBuffer[Card] = ArrayBuffer[Card]()
    CardValue.values.foreach(value => suitCards += model.Card(suit, value))
    suitCards.toList
  }

  def createCardDeck() : List[List[Card]] = {
    val spades = createSuit(Suit.Spades)
    val clubs = createSuit(Suit.Clubs)
    val hearts = createSuit(Suit.Hearts)
    val diamonds = createSuit(Suit.Diamonds)

    spades :: clubs :: hearts :: diamonds :: Nil
  }

  def ifCardsMatch(card1: Card, card2: Card): Boolean = {
    val suitsMatch = card1.suit match {
      case card2.suit => true
      case _          => false
    }

    if (suitsMatch) {
      card1.value match {
        case card2.value  => return true
        case _            => return false
      }
    }
    false
  }

  def getRandomCard[A](seq: Seq[A], random: Random): A =
    seq(random.nextInt(seq.length))

  def formatCardForDisplay(card: Card):String = {
    val displayValue = card.value match {
      case numberCard if numberCard.id < 11  => numberCard.id
      case faceCard                          => faceCard
    }
    String.format(displayValue + " of " + card.suit)
  }

  def updateCardDeck(usedCard: Card, cardDeck: CardDeck): CardDeck = {
    //update used card by adding card to list
    val updatedUsedCards: List[Card] = cardDeck.usedCards ::: usedCard :: Nil

    //update cards remaining list by filtering card dealt
    val updatedRemainingCards = cardDeck.remainingCards.filter(!ifCardsMatch(_, usedCard))

    cardDeck.copy(usedCards = updatedUsedCards, remainingCards = updatedRemainingCards)
  }

  def isFaceCard(card: Card): Boolean = card.value.id > 9

  def getBestHand(sevenCards: List[Card]):PokerHand = {

    //######  check for royal flush  #######
    val flush = checkForFlush(sevenCards)
    val isFlush = flush.nonEmpty

    val royalFlush = flush match {
      case x :: tail  => checkForRoyalFlush(flush)
      case List()     => List()
    }
    if (!royalFlush.isEmpty) return RoyalFlush(royalFlush)


    //#####  check for straight flush  #####
    val flushForStraight = checkForFlush(sevenCards)
    val straightFlush = flushForStraight match {
      case x :: tail  => checkForStraightFlush(flushForStraight)
      case List()     => List()
    }
    if (straightFlush.nonEmpty) return StraightFlush(straightFlush)


    //#####  check for 4 of a kind  #####
    val fourOfAKindHand = checkForFourOfAKind(sevenCards)

    if (fourOfAKindHand.nonEmpty){
      return FourOfAKind(fourOfAKindHand)
    }


    //#####  check for full house  #####
    val fhTripsHand = checkForTrips(sevenCards)

    if (fhTripsHand.nonEmpty){
      val fhPairHand = checkForPair(sevenCards)
      if (fhPairHand.nonEmpty){
        return FullHouse(fhTripsHand ::: fhPairHand)
      }
    }


    //#####  check for flush  #####
    if (isFlush){
      val flushHand = checkForFlush(sevenCards)
      if (flushHand.nonEmpty) return Flush(flushHand)
    }


    //#####  check for straight  #####
    val straightHand = checkForStraight(sevenCards)
    if (straightHand.nonEmpty) return Straight(straightHand)


    //#####  check for 3 of a kind  #####
    val tripsHand = checkForTrips(sevenCards)
    if (tripsHand.nonEmpty){
      return ThreeOfAKind(tripsHand)
    }

    //#####  check for 2 pair  #####
    val twoPairHand = checkForTwoPair(sevenCards)
    if (twoPairHand.nonEmpty){
      return TwoPair(twoPairHand)
    }

    //#####  check for 2 of a kind  #####
    val pairHand = checkForPair(sevenCards)
    if (pairHand.nonEmpty){
      return Pair(pairHand)
    }

    //#####  high card  #####
    HighCard(sevenCards.max :: Nil)
  }

  def checkForRoyalFlush(flush: List[Card]):List[Card] = {
    val faceCards = for {
      card <- flush
      if (isFaceCard(card))
    } yield card
    if (faceCards.size == 5) faceCards else List()
  }

  def checkForFlush(cards: List[Card]):List[Card] = {
    val sevenSuits = cards.map(_.suit)
    val suitCountMap = sevenSuits.groupBy(identity).mapValues(_.size).toMap
    val suitFlush: Map[Suit, Int] = for {
      entry <- suitCountMap
      if entry._2 > 4
    } yield entry

    suitFlush match {
      case m:Map[Suit, Int] if m.isEmpty =>  List()
      case _                             =>  cards.filter(nonMatchingSuit(_, suitFlush))
    }
  }

  def checkForStraightFlush(cards: List[Card]):List[Card] = {
    val sortedDistinctValues = cards.map(_.value.id).distinct.sorted
    val isStraight = sortedDistinctValues.sliding(2).count(a => a(0)+1 == a(1))

    isStraight match {
      case count if count == 4  => cards
      case _                    => List()
    }
  }

  //TODO straight bug
  //when 5,6,7,8 10,11,12 returns true

  def getPossibleStraightSubsets(xs: List[Int]):List[List[Int]] = {
    xs.sliding(5,1).toList
  }

  def aceLowStraight(cards: List[Card]): Boolean = {
    val values: List[Int] = cards.map(_.value.id)
    List(14,2,3,4,5).forall(values.contains)
  }

  def checkForStraight(cards: List[Card]):List[Card] = {
    if (cards.size < 5) return List()
    if (aceLowStraight(cards)) return cards
    val sortedDistinctValues = cards.map(_.value.id).distinct.sorted
    val subsets = getPossibleStraightSubsets(sortedDistinctValues)

    //check for 4 consecutive incremental numbers after 1st num
    val isStraight: List[List[Int]] = for {
      subset <- subsets
      consecutiveIncrements = subset.sliding(2).count(a => a(0)+1 == a(1))
      if consecutiveIncrements == 4
    } yield subset

    if (isStraight.flatten.nonEmpty) {
      cards.filter(!isStraight.flatten.contains(_) )
    }else List()
  }

  def checkForFourOfAKind(cards: List[Card]):List[Card] = {
    val sevenValues = cards.map(_.value.id)
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap

    val fourOfAKind: Map[Int, Int] = for {
      entry <- valueCountMap
      if entry._2 == 4
    } yield entry

    fourOfAKind match {
      case m:Map[Int, Int] if m.isEmpty =>  List()
      case m:Map[Int, Int]              =>  cards.filter(_.value.id == m.head._1)
    }
  }

  def checkForTrips(cards: List[Card]):List[Card] = {
    val sevenValues = cards.map(_.value.id)
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap

    val trips: Map[Int, Int] = for {
      entry <- valueCountMap
      if entry._2 == 3
    } yield entry

    trips match {
      case m:Map[Int, Int] if m.isEmpty =>  List()
      case m:Map[Int, Int]              =>  cards.filter(_.value.id == m.head._1)
    }
  }

  def checkForTwoPair(cards: List[Card]):List[Card] = {
    val pair = checkForPair(cards)
    if (pair.isEmpty) { return List()}
    val pairValue = pair(0).value.id
    val nextPairCheck = cards.filter(_.value.id != pairValue)
    val secondPair = checkForPair(nextPairCheck)
    if (secondPair.nonEmpty){
      pair ::: secondPair
    }else{
      List()
    }
  }

  def checkForPair(cards: List[Card]):List[Card] = {
    val sevenValues = cards.map(_.value.id)
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap

    val pair: Map[Int, Int] = for {
      entry <- valueCountMap
      if entry._2 == 2
    } yield entry

    pair match {
      case m:Map[Int, Int] if m.isEmpty =>  List()
      case m:Map[Int, Int]              =>  cards.filter(_.value.id == m.head._1)
    }
  }

  def getSuitFromMap(suitMap: Map[Suit, Int]): Suit = {
    suitMap.keySet.head
  }

  def nonMatchingSuit(card: Card, suitMap: Map[Suit, Int]): Boolean = {
    val suit: Suit = getSuitFromMap(suitMap)
    card.suit == suit
  }

  def compareBestHands (playerHand: PokerHand, computerHand: PokerHand): Int = {
    playerHand.value.compare(computerHand.value)
  }

  def playerWithHighCard(playerCards: List[Card], computerCards: List[Card]): String = {
    val playerHighCard = playerCards.max
    val computerHighCard = computerCards.max
    if (playerHighCard > computerHighCard) {
      s"Player with $playerHighCard"
    }
    else if (playerHighCard < computerHighCard) {
      s"Computer with $computerHighCard"
    }
    else {
      "Split Pot"
    }
  }

}
