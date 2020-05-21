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
    println(s"back from checkForFlush: $flush")
    val isFlush = flush.nonEmpty

    val royalFlush = flush match {
      case x :: tail  => checkForRoyalFlush(flush)
      case List()     => List()
    }
    if (!royalFlush.isEmpty) return RoyalFlush(royalFlush)


    //#####  check for straight flush  #####
    val flushForStraight = checkForFlush(sevenCards)
    println(s"back from checkForFlush: $flushForStraight")
    val straightFlush = flushForStraight match {
      case x :: tail  => checkForStraightFlush(flushForStraight)
      case List()     => List()
    }
    if (straightFlush.nonEmpty) return StraightFlush(straightFlush)


    //#####  check for 4 of a kind  #####
    val fourOfAKindHand = checkForFourOfAKind(sevenCards)
    println(s"back from checkForFourOfAKind: $fourOfAKindHand")

    if (fourOfAKindHand.nonEmpty){
      return FourOfAKind(fourOfAKindHand)
    }


    //#####  check for full house  #####
    val fhTripsHand = checkForTrips(sevenCards)
    println(s"back from fh trips: $fhTripsHand")

    if (fhTripsHand.nonEmpty){
      val fhPairHand = checkForPair(sevenCards)
      println(s"back from fh pair: $fhPairHand")
      if (fhPairHand.nonEmpty){
        return FullHouse(fhTripsHand ::: fhPairHand)
      }
    }


    //#####  check for flush  #####
    if (isFlush){
      val flushHand = checkForFlush(sevenCards)
      println(s"back from checkForFlush: $flushHand")
      if (flushHand.nonEmpty) return Flush(flushHand)
    }


    //#####  check for straight  #####
    val straightHand = checkForStraight(sevenCards)
    if (straightHand.nonEmpty) return Straight(straightHand)


    //#####  check for 3 of a kind  #####
    val tripsHand = checkForTrips(sevenCards)
    println(s"back from trips: $tripsHand")
    if (tripsHand.nonEmpty){
      return ThreeOfAKind(tripsHand)
    }

    //#####  check for 2 pair  #####


    //#####  check for 2 of a kind  #####
    val pairHand = checkForPair(sevenCards)
    println(s"back from pair: $pairHand")
    if (pairHand.nonEmpty){
      return Pair(pairHand)
    }

    //#####  high card  #####
    println(s"returning High Card as no match: ${sevenCards.max}")
    HighCard(sevenCards.max :: Nil)
  }

  def checkForRoyalFlush(flush: List[Card]):List[Card] = {
    println("\nchecking for Royal Flush")
    val faceCards = for {
      card <- flush
      if (isFaceCard(card))
    } yield card
    if (faceCards.size == 5) faceCards else List()
  }

  def checkForFlush(cards: List[Card]):List[Card] = {
    println("\nchecking for Flush")
    val sevenSuits = cards.map(_.suit)
    val suitCountMap = sevenSuits.groupBy(identity).mapValues(_.size).toMap
    val suitFlush: Map[Suit, Int] = for {
      entry <- suitCountMap
      if entry._2 > 4
    } yield entry

    println(suitCountMap)
    println(suitFlush)
    //println("this is what's going back:" + cards.filter(nonMatchingSuit(_, suitFlush)))

    suitFlush match {
      case m:Map[Suit, Int] if m.isEmpty =>  List()
      case _                             =>  cards.filter(nonMatchingSuit(_, suitFlush))
    }
  }

  def checkForStraightFlush(cards: List[Card]):List[Card] = {
    println(s"\nchecking for Straight Flush with $cards")
    val sortedDistinctValues = cards.map(_.value.id).distinct.sorted
    println(s"sorted distinct values: $sortedDistinctValues")

    val isStraight = sortedDistinctValues.sliding(2).count(a => a(0)+1 == a(1))
    println(s"isStraight: $isStraight")

    isStraight match {
      case count if count == 4  => cards
      case _                    => List()
    }
  }

  //TODO straight bug
  //when 5,6,7,8 10,11,12 returns true

  def getPossibleStraightSubsets(xs: List[Int]):List[List[Int]] = {
    val subset1 = xs.slice(0,5)
    val subset2 = xs.slice(1,6)
    val subset3 = xs.slice(2,7)

    List(subset1, subset2, subset3)
  }

  def checkForStraight (cards: List[Card]):List[Card] = {
    if (cards.size < 6) return List()
    val sortedDistinctValues = cards.map(_.value.id).distinct.sorted
    val subsets = getPossibleStraightSubsets(sortedDistinctValues)

    //check for 4 consecutive incremental numbers after 1st num
    val isStraight: List[List[Int]] = for {
      subset <- subsets
      consecutiveIncrements = subset.sliding(2).count(a => a(0)+1 == a(1))
      if consecutiveIncrements == 4
    } yield subset

    //isStraight.flatten
    if (isStraight.flatten.nonEmpty) {
      cards.filter(!isStraight.flatten.contains(_) )
    }else List()
  }

  def checkForFourOfAKind(cards: List[Card]):List[Card] = {
    println(s"\nchecking for Four of a Kind with $cards")
    val sevenValues = cards.map(_.value.id)
    println(s"values: $sevenValues")
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap
    println(s"value map: $valueCountMap")

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
    println(s"\nchecking for Trips with $cards")
    val sevenValues = cards.map(_.value.id)
    println(s"values: $sevenValues")
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap
    println(s"value map: $valueCountMap")

    val trips: Map[Int, Int] = for {
      entry <- valueCountMap
      if entry._2 == 3
    } yield entry

    trips match {
      case m:Map[Int, Int] if m.isEmpty =>  List()
      case m:Map[Int, Int]              =>  cards.filter(_.value.id == m.head._1)
    }
  }

  def checkForPair(cards: List[Card]):List[Card] = {
    println(s"\nchecking for Pair with $cards")
    val sevenValues = cards.map(_.value.id)
    println(s"values: $sevenValues")
    val valueCountMap:Map[Int, Int] = sevenValues.groupBy(identity).mapValues(_.size).toMap
    println(s"value map: $valueCountMap")

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
    println(s"suitMap: $suitMap")
    val suit: Suit = getSuitFromMap(suitMap)
    println(s"checking $card v $suit")
    card.suit == suit
  }
}
