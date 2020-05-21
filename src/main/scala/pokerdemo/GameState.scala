package pokerdemo

import pokerdemo.utils.CardUtils.formatCardForDisplay
import pokerdemo.model.{Card, CardDeck}
import pokerdemo.utils.CardUtils

import scala.util.Random

/**
 * @author David Mackessy
 * @date 13/05/2020
 **/
case class GameState(playerCards: List[Card], computerCards: List[Card], communityCards: List[Card], cardDeck: CardDeck) {
  def dealCardsToPlayers(gameState: GameState): GameState = {
    //deal 2 cards to each player
    val cardDeck1 = gameState.cardDeck
    println(s"starting cardDeck: $cardDeck1")

    val playerCard1 = CardUtils.getRandomCard(cardDeck.cards, Random)
    val cardDeck2 = CardUtils.updateCardDeck(playerCard1, cardDeck)
    val gameStateUpdate1 = gameState.copy(playerCards = List(playerCard1), cardDeck = cardDeck2)
    //    println(s"\ngameStateUpdate after 1st card dealt: player cards: ${gameStateUpdate1.playerCards}, comp cards: ${gameStateUpdate1.computerCards}")
    //    println(s"gameStateUpdate after 1st card dealt: used cards: ${gameStateUpdate1.cardDeck.usedCards}, remaining cards: ${gameStateUpdate1.cardDeck.remainingCards}")

    val computerCard1 = CardUtils.getRandomCard(cardDeck2.remainingCards, Random)
    val cardDeck3 = CardUtils.updateCardDeck(computerCard1, cardDeck2)
    val gameStateUpdate2 = gameStateUpdate1.copy(computerCards = List(computerCard1), cardDeck = cardDeck3)
    //    println(s"\ngameStateUpdate after 2nd card dealt: player cards: ${gameStateUpdate2.playerCards}, comp cards: ${gameStateUpdate2.computerCards}")
    //    println(s"gameStateUpdate after 2nd card dealt: used cards: ${gameStateUpdate2.cardDeck.usedCards}, remaining cards: ${gameStateUpdate2.cardDeck.remainingCards}")

    val playerCard2 = CardUtils.getRandomCard(cardDeck3.remainingCards, Random)
    val cardDeck4 = CardUtils.updateCardDeck(playerCard2, cardDeck3)
    val gameStateUpdate3 = gameStateUpdate2.copy(playerCards = List(playerCard1, playerCard2), cardDeck = cardDeck4)
    //    println(s"\ngameStateUpdate after 3rd card dealt: player cards: ${gameStateUpdate3.playerCards}, comp cards: ${gameStateUpdate3.computerCards}")
    //    println(s"gameStateUpdate after 3rd card dealt: used cards: ${gameStateUpdate3.cardDeck.usedCards}, remaining cards: ${gameStateUpdate3.cardDeck.remainingCards}")

    val computerCard2 = CardUtils.getRandomCard(cardDeck4.remainingCards, Random)
    val cardDeck5 = CardUtils.updateCardDeck(computerCard2, cardDeck4)
    val gameStateUpdate4 = gameStateUpdate3.copy(computerCards = List(computerCard1, computerCard2), cardDeck = cardDeck5)
    //    println(s"\ngameStateUpdate after 4th card dealt: player cards: ${gameStateUpdate4.playerCards}, comp cards: ${gameStateUpdate4.computerCards}")
    //    println(s"gameStateUpdate after 4th card dealt: used cards: ${gameStateUpdate4.cardDeck.usedCards}, remaining cards: ${gameStateUpdate4.cardDeck.remainingCards}")


    //show player cards
    println(s"""Your cards are:
            ------------------------------------
            ${formatCardForDisplay(gameStateUpdate4.playerCards(0))}
            ${formatCardForDisplay(gameStateUpdate4.playerCards(1))}
            ------------------------------------""")

    //show computer cards
    println(s"""Computer cards are:
            ------------------------------------
            ${formatCardForDisplay(gameStateUpdate4.computerCards(0))}
            ${formatCardForDisplay(gameStateUpdate4.computerCards(1))}
            ------------------------------------""")

    gameStateUpdate4
  }

  def dealFlop(gameState: GameState):GameState = {
    val remainingCards = gameState.cardDeck.remainingCards

    //get 3 Cards For FLop
    val flop1 = CardUtils.getRandomCard(remainingCards, Random)
    val cardDeckAfterFlop1 = CardUtils.updateCardDeck(flop1, gameState.cardDeck)

    val flop2 = CardUtils.getRandomCard(cardDeckAfterFlop1.remainingCards, Random)
    val cardDeckAfterFlop2 = CardUtils.updateCardDeck(flop2, cardDeckAfterFlop1)

    val flop3 = CardUtils.getRandomCard(cardDeckAfterFlop2.remainingCards, Random)
    val cardDeckAfterFlop3 = CardUtils.updateCardDeck(flop3, cardDeckAfterFlop2)

    val gameStateAfterFlop = gameState.copy(communityCards = List(flop1, flop2, flop3), cardDeck = cardDeckAfterFlop3)

    //display flop
    println(s"""Cards on the flop are:
            ------------------------------------
            ${formatCardForDisplay(gameStateAfterFlop.communityCards(0))}
            ${formatCardForDisplay(gameStateAfterFlop.communityCards(1))}
            ${formatCardForDisplay(gameStateAfterFlop.communityCards(2))}
            ------------------------------------""")

    gameStateAfterFlop
  }

  def dealTurn(gameState: GameState):GameState = {
    val remainingCards = gameState.cardDeck.remainingCards

    //get 1 Cards For the Turn
    val turn = CardUtils.getRandomCard(remainingCards, Random)
    val cardDeckAfterTurn = CardUtils.updateCardDeck(turn, gameState.cardDeck)

    val gameStateAfterTurn = gameState.copy(communityCards = gameState.communityCards ::: turn :: Nil, cardDeck = cardDeckAfterTurn)

    //display flop
    println(s"""Cards after the turn are:
            ------------------------------------
            ${formatCardForDisplay(gameStateAfterTurn.communityCards(0))}
            ${formatCardForDisplay(gameStateAfterTurn.communityCards(1))}
            ${formatCardForDisplay(gameStateAfterTurn.communityCards(2))}
            ${formatCardForDisplay(gameStateAfterTurn.communityCards(3))}
            ------------------------------------""")

    gameStateAfterTurn
  }

  def dealRiver(gameState: GameState):GameState = {
    val remainingCards = gameState.cardDeck.remainingCards

    //get 1 Cards For the River
    val river = CardUtils.getRandomCard(remainingCards, Random)
    val cardDeckAfterRiver = CardUtils.updateCardDeck(river, gameState.cardDeck)

    val gameStateAfterRiver = gameState.copy(communityCards = gameState.communityCards ::: river :: Nil, cardDeck = cardDeckAfterRiver)

    //display flop
    println(s"""Cards after the turn are:
            ------------------------------------
            ${formatCardForDisplay(gameStateAfterRiver.communityCards(0))}
            ${formatCardForDisplay(gameStateAfterRiver.communityCards(1))}
            ${formatCardForDisplay(gameStateAfterRiver.communityCards(2))}
            ${formatCardForDisplay(gameStateAfterRiver.communityCards(3))}
            ${formatCardForDisplay(gameStateAfterRiver.communityCards(4))}
            ------------------------------------""")

    gameStateAfterRiver
  }

  def calculateWinner(finalGameState: GameState):GameState = {
    //get players best 5 cards from 7
    val playerBestHand = CardUtils.getBestHand(List(finalGameState.playerCards, finalGameState.communityCards).flatten)
    println(playerBestHand)

    //get players best 5 cards from 7
    val computerBestHand = CardUtils.getBestHand(List(finalGameState.computerCards, finalGameState.communityCards).flatten)
    println(computerBestHand)

    //compare player best 5 v computer best 5
    print(
      s"""final hands were,
         |player:   $playerBestHand
         |computer: $computerBestHand""".stripMargin)


    finalGameState
  }
}
