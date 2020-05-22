package pokerdemo

import pokerdemo.model.CardDeck
import pokerdemo.utils.CardUtils

import scala.util.Random
import scala.io.StdIn.readLine

/**
 * @author David Mackessy
 * @date 12/05/2020
 **/
object PokerApp extends App {

  println(
    """
      |##############################
      |     Welcome to Poker App
      |##############################
      |""".stripMargin)

  startGameLoop()

  def startGameLoop(): Unit = {
    println("dealer will now shuffle & deal...\n")

    //create card deck
    val cardDeck = CardDeck(CardUtils.createCardDeck().flatten, List(), CardUtils.createCardDeck().flatten)

    //create game state
    val gameState = GameState(List(), List(), List(), cardDeck)

    //deal cards
    val gameStateAfterDeal = gameState.dealCardsToPlayers(gameState)

    println("\nhow much would you like to bet?")
    val bet1 = readLine("bet amount?")

    //deal the flop
    val gameStateAfterFlop = gameState.dealFlop(gameStateAfterDeal)

    println("\nhow much would you like to bet?")
    val bet2 = readLine("bet amount?")

    //deal the turn
    val gameStateAfterTurn = gameState.dealTurn(gameStateAfterFlop)

    println("\nhow much would you like to bet?")
    val bet3 = readLine("bet amount?")

    //deal the river
    val gameStateAfterRiver = gameState.dealRiver(gameStateAfterTurn)

    println("\nhow much would you like to bet?")
    val bet4 = readLine("bet amount?")

    //check who won
    val finalGameState = gameState.calculateWinner(gameStateAfterRiver)

    println("\ndo you want to play again?\n")
    val choice = readLine("y or n?")

    //play again?
    choice match {
      case "y" => startGameLoop()
      case _   => println("Until next time, goodbye")
    }
  }

}
