package pokerdemo.model

import pokerdemo.model.Suit.Suit

/**
 * @author David Mackessy
 * @date 12/05/2020
 **/
case class Card(suit: Suit, value: CardValue.Value) extends Ordered[Card] {
  override def compare(that: Card): Int = {
    if (this.value.id > that.value.id) 1 else -1
  }
}

object Card {
  def apply(suit: Suit, value: Int): Card = {
    value match {
      case 2 => new Card(suit, CardValue.Two)
      case 3 => new Card(suit, CardValue.Three)
      case 4 => new Card(suit, CardValue.Four)
      case 5 => new Card(suit, CardValue.Five)
      case 6 => new Card(suit, CardValue.Six)
      case 7 => new Card(suit, CardValue.Seven)
      case 8 => new Card(suit, CardValue.Eight)
      case 9 => new Card(suit, CardValue.Nine)
      case 10 => new Card(suit, CardValue.Ten)
      case 11 => new Card(suit, CardValue.J)
      case 12 => new Card(suit, CardValue.Q)
      case 13 => new Card(suit, CardValue.K)
      case 14 => new Card(suit, CardValue.A)
    }

  }
}
