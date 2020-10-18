import sun.security.krb5.internal.crypto.Nonce.value
// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.

// Attributions and useful links:
// https://nrinaudo.github.io/scala-best-practices/definitions/adt.html
// https://alvinalexander.com/scala/fp-book/algebraic-data-types-adts-in-scala/
// https://en.wikipedia.org/wiki/Algebraic_data_type

import scala.util.control.Exception._

case class Rank(val rank: Char) {
   def apply( rank: Char): Rank = {
    val ranks: Set[Char] = Set( '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
    if (ranks.contains(rank))
      Rank(rank)
    else
      throw new Exception(s"$rank is not valid rank.")
  }
}

case class Suit(val suit: Char) {
   def apply( suit: Char): Suit = {
    val suits: Set[Char] = Set('c', 'd', 'h', 's')
    if (suits.contains(suit))
      Suit(suit)
    else
      throw new Exception(s"$suit is not valid suit.")
  }
}

final case class Card (val rank: Rank, val suit: Suit)

//sealed abstract class Hand
//object Hand {
  //case class Omaha (cards: Set[Card]) extends Hand
  //case class Texas (cards: Set[Card]) extends Hand
//}

final case class Hand private (cards: Set[Card], isOmaha: Boolean = false)
object Hand {
  def create(cards: Set[Card], isOmaha: Boolean = false): Option[Hand] = cards.size match {
    case 2 if !isOmaha => Some(Hand(cards))
    case 4 if isOmaha  => Some(Hand(cards))
    case _             => None
  }
}

class Board(cards: Set[Card])

sealed trait PokerCombination
object PokerCombination {
  case class Highcard (cards: Set[Card]) extends PokerCombination
  case class Pair (cards: Set[Card]) extends PokerCombination
  case class TwoPairs (cards: Set[Card]) extends PokerCombination
  case class ThreeOfAKind (cards: Set[Card]) extends PokerCombination
  case class Straight (cards: Set[Card]) extends PokerCombination
  case class Flush (cards: Set[Card]) extends PokerCombination
  case class FullHouse (cards: Set[Card]) extends PokerCombination
  case class FourOfAKind (cards: Set[Card]) extends PokerCombination
  case class StraightFlush (cards: Set[Card]) extends PokerCombination
  case class RoyalFlush (cards: Set[Card]) extends PokerCombination
}

class TestCase(board: Board, hands: List[Hand])

class TestResult(combination: PokerCombination)
