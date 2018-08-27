package fpis.chapter06

import cats.implicits._
import cats.data.State

object IndexedCheck {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  sealed trait Machine
  final case class EmptyLockedMachine(coins: Int) extends Machine
  final case class LockedMachine(candies: Int, coins: Int) extends Machine
  final case class UnlockedMachine(candies: Int, coins: Int) extends Machine

  def input(i: Input, machine: Machine): Machine = (i, machine) match {
    // A machine that’s out of candy ignores all inputs.
    case (_, e: EmptyLockedMachine) => e
    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    case (Coin, LockedMachine(candies, coins)) =>  UnlockedMachine(candies, coins + 1)
    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    case (Turn, UnlockedMachine(candies, coins)) =>
      val newcandies = candies - 1
      if (newcandies == 0) EmptyLockedMachine(coins)
      else LockedMachine(newcandies, coins)
    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    case (_, m) => m
  }

  def getCandiesAndCoins(m: Machine): (Int, Int) = m match {
    case EmptyLockedMachine(c) => (0, c)
    case LockedMachine(cand, coin) => (cand, coin)
    case UnlockedMachine(cand, coin) => (cand, coin)
  }

  def simulateMachine(xa: List[Input]): State[Machine, (Int, Int)] = 
    simulateInputs(xa) *> State.inspect(getCandiesAndCoins)

  def simulateInputs(xa: List[Input]): State[Machine, Unit] =
    xa.traverse_(simulateInput)

  def simulateInput(i: Input): State[Machine, Unit] = 
    State.modify{m: Machine => input(i, m)}


}