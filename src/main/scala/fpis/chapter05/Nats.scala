package fpis.chapter05

object Nats {

  sealed trait Nat
  object Nat {
    def toInt(nat: Nat): Int = nat match {
      case One => 1
      case Succ(x) => 1 + toInt(x)
    }
    def fromInt(i: Int): Option[Nat] = {
      // precondition int supplied >= 1
      @scala.annotation.tailrec
      def accumulate(int: Int, nat: Nat): Nat = int match {
        case int if int == 1 => nat
        case _ => accumulate(int - 1, Succ(nat))
      }
      i match {
        case i if i < 1 => None
        case _ => Some(accumulate(i, One))
      }
    }

    // A -> A -> A
    def addition(first: Nat, second: Nat): Nat = first match {
      case One => Succ(second)
      case Succ(x) => addition(x, Succ(second))
    }
    def multiplication(first: Nat, second: Nat): Nat = first match {
      case One => second
      case Succ(x) => addition(second, multiplication(x, second))
    }

    def subtraction(first: Nat, second: Nat): Option[Nat] = (first, second) match {
      case (One, _) => None
      case (Succ(x), One) => Some(x)
      case (Succ(x), Succ(y)) => subtraction(x, y)
    }

    // additition(second, addition(second, addition(second, second)))
    def decr(nat: Nat): Option[Nat] = nat match {
      case One => None
      case Succ(x) => Some(x)
    }
  }

  final case object One extends Nat
  final case class Succ(x: Nat) extends Nat

}