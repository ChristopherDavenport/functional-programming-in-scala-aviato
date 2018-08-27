package fpis.chapter06

import cats._
import cats.data._
import cats.implicits._
import cats.effect._

object Check {


  def check(int: Int): ValidatedNel[Throwable, Int] = {
    if (int < 1000 == 0) int.validNel
    else new Throwable("Foo!").invalidNel
  }

  val xa : List[Int] = List.range(1, 1000)

  val checked : ValidatedNel[Throwable, List[Int]] = xa.traverse(check)
  //  Either[NonEmptyList[Throwable], List[Int]]


  def traverse[G[_]: Applicative, A, B](xa: List[A])(f: A => G[B]): G[List[B]] = xa match {
    case Nil => List.empty[B].pure[G]
    case x :: xs => (f(x), traverse(xs)(f)).mapN(_ :: _)
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)

  // * -> *
  type StateM[A] = State[Machine, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = 
    inputs.traverse_(simulateInput) *> getValues

  def weirdReadLine(i: List[String]): IO[String] =
    i.traverse_(putLine) *> readLine

  def putLine(s: String): IO[Unit] = IO(println(s))

  def readLine: IO[String] = IO(scala.io.StdIn.readLine)
    
  def simulateInput(input: Input): State[Machine, Unit] = input match {
    case Coin =>
      cats.data.State.modify{ machine : Machine => 
        Machine(locked = false, machine.candies, machine.coins + 1)
      }
    case Turn =>
      cats.data.State.modify{ machine: Machine => 
        if (machine.candies <= 0) machine
        else if (!machine.locked) Machine(locked= true, machine.candies -1 , machine.coins)
        else machine
      }
  }

  def getValues : State[Machine, (Int, Int)] = cats.data.State.inspect{m: Machine => (m.candies, m.coins)}

  def inspect[S, A, B](s: cats.data.State[S, A])(f: S => B): cats.data.State[S, B] = 
    s *> cats.data.State{s => 
      (s, f(s))
    }

    // State: S => (S, A)



  // def traverse2[G[_]: Applicative, F[_]: Alternative: Foldable, A, B](xa: F[A])(f: A => G[B]): G[F[B]] =
  //   Foldable[F].foldRight(xa, Eval.now(MonoidK[F].empty.pure[G])){ case (a, finalThing) => 
  //     finalThing.map(glistB => 
  //       (f(a), glistB).mapN{ (b, fb) => MonoidK[F].combineK(Alternative[F].pure(b), fb)}
  //     )
  //   }.value

  def sequence[G[_]: Applicative, A](xa: List[G[A]]): G[List[A]] = traverse(xa)(identity)



}