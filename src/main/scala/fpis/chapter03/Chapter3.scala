package fpis.chapter03

import scala.annotation.tailrec

object Chapter3 {

  /**
    * Exercises 3.1 - 3.6 atm
    */
  sealed trait MList[+A]
  final case class MCons[+A](head: A, tail: MList[A]) extends MList[A]
  final case object MNil extends MList[Nothing]
  object MList{
    def empty[A]: MList[A] = MNil
    def cons[A](x: A, xs: MList[A]): MList[A] = new MCons(x, xs)

    def apply[A](as: A*): MList[A] = 
      as.headOption.fold(MList.empty[A])(h => new MCons(h, apply(as.tail: _*)))

      /** 
    * Exercise 3.1
    * Answer 3 
    */
  val threePoint1 = MList(1,2,3,4,5) match {  
    // No 1,2,3
    case MCons(x, MCons(2, MCons(4, _))) => x
    // No Definitely Not Empty List
    case MNil => 42
    // x: 1, y: 2, 3, 4, ... => this path =>
    // 1 + 2 => 3
    case MCons(x, MCons(y, MCons(3, MCons(4, _)))) => x + y
    // Never Reaches
    case MCons(h, t) => h + sum(t)
    // Never Reaches
    case _ => 101
  }

  def sum(ints: MList[Int]): Int = ints match {
    case MNil => 0
    case MCons(x, xs) => x + sum(xs)
  }

    /**
      * Exercise 3.2
      * Either Option or Empty are reasonable on empty list
      **/
    def tail[A](l: MList[A]): MList[A] = l match {
      case MNil => MNil
      case MCons(_, xs) => xs
    }

    /**
      * Exercise 3.3
      **/
    def setHead[A](l: MList[A], a: A): MList[A] = l match {
      case MNil => MNil
      case MCons(_, xs) => MList.cons(a, xs)
    }

    /** 
      * Exercise 3.4
      **/
    @tailrec
    def drop[A](l: MList[A], n: Int): MList[A] = (l, n) match {
      case (x, 0) => x
      case (MNil, _) => MNil
      case (MCons(_, xs), int) => drop(xs, int-1)
    }

    /**
      * Exercise 3.5
      */
    @tailrec
    def dropWhile[A](l: MList[A], f: A => Boolean): MList[A] = l match {
      case MCons(x, xs) if f(x) => dropWhile(xs, f)
      case a => a
    }

    /**
      * Exercise 3.6
      **/
    def init[A](l: MList[A]): MList[A] =
      reverse(tail(reverse(l)))


    def foldRight[A, B](as: MList[A], z: B)(f: (A, B) => B): B =
      as match {
        case MNil => z
        case MCons(x, xs) => f(x, foldRight(xs, z)(f))
      }


    /**
      * Exercise 3.7 - No Foldright can't be implemented which can terminate early, as the function
      * Needs to be passed down the whole of the list.
      **/



    /**
      * Exercise 3.8 - foldRight is analagous to cons and nil
      **/
    def recreateList[A](as: MList[A]) = foldRight[A, MList[A]](as, MList.empty[A])(MList.cons)

    /**
      * Exercise 3.9 - Implement Length Via Foldright
      **/
    def length[A](as: MList[A]) = foldRight[A, Int](as, 0)((_, acc) => acc + 1)


    /**
      * Exercise 3.10
      **/
    @tailrec
    def foldLeft[A, B](as: MList[A], z: B)(f: (B, A) => B): B = 
      as match {
        case MNil => z
        case MCons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    

    /**
      * Helpers
      **/
    def reverse[A](l: MList[A]): MList[A] = {
      @tailrec
      def recurse(l: MList[A], acc: MList[A]): MList[A] = l match {
        case MNil => acc
        case MCons(x, xs) => recurse(xs, MList.cons(x, acc))
      }
      recurse(l, MList.empty[A])
    }

  }



}