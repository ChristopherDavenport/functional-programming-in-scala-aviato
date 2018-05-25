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
      reverseHelper(tail(reverseHelper(l)))


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
    def recreateList[A](as: MList[A]) = 
      foldRight[A, MList[A]](as, MList.empty[A])(MList.cons)

    /**
      * Exercise 3.9 - Implement Length Via Foldright
      **/
    def length[A](as: MList[A]) = 
      foldRight[A, Int](as, 0)((_, acc) => acc + 1)

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
      * Exercise 3.11 - Write sum, product, and a function to compute the length of a list using foldLeft.
      **/

    def sumFL(as: MList[Int]): Int = foldLeft(as, 0)(_ + _)
    def productFL(as: MList[Int]): Int = foldLeft(as, 1)(_ * _)
    def lengthFL[A](as: MList[A]): Int = foldLeft(as, 0)((b, _) => b + 1)

    /**
      * Exercise 3.12 - Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). 
      * See if you can write it using a fold.         
      */
    def reverse[A](as: MList[A]): MList[A] = 
      foldLeft(as, MList.empty[A])((l, h) => MList.cons(h, l))

    /**
      * Exercise 3.13 - Hard: Can you write foldLeft in terms of foldRight? 
      * How about the other way around? 
      * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively, 
      * which means it works even for large lists without overflowing the stack.         
      */
    def foldLeftViaFoldRight[A, B](as: MList[A], z: B)(f: (B, A) => B): B = 
      foldRight(reverseHelper(as), z)((a, b) => f(b, a))

    def foldRightViaFoldLeft[A, B](as: MList[A], z: B)(f: (A, B) => B): B = 
      foldLeft(reverse(as), z)((b, a) => f(a, b))

    /**
      * Exercise 3.14 - Implement append in terms of either foldLeft or foldRight.
      */ 
    def concat[A](as: MList[A], bs: MList[A]): MList[A] = 
      foldRightViaFoldLeft(as, bs)(MList.cons)

    def append[A](as: MList[A], a: A): MList[A] = 
      concat(as, MList(a))

    /**
      * Exercise 3.15 - Hard: Write a function that concatenates a list of lists into a single list. 
      * Its runtime should be linear in the total length of all lists. 
      * Try to use functions we have already defined.         
      */
    def flatten[A](as: MList[MList[A]]): MList[A] =
      foldRightViaFoldLeft(as, MList.empty[A])(concat)

    def deconstructedFlatten[A](as: MList[MList[A]]): MList[A] = {
      def myreverse[B](as: MList[B]): MList[B] = 
        myfoldLeft(as, MList.empty[B])((l, h) => MList.cons(h, l))
      def myfoldLeft[C, B](as: MList[C], z: B)(f: (B, C) => B): B = {
        as match {
          case MNil => z
          case MCons(x, xs) => myfoldLeft(xs, f(z, x))(f)
        }
      }
      def myfoldRightViaFoldLeft[C, B](as: MList[C], z: B)(f: (C, B) => B): B = {
        myfoldLeft(reverse(as), z)((b, a) => f(a, b))
      }
      def myconcat[C](as: MList[C], bs: MList[C]): MList[C] = 
        myfoldRightViaFoldLeft(as, bs)(MList.cons)
      myfoldLeft(myreverse(as), MList.empty[A])((b, a) => myconcat(a, b))
    }


    /**
      * Exercise 3.16 - Write a function that transforms a list of integers by adding 1 to each element. 
      * (Reminder: this should be a pure function that returns a new List!)         
      */
    def addOne(as: MList[Int]): MList[Int] = 
      foldRightViaFoldLeft(as, MList.empty[Int])((h, t) => MList.cons(h+1, t))
    
    /**
      * Exercise 3.17 - Write a function that turns each value in a List[Double] into a String. 
      * You can use the expression d.toString to convert some d: Double to a String.         
      */
    def convertDoubleToString(as: MList[Double]): MList[String] =
      foldRightViaFoldLeft(as, MList.empty[String])((h, t) => MList.cons(h.toString, t))


    /**
      * Exercise 3.18 - Write a function map that generalizes 
      * modifying each element in a list while maintaining the structure of the list.
      */
    def map[A,B](as: MList[A])(f: A => B): MList[B] = 
      foldRightViaFoldLeft(as, MList.empty[B])((h, t) => MList.cons(f(h), t))

    /**
      * Exercise 3.19 - Write a function filter 
      * that removes elements from a list unless they satisfy a given predicate. 
      * Use it to remove all odd numbers from a List[Int].
      */
    def filter[A](as: MList[A])(f: A => Boolean): MList[A] = 
      foldRightViaFoldLeft(as, MList.empty[A])((h, t) => if(f(h)) MList.cons(h, t) else t)

    def evenNumbers(as: MList[Int]): MList[Int] = filter(as)(_ % 2 == 0)

    /**
      * Exercise 3.20 - Write a function flatMap that works like map 
      * except that the function given will return a list instead of a single result, 
      * and that list should be inserted into the final resulting list. 
      */
    def flatMap[A, B](as: MList[A])(f: A => MList[B]): MList[B] = 
      foldRightViaFoldLeft(as, MList.empty[B])((a, b) => concat(f(a), b))

    /**
      * Exercise 3.21 - Use flatMap to implement filter
      */
    def filterViaFlatMap[A](as: MList[A])(f: A => Boolean): MList[A] = 
      flatMap(as)(a => if (f(a)) MList(a) else MList.empty[A])

    /**
      * Helpers
      **/
    def reverseHelper[A](l: MList[A]): MList[A] = {
      @tailrec
      def recurse(l: MList[A], acc: MList[A]): MList[A] = l match {
        case MNil => acc
        case MCons(x, xs) => recurse(xs, MList.cons(x, acc))
      }
      recurse(l, MList.empty[A])
    }

  }



}