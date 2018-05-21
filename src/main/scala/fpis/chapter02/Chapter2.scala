package fpis.chapter02

import scala.annotation.tailrec

object Chapter2 {

  /**
    * Exercise 2.1 - Recursive Fibonacci to get the Nth number
    * Takes the number in the sequence to get, if you ask for a
    * negative or zero you receive none, otherwise you receive
    * the fibonacci barring integer overflow
    */
  def fib(n: Int): Option[BigInt] = {
    @tailrec
    def getFib(first: BigInt, second: BigInt, countdown: Int): BigInt = countdown match {
      case 1 => first
      case 2 => second
      case _ => getFib(second, first + second, countdown - 1)
    }
    if (n > 0) Some(getFib(BigInt(0),BigInt(1),n)) else None
  }

  /**
    * Exercise 2.2
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def check(n: Int, array: Array[A], ordered: (A, A) => Boolean): Boolean = n match {
      case i if i >= (array.length - 1) => true
      case i if !ordered(array(i), array(i+1)) => false
      case i => check(i + 1, array, ordered)
    }
    check(0, as, ordered)
  }

  /**
    * Exercise 2.3
    *
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    {a: A => {b : B => 
      f(a, b)
    }}

  /**
    * Exercies 2.4
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) => f(a)(b) }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {a : A => f(g(a))}
  

}