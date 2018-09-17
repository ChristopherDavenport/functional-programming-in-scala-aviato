package fpis.chapter07

import java.util.concurrent.{Callable, Future, ExecutorService, TimeUnit}
import cats.Applicative

object Parallelism {
  sealed trait Par[A]{
    def map[B](f: A => B): Par[B] =
      Par.map2(this, Par.unit(())){ case (a, _) => f(a)}
    def map2[B, C](that: Par[B])(f: (A, B) => C): Par[C] = 
      Par.map2(this, that)(f)
    
  }
  object Par {
    private final case class Pure[A](a: A) extends Par[A]
    private final case class Delay[A](a: () => A) extends Par[A]
    private final case class Fork[A](a: () => Par[A]) extends Par[A]
    private final case class Map2[A, B, C](a: Par[A], b: Par[B], f: (A, B) => C) extends Par[C]


    def unit[A](a: A): Par[A] = Pure(a)
    def delay[A](thunk: => A): Par[A] = Delay(() => thunk)
    
    def fork[A](thunk: => Par[A]): Par[A] = Fork(() => thunk)
    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = Map2(p1, p2, f)

    // Non Primitive
    def asyncF[A, B](f: A => B): A => Par[B] = {a => 
      Par.fork(Par.delay(f(a)))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      import cats.implicits._
      ps.traverse(Par.asyncF(f))
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
      Par.parMap(as)(a => (a, f(a)))
        .map(_.filter(_._2))
        .map(_.map(_._1))


    implicit val parApplicative: Applicative[Par] = new Applicative[Par]{
      def pure[A](x: A): Par[A] = Par.unit(x)
      def ap[A, B](ff: Par[A => B])(fa: Par[A]): Par[B] = 
        Par.map2(ff, fa)((f, a) => f(a))
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      import cats.implicits._
      ps.sequence
    }

    def unsafeRunSync[A](a: Par[A])(es: ExecutorService): A = unsafeRunFuture(a)(es).get
    def unsafeRunFuture[A](a: Par[A])(es: ExecutorService): Future[A] = a match {
      case Pure(a) => new Future[A]{
        def cancel(mayInterruptIfRunning: Boolean): Boolean = false
        def get(): A = a
        def get(timeout: Long, unit: TimeUnit): A = a
        def isCancelled(): Boolean = false
        def isDone(): Boolean = true
      }
      case Delay(a) => new Future[A]{
        def cancel(mayInterruptIfRunning: Boolean): Boolean = false
        def get(): A = a()
        def get(timeout: Long, unit: TimeUnit): A = a()
        def isCancelled(): Boolean = false
        def isDone(): Boolean = true
      }
      case Fork(fork) => new Future[A]{
        def cancel(mayInterruptIfRunning: Boolean): Boolean = false
        def get(): A = es.submit(
          new Callable[A]{
            def call = unsafeRunFuture(fork())(es).get
          }
        ).get
        def get(timeout: Long, unit: TimeUnit): A = 
          es.submit(
          new Callable[A]{
            def call = unsafeRunFuture(fork())(es).get(timeout, unit)
          }
        ).get(timeout, unit)
        def isCancelled(): Boolean = false
        def isDone(): Boolean = true
      }
      case Map2(p1, p2, f) => new Future[A]{
        def cancel(mayInterruptIfRunning: Boolean): Boolean = false
        def get(): A = {
          val f1 = unsafeRunFuture(p1)(es)
          val f2 = unsafeRunFuture(p2)(es)
          val a = f1.get
          val b = f2.get
          f(a, b)
        }
        def get(timeout: Long, unit: TimeUnit): A = {
          val f1 = unsafeRunFuture(p1)(es)
          val f2 = unsafeRunFuture(p2)(es)
          val a = f1.get(timeout,unit)
          val b = f2.get(timeout, unit)
          f(a, b)
        }
        def isCancelled(): Boolean = false
        def isDone(): Boolean = true
      }
    }
  }
}