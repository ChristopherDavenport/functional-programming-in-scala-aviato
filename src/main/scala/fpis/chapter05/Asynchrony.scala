package fpis.chapter05

object Asynchrony {
  // Single Threading
  // Thread 1 : Task 1 -> Task 2 -> Task 3 -> Task 4

  // Multi Threading
  // Thread 1: Task 1
  // Thread 2: Task 2
  // Thread 3: Task 3
  // Thread 4: Task 4

  // Asynchronous Single Threading
  // Start executing a task, hold it in the middle of a task, and 
  // start executing other tasks, and interleave them.
  // Thread 1: 
  // Task1 -> Task 2 -> Task 1 -> Task 3 -> Task 4 ->
  // Task 2 -> Task 1 -> Task 3 -> Task 1

  // Asynchronous Multi Threaded
  // When you start and resume, they can start and then
  // be resumed on another thread

  // Scala Solution :(
  import scala.concurrent.{Future, Promise, ExecutionContext, Await}
  import scala.concurrent.duration._
  import java.lang.Runnable

  def basicPromise(implicit ec: ExecutionContext) = {
    val promise : Promise[Int] = Promise[Int]
    promise.complete(scala.util.Try(3))
    val p = promise.future
    val f = p.map(i => println(i))
    Await.result(f, 1.second)
  }

  // What can we define a callback as

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      Async.FlatMap(this, f)
    
  }

  object Async {
    def delay[A](a: => A): Async[A] = 
      Delay(() => a)
    def pure[A](a: A ): Async[A] =
      Pure(a)

    def async[A](f: (Either[Throwable, A] => Unit) => Unit): Async[A] = 
      AsyncA(f)

    final case class FlatMap[A, B](a: Async[A], f: A => Async[B]) extends Async[B]
    final case class Pure[A](a: A) extends Async[A]
    final case class Delay[A](a: () => A) extends Async[A]
    final case class AsyncA[A](
      f: (Either[Throwable, A] => Unit) => Unit
    ) extends Async[A]

    // def unsafeRunAsync[A](a: Async[A], cb: Either[Throwable, A] => Unit) : Unit = {
    //   def runWith[M, N](
    //     f: M => Async[N], 
    //     cb: Either[Throwable, M] => Unit,
    //     e: Either[Throwable, N]
    //   ): Unit =  {
    //     ???
    //   }
    //   a match {
    //     // flatmap[A, B]
    //     case FlatMap[B, A](as, f) => 
    //       unsafeRunAsync(as, runWith[A,B](f, cb, _))

    //     case Pure(a) => cb(Right(a)) 
    //     case Delay(a) => try {
    //       cb(Right(a()))
    //     } catch {
    //       case t: Throwable => 
    //         cb(Left(t))
    //     }
    //     case AsyncA(f) => f(cb)
    //   }
    // }

    
    // def unsafeRunAsync[A](a: Async[A])(implicit ec: ExecutionContext): Unit = 
    //   a match {
    //     case Async.AsyncA(f) =>  ec.execute(
    //       Runnable.run()
    //     )
    //   }
  }


}