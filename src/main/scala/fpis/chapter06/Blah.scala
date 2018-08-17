package fpis.chapter05

import cats._
import cats.implicits._
import cats.effect.IO

object Blah {

  // flatMap(fa: F[A])(f: A => F[B]): F[B]

  def exists[A](a: A): A = a

  def myFlatTap[F[_]: FlatMap, A, B](fa: F[A])(f: A => F[B]): F[A] = for {
    a <- fa
    _ <- f(a)
  } yield a

  myFlatTap(IO(1))(i => IO(println(i)))
  myFlatTap(Option(1))(_ => Option(()))

}