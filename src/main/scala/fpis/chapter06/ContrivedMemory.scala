package fpis.chapter06

import cats.Applicative
import cats.implicits._

object ContrivedMemory {
  trait Foo[F[_]]{
    def foo: F[Int]
  }

  object Foo {
    def apply[F[_]](implicit ev: Foo[F]) = ev

    def impl[F[_]: Applicative]: Foo[F] = new Foo[F]{
      def foo: F[Int] = 1.pure[F]
    }
  }

}