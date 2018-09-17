package fpis.chapter06

import cats._
import cats.implicits._

object AppError {

  implicit def catsInvariantApplicativeError[F[_]]: Invariant[ApplicativeError[F, ?]] = new Invariant[ApplicativeError[F, ?]]{
    def imap[A, B](fa: ApplicativeError[F, A])(f: A => B)(g: B => A): ApplicativeError[F, B] = new ApplicativeError[F, B] {
      private val outerfa = fa
      private val outerF = f
      override def raiseError[C](e: B): F[C] = 
        outerfa.raiseError(g(e))

      def pure[A](x: A): F[A] = outerfa.pure(x)
  
      def handleErrorWith[C](fa: F[C])(f: B => F[C]): F[C] = 
        outerfa.handleErrorWith(fa)(outerF andThen f)
  

      def ap[C, D](ff: F[C => D])(fa: F[C]): F[D] = outerfa.ap(ff)(fa)
    }
  }

}