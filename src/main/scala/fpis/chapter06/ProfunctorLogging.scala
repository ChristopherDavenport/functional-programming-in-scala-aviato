package fpis.chapter06

trait Profunctor[P[_, _]]{
  def dimap[A, B, C, D](p: P[B, C])(f: A => B)(f2: C => D) : P[A, D]
  def lmap[A,B, C](p: P[B, C])(f: A => B): P[A, C]
  def rmap[A, B, C](p: P[A, B])(f: B => C): P[A, C]
  // def lmap
//   dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
// Map over both arguments at the same time.
// dimap f g ≡ lmap f . rmap g
// lmap :: (a -> b) -> p b c -> p a c
// Mp the first argument contravariantly.
// lmap f ≡ dimap f id
// rmap :: (b -> c) -> p a b -> p a c
// Mp the second argument covariantly.
// rmap ≡ dimap id
}

trait Logger[A, B]{
  def error(a: A): B
  def info(a: A): B
  def debug(a: A): B
}

object Logger {
  def profunctorLogger : Profunctor[Logger] = new Profunctor[Logger]{
    def dimap[A, B, C, D](p: Logger[B, C])(f: A => B)(f2: C => D) : Logger[A, D] = new Logger[A, D]{
      def error(a: A): D = f2(p.error(f(a)))
      def info(a: A): D = f2(p.info(f(a)))
      def debug(a: A): D = f2(p.debug(f(a)))
    }
    def lmap[A,B, C](p: Logger[B, C])(f: A => B): Logger[A, C] = new Logger[A, C]{
      def error(a: A): C = p.error(f(a))
      def info(a: A): C = p.info(f(a))
      def debug(a: A): C = p.debug(f(a))
    }
    def rmap[A, B, C](p: Logger[A, B])(f: B => C): Logger[A, C] = new Logger[A, C]{
      def error(a: A): C = f(p.error(a))
      def info(a: A): C = f(p.info(a))
      def debug(a: A): C = f(p.debug(a))
    }
  }

  // given A => String

  def showLogger[F[_]: cats.effect.Sync, A: cats.Show]: Logger[A, F[Unit]] = profunctorLogger.lmap(baseLogger)(cats.Show[A].show(_))

  def baseLogger[F[_]: cats.effect.Sync]: Logger[String, F[Unit]] = new Logger[String, F[Unit]]{

    def error(a: String): F[Unit] = cats.effect.Sync[F].unit
    def info(a: String): F[Unit] = cats.effect.Sync[F].unit
    def debug(a: String): F[Unit] = cats.effect.Sync[F].unit

  }
}