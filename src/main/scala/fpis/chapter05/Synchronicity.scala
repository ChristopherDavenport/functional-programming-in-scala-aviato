package fpis.chapter05

object Synchronicity {
  sealed trait Sync[A]{

    def flatMap[B](f: A => Sync[B]): Sync[B] = 
      Sync.Flatmap(this, f)
    def map[B](f: A => B): Sync[B] =
      Sync.Flatmap(this, f.andThen(Sync.Pure[B]))
    def recover(f: Throwable => A) : Sync[A] =
      Sync.Recover(this, f)
    def recoverWith(f: Throwable => Sync[A]): Sync[A] =
      Sync.RecoverWith(this, f)
    
  }
  object Sync {
    def delay[A](a: => A): Sync[A] = Delay(() => a)
    def pure[A](a: A): Sync[A] = Pure(a)
    def raiseError[A](e: Throwable): Sync[A] = Error(e)

    private final case class Delay[A](a: () => A) extends Sync[A]
    private final case class Error[A](e: Throwable) extends Sync[A]
    private final case class Pure[A](a: A) extends Sync[A]
    private final case class Flatmap[A, B](s: Sync[A], f: A => Sync[B]) extends Sync[B]
    private final case class Recover[A](s: Sync[A], f: Throwable => A) extends Sync[A]
    private final case class RecoverWith[A](s: Sync[A], f: Throwable => Sync[A]) extends Sync[A]

    def unsafeRunSync[A](s: Sync[A]): A = 
      unsafeRunAttempt(s).fold(e => throw e, identity)

    def unsafeRunAttempt[A](s: Sync[A]): Either[Throwable, A] =  s match {
    case Sync.Delay(a) => try {
      Right(a())
    } catch {
      case t: Throwable => Left(t)
    }
    case Sync.Error(e) => Left(e)
    case Sync.Pure(a) => Right(a)
    case Sync.Recover(s, f) => 
      unsafeRunAttempt(s).fold(t => Right(f(t)), a=> Right(a))
    case Sync.RecoverWith(s, f) => 
      unsafeRunAttempt(s).fold(t => unsafeRunAttempt(f(t)), a => Right(a))
    case Sync.Flatmap(s, f) => 
      try {
        unsafeRunAttempt(s).flatMap(a => unsafeRunAttempt(f(a)))
      } catch {
        case t: Throwable => Left(t)
      }
    }
  }

  

  

}