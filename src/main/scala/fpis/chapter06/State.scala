package fpis.chapter06

object State {
  case class State[S, A](fInit: S => (S, A)){
      def flatMap[B](fFlatmap: A => State[S, B]) : State[S, B] = 
        State{s1: S => fInit(s1) match {
            case (s2, a) => 
              val stateB : State[S, B] = fFlatmap(a)
              val sTuple : (S, B) = stateB.fInit(s2)
              sTuple
          }
        }
      def map[B](f: A => B): State[S, B] = 
        State{s1: S => fInit(s1) match {
          case (s2, a) => (s2, f(a))
        }}

    }

    trait MyApplicative[F[_]]{
      def map[A, B](xa: F[A])(f: A => B): F[B]
      // def pure[A](a: A): F[A]
      def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

      def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
        ap(map(fa)(a => (b: B) => (a, b)))(fb)


      // def product2[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      //   ap(liftF[A, B, (A, B)](fa){(a, b) => (a, b)})(fb)
      // private def liftF[A, B, C](fa: F[A])(f: (A, B) => C): F[B => C] =
      //   map(fa)(a => {(b: B) => f(a, b)})

      def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A,B) => Z): F[Z] =
        map(product(fa, fb))(f.tupled)

      }

    object State {
      import cats.Monad
      import cats.implicits._
      def continually[F[_] : Monad, A](fa: F[A]): F[A] = 
        fa >> continually(fa)
      // def relplicateM[S, A](s: State[S, A])(n: Int): State[S, A] = for {
      //   a <- s
      //   _ <- replicateM(State{s => (s, a)})(n - 1)
      // } yield 
      def modify[S, A](state: State[S, A])(f: S => S): State[S, A] = 
        State{s1: S => 
          val (s, a) = state.fInit(s1)
          (f(s), a)
        }
      def modify0[S](f: S => S): State[S, Unit] = 
        State[S, Unit]{s =>
          (f(s), ())
        }
      def value[S]: State[S, S] = 
      State{s => (s, s)}

      def runState[S, A](state: State[S, A])(s: S): (S, A) = 
        state.fInit(s)
    }

    def increaseValueBy1[A](s: State[Int, A]): State[Int, A] =
      State.modify(s)(_ + 1)

    def readString(s: String): State[Int, Unit] = 
      State{s1: Int => 
        (s.toInt, ())
      }
    def increaseByString(s: String): State[Int, Unit] =
      State{s1: Int => 
        (s1 + s.toInt, ())
      }

    val simpleProgram: String => Int => Int = s => i => {
      val read = for {
        unit <- increaseByString(s)
        string <- State.value[Int].map(_.toString)
        _ <- increaseByString(string)
      } yield ()
      read.fInit(i)._1
    }
    

    def unit[A](a: A): Unit = ()

    


}