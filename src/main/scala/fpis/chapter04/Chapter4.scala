package fpis.chapter04

import scala.util.control.NonFatal

object Chapter4 {
  // Domain
  // Codomain
  // Argument
  def Agh: Int = 5
  // Operation
  // Totality
  // A => B
  // forall a in A return b which is a member of B

  // Referential Transparent
  // forall a in A return a single b which is a member of B 
  // b = f(a) and b' = f(a) , b === b'
  // def randomInt(): Int = scala.util.Random.nextInt
  
  // def divide(numerator: Int, denominator: Int): BigDecimal = 
  //   if (denominator == 0) ???
  //   else BigDecimal(numerator/denominator)

  sealed trait MOption[+A]{}
  object MOption{
    def some[A](a: A): MOption[A] = MSome(a)
    def none[A]: MOption[A] = MNone

    def catchNull[A](a: => A): MOption[A] = 
      if (a == null) MOption.none[A]
      else MOption.some(a)

    def catchThrowable[A](a: => A): MOption[A] = 
      try { MOption.some(a) } catch { case NonFatal(_) => MOption.none[A] }
    
    def safe[A](a: => A): MOption[A] = flatMap(catchThrowable(a))(a => catchNull[A](a))

    def lift[A,B](f: A => B): MOption[A] => MOption[B] = map(_)(f)

    def fold[A, B](ma: MOption[A])(ifNone: => B)(ifSome: A => B): B = ma match {
      case MSome(a) => ifSome(a)
      case MNone => ifNone
    }

    sealed trait CEither[A, B]
    final case class CLeft[A, B](a: A) extends CEither[A, B]
    final case class CRight[A, B](b: B) extends CEither[A, B]
    object CEither {
      def left[A, B](a: A): CEither[A, B] = CLeft(a)
      def right[A, B](b: B): CEither[A, B] = CRight(b)

      def show[A, B](e: CEither[A, B]): String = e match {
        case CLeft(a) => s"Left($a)"
        case CRight(b) => s"Right($b)"
      }
      trait Show[A]{
        def show(a: A): String
      }
      def cEitherShow[A, B]: Show[CEither[A, B]] = new Show[CEither[A, B]]{
        def show(a: CEither[A, B]): String = CEither.show[A, B](a)
      }


      def cRightShow[A,B]: Show[CRight[A, B]] = new Show[CRight[A, B]]{
        def show(a: CRight[A, B]): String = a match {
          case CRight(a) => s"Right($a)"
        }
      }

      sealed trait A1
      case class B1() extends A1

      sealed trait COption[+A]
      case class CSome[+A](a: A) extends COption[A]
      case object CNone extends COption[Nothing]

      val bsome : COption[A1] = CSome(B1())

      val a: A1 = B1()

      // f (B => C) => A => C iff (A => B)
      // f (A => C) => B => C iff (B => A)

      trait C1 extends A1


      // def cEitherShowFromCrightShow[A, B](showRight: Show[CRight[A, B]]): Show[CEither[A, B]] = new Show[]






      // def contravariantShow : ContravariantFunctor[Show] = new ContravariantFunctor[Show]{
      //   def map[A, B](fa: Show[A])(f: A => B): Show[B] = ???
      // }

      // def covariantShow : CovariantFunctor[Show] = ???

      // trait CovariantFunctor[F[_]]{
      //   def map[A, B](fa: F[A])(f: A => B): F[B] 
      // }
      // trait ContravariantFunctor[F[_]]{
      //   def contramap[A, B](fb: F[B])(f: A => B): F[A]
      // }
    }
    




    def map[A, B](ma: MOption[A])(f: A => B): MOption[B] = fold(ma)(MOption.none[B])(a => MOption.some(f(a)))
    def flatMap[A, B](ma: MOption[A])(f: A => MOption[B]): MOption[B] = fold(ma)(MOption.none[B])(f)
    def getOrElse[A](ma: MOption[A])(default: A): A = fold(ma)(default)(identity)
    def orElse[A](ma: MOption[A])(mb: => MOption[A]): MOption[A] = fold(ma)(mb)(MOption.some)
    def select[A](ma: MOption[A])(f: A => Boolean): MOption[A] = fold(ma)(MOption.none[A])(a => if (f(a)) MOption.some(a) else MOption.none)
    def filter[A](ma: MOption[A])(f: A => Boolean): MOption[A] = select(ma)(f)

    def ap[A,B](ma: MOption[A])(mf: MOption[A => B]): MOption[B] =
      flatMap(ma)(a => map(mf)(f => f(a)))

    def product[A, B](ma: MOption[A], mb: MOption[B]): MOption[(A, B)] = {
      val function: MOption[B => (A, B)] = map(ma)(a => (b: B) => (a, b))
      ap(mb)(function)
    }

    def map2[A,B, C](ma: MOption[A], mb: MOption[B])(f: (A, B) => C): MOption[C] = 
      map(product(ma, mb))(f.tupled)

    def sequence[A](xa: List[MOption[A]]): MOption[List[A]] = 
      traverse(xa)(identity)

    def traverse[A, B](xa: List[A])(f: A => MOption[B]): MOption[List[B]] = 
      xa.foldRight(MOption.some(List.empty[B])){case (next, acc) => 
        flatMap(acc)(l => map(f(next))(a => a::l))
      }

    def sequenceViaTraverse[A](xa: List[MOption[A]]): MOption[List[A]] = traverse(xa)(identity)

    private final case class MSome[A](value: A) extends MOption[A]
    private final case object MNone extends MOption[Nothing]
  }
  
  def divide(numerator: Int, denominator: Int): MOption[BigDecimal] = 
    if (denominator == 0) MOption.none[BigDecimal]
    else MOption.some(BigDecimal(numerator/denominator))

  final class NonZeroInt private (val n: Int) extends AnyVal
  object NonZeroInt {
    def validate(i: Int): MOption[NonZeroInt] = 
      if (i == 0) MOption.none[NonZeroInt]
      else MOption.some(new NonZeroInt(i))
  }

  def divide(numerator: Int, denominator: NonZeroInt): BigDecimal =
    BigDecimal(numerator / denominator.n)

  final case class NonEmptyList[A](x: A, xs: List[A])

  def mean(xs: NonEmptyList[Double]): Double = ???
  // l: List[List[Double]]
  // fa: List[A] => Option[NonEmptyList[A]]
  // traverse: G[A] => (A => F[B]) => F[G[B]]
  // l.traverse(fa): Option[List[NonEmptyList[Double]]]
  // fb: NonEmptyList[Double] => Double
  // monoid: Monoid[Double]
  // foldMap: Monoid[B] => F[A] => (A => B) => B
  // l.traverse(fa).map(_.foldMap(fb))
  // l.map(fa): List[Option[NonEmptyList[A]]]
  // List[List[Double]] => Double
  // l.map(fa).foldMap(
  //   _.fold(
  //     0.0
  //   )(
  //     fb
  //   )
  // )
  def mean2(xs: List[Double]): MOption[Double] = ???
  
  final case class ParseError(throwable: Throwable)
  def mean3(xs: List[Double]): Either[ParseError, Double] = ???

  sealed trait MEither[E, A]
  object MEither {
    def left[E, A](e: E): MEither[E, A] = MLeft[E,A](e)
    def right[E, A](a: A): MEither[E, A] = MRight[E, A](a)
    def catchException[A](a: => A): MEither[Exception, A] = 
      try {right(a)} catch { case e: Exception => left(e)}

    def fold[E, A, B](ma: MEither[E,A])(first: E => B)(second: A => B): B = ma match {
      case MLeft(e) => first(e)
      case MRight(a) => second(a)
    }
    def map[E,A, B](ma: MEither[E, A])(f: A => B): MEither[E, B] = fold(ma)(left[E,B])(a => right(f(a)))
    def flatMap[E, A, B](ma: MEither[E, A])(f: A => MEither[E, B]) = fold(ma)(left[E, B])(f)
    def orElse[E,A](ma: MEither[E, A])(mb: => MEither[E, A]): MEither[E, A] = fold(ma)(_ => mb)(right)
    def map2[E, A, B, C](ma: MEither[E, A])(mb: MEither[E, B])(f: (A, B) => C): MEither[E, C] = 
      fold(ma)(left[E,C])(a => fold(mb)(left[E, C])(b => right(f(a, b))))

    def traverse[E, A, B](ma: List[A])(f: A => MEither[E, B]): MEither[E, List[B]] = 
      ma.foldRight(MEither.right[E, List[B]](List.empty[B])){ case (next, acc) => flatMap(acc)(l => map(f(next))(b => b :: l))}

    def sequence[E, A](ma: List[MEither[E, A]]): MEither[E, List[A]] = traverse(ma)(identity)

    // Exercise 4.8
    // Would need to not have a flatMap such that errors on the left could be combined. Such that E semigroup
    // combines values of e on each iteration of map2

    private final case class MRight[E, A](a: A) extends MEither[E, A]
    private final case class MLeft[E, A](e: E) extends MEither[E, A]
  }


}