package fpis.chapter04

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

  sealed trait MOption[+A]
  object MOption{
    def some[A](a: A): MOption[A] = MSome(a)
    def none[A]: MOption[A] = MNone

    def fold[A, B](ma: MOption[A])(ifNone: => B)(ifSome: A => B): B = ma match {
      case MSome(a) => ifSome(a)
      case MNone => ifNone
    }
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
  





  def mena2(xs: List[Double]): MOption[Double] = ???
  
  final case class ParseError(throwable: Throwable)
  def mean3(xs: List[Double]): Either[ParseError, Double] = ???

}