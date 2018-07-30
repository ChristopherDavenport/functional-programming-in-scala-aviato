package fpis.chapter06

object Random {
  trait RNG {  def nextInt: (RNG, Int)}

  final case class Rand[A](f: RNG => (RNG, A))

  

}