package fpis.chapter06

object Rand2 {
  trait RNG
  // trait RNG {
  //   def nextInt: (Int, RNG)
  // }
  final case class Rand[A](f: RNG => (A, RNG)){
    def map[B](f: A => B): Rand[B] = Rand2.map(this)(f)
    def map2[B, C](that: Rand[B])(f: (A, B) => C) = Rand2.map2(this, that)(f)
    def flatMap[B](f: A => Rand[B]): Rand[B] = Rand2.flatMap(this)(f)
  }


  // def map[A,B](randA: Rand[A])(f: A => B): Rand[B] = 
    // Rand[B]{
    //   rng => 
    //   val (a, endRNG) = randA.f(rng)
    //   (f(a), endRNG)
    // }
  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = 
    rand.flatMap(f.andThen(pure))


  def pure[A](a: A): Rand[A] = Rand[A]({rng => (a, rng)})
  // def map2ApplicativeOnly[A, B, C](randA: Rand[A], randB: Rand[B])(f: (A, B) => C): Rand[C] = {
  //   Rand{ rngInit =>
  //     val (a, rngMid) = randA.f(rngInit)
  //     val (b, rngFinal) = randB.f(rngMid)
  //     (f(a, b), rngFinal)
  //   }
  // }

  def map2[A,B,C](randA: Rand[A], randB: Rand[B])(f: (A, B) => C):Rand[C] =
    randA.flatMap(a => randB.map(f(a, _)))
    // for {
    //   a <- randA
    //   b <- randB
    // } yield f(a, b)


  def flatMap[A, B](randA: Rand[A])(f: A => Rand[B]): Rand[B] = {
    Rand{ rngInit =>
      val (a, rngMid) = randA.f(rngInit)
      val (b, rngFinal) = f(a).f(rngMid)
      (b, rngFinal)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(pure(List.empty[A]))(map2(_, _)(_ :: _))




  def sequenceComplex[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(pure(List.empty[A])){ case (Rand(rA), Rand(rList)) => 
      Rand{
        rng => 
        
        val (a, nextRNG) = rA(rng)
        val (l, endRNG) = rList(nextRNG)
        

        val finalListA : List[A] = a :: l
        val finalRNG : RNG = endRNG
        (finalListA, finalRNG)
      }
    }
    
}