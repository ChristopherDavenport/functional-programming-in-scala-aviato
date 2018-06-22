package fpis.chapter05

object Chapter5 {
  sealed trait MStream[A]
  private final case class MEmpty[A]() extends MStream[A]
  private final case class MCons[A](h: () => A, t: () => MStream[A]) extends MStream[A]

  object MStream {
    def cons[A](hd: => A, tl: => MStream[A]): MStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      MCons(() => head, () => tail)
    }
    def empty[A]: MStream[A] = MEmpty()
    def apply[A](as: A*): MStream[A] = 
      if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
    def of[A](hd: => A, rest: A*): MStream[A] =
      cons(hd, apply[A](rest:_*))

    def toList[A](xa: MStream[A]): List[A] = xa match {
      case MEmpty() => List.empty[A]
      case MCons(h, t) => h() :: toList(t())
    }

    def take[A](xa: MStream[A])(n: Int): MStream[A] = xa match {
      case MEmpty() => empty[A]
      case MCons(h, t) => if (n <= 0) empty[A] else cons(h(), take(t())(n - 1))
    }

    def drop[A](xa: MStream[A])(n: Int): MStream[A] = xa match {
      case MEmpty() => empty[A]
      case c@MCons(_, t) => if (n >= 0) drop(t())(n - 1) else c
    }

    def takeWhile[A](xa: MStream[A])(f: A => Boolean): MStream[A] = xa match {
      case MEmpty() => empty[A]
      case MCons(h, t) if f(h()) => cons(h(), takeWhile(t())(f))
      case _ => empty[A]
    }
    def exists[A](xa: MStream[A])(f: A => Boolean): Boolean = xa match {
      case MCons(h, t) => f(h()) || exists(t())(f)
      case _ => false
    }

    def foldRight[A, B](xa: MStream[A])(z: => B)(f: (A, => B) => B) : B = xa match {
      case MCons(h, t) => f(h(), foldRight(t())(z)(f))
      case _ => z
    }

    def forAll[A](xa: MStream[A])(f: A => Boolean): Boolean = 
      foldRight[A, Boolean](xa)(true){case (a, b) => f(a) && b}

    def forAllRaw[A](xa: MStream[A])(f: A => Boolean): Boolean = xa match {
      case MEmpty() => true
      case MCons(h, _) if !f(h()) => false
      case MCons(_, t) => forAllRaw(t())(f)
    }

    def takeWhileFoldRight[A](xa: MStream[A])(f: A => Boolean): MStream[A] =
      foldRight(xa)(MStream.empty[A]){ case (a, b) => if (f(a)) cons(a, b) else b}

    def headOptionFoldRight[A](xa: MStream[A]): Option[A] = 
      foldRight(xa)(Option.empty[A]){ case (a, _) => Some(a)}
  
    def map[A, B](xa: MStream[A])(f: A => B): MStream[B] = 
      foldRight(xa)(empty[B]){ case (a, b) => cons(f(a), b)}

    def filter[A](xa: MStream[A])(f: A => Boolean): MStream[A] = 
      foldRight(xa)(empty[A]){ case (a, b) => if (f(a)) cons(a, b) else b}

    def append[A](xa: MStream[A])(that: MStream[A]): MStream[A] =
      foldRight(xa)(that)(cons(_, _))

    def flatMap[A, B](xa: MStream[A])(f: A => MStream[B]): MStream[B] =
      foldRight(xa)(empty[B]){ case (a, b) => append(f(a))(b)}

    def constant[A](a: A): MStream[A] = 
      cons(a, constant(a))

    def from(i: Int): MStream[Int] = 
      cons(i, from(i + 1))
    
    def fibs: MStream[Int] = {
      def fibsInt(lower: Int, higher: Int): MStream[(Int, Int)] = 
        cons((lower, higher), fibsInt(higher, higher + lower))
      map(fibsInt(0, 1))(_._1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): MStream[A] = 
      f(z).fold(empty[A]){ case (a, s) => cons(a, unfold(s)(f))}

    def fibsViaUnfold: MStream[Int] = unfold((0, 1)){ case (lower, higher) => Some((lower, (higher, higher + lower)))}

    def constantViaUnfold[A](a: A): MStream[A] = unfold(a)(a => Some((a, a)))
    def onesViaUnfold: MStream[Int] = constantViaUnfold(1)

    def mapViaUnfold[A, B](xa: MStream[A])(f: A => B): MStream[B] = 
      unfold(xa){ 
        case MCons(h, t) => Some((f(h()), t()))
        case MEmpty() => None
      }

    def takeViaUnfold[A](xa: MStream[A])(n: Int): MStream[A] = 
      unfold((xa, n)){
        case ((MCons(h, _), 1)) => Some((h(), (empty[A], 0)))
        case (MCons(h, t), n) if n > 1 => Some((h(), (t(), n -1)))
        case _ => None
      }

    def takeWhileViaUnfold[A](xa: MStream[A])(f: A => Boolean): MStream[A] =
      unfold(xa){
        case MCons(h, t) if f(h()) => Some((h(), t()))
        case _ => None
      }

    def zipWith[A, B, C](xa: MStream[A])(that: MStream[B])(f: (A, B) => C): MStream[C] = 
      unfold((xa, that)){
        case ((MCons(h1, t1), MCons(h2, t2))) => 
          Some((f(h1(), h2()), (t1(), t2())))
        case _ => None
      }
    
    def zip[A, B](xa: MStream[A])(that: MStream[B]): MStream[(A, B)] = zipWith(xa)(that)((_, _))

    def zipWithAll[A, B, C](xa: MStream[A])(that: MStream[B])(f: (Option[A], Option[B]) => C): MStream[C] =
      unfold((xa, that)){
        case (MEmpty(), MEmpty()) => None
        case (MCons(h,t), MEmpty()) => Some((f(Some(h()), None), (t(), empty[B])))
        case (MEmpty(), MCons(h, t)) => Some((f(None, Some(h())), (empty[A], t())))
        case (MCons(h1, t1), MCons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      }

    def zipAll[A, B](xa: MStream[A])(that: MStream[B]): MStream[(Option[A], Option[B])] =
      zipWithAll(xa)(that)((_, _))



    
  }
  

}