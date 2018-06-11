package fpis.chapter05

object Chapter5 {
  sealed trait MStream[+A]
  object MStream {
    def cons[A](hd: => A, tl: => MStream[A]): MStream[A] = {
      lazy val head = hd
      lazy val tail = tl
      MCons(() => head, () => tail)
    }
    def empty[A]: MStream[A] = MEmpty
    def apply[A](as: A*): MStream[A] = 
      if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
    def of[A](hd: => A, rest: A*): MStream[A] =
      cons(hd, apply[A](rest:_*))

    def toList[A](xa: MStream[A]): List[A] = xa match {
      case MEmpty => List.empty[A]
      case MCons(h, t) => h() :: toList(t())
    }

    def take[A](xa: MStream[A])(n: Int): MStream[A] = xa match {
      case MEmpty => empty[A]
      case MCons(h, t) => if (n <= 0) empty[A] else cons(h(), take(t())(n - 1))
    }
    def drop[A](xa: MStream[A])(n: Int): MStream[A] = xa match {
      case MEmpty => empty[A]
      case c@MCons(_, t) => if (n >= 0) drop(t())(n - 1) else c
    }
    def takeWhile[A](xa: MStream[A])(f: A => Boolean): MStream[A] = xa match {
      case MEmpty => empty[A]
      case MCons(h, t) => {
        val head = h()
        if (f(head)) cons(head, takeWhile(t())(f))
        else empty[A]
      }
    }
    def exists[A](xa: MStream[A])(f: A => Boolean): Boolean = xa match {
      case MCons(h, t) => f(h()) || exists(t())(f)
      case _ => false
    }

    def foldRight[A, B](xa: MStream[A])(z: => B)(f: (A, => B) => B) : B = xa match {
      case MCons(h, t) => f(h(), foldRight(t())(z)(f))
      case _ => z
    }

    def forAll[A](xa: MStream[A])(f: A => Boolean): Boolean = xa match {
      case MCons(h, t) => f(h()) && forAll(t())(f)
      case _ => true
    }



    private final case object MEmpty extends MStream[Nothing]
    private final case class MCons[A](h: () => A, t: () => MStream[A]) extends MStream[A]
  }
  

}