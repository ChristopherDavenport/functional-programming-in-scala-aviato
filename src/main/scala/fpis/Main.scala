package fpis

// import fpis.chapter03.Chapter3.MList._

object Main {

  def main(args: Array[String]): Unit = {
    import fpis.chapter05.Nats._

    // (1 to 10).toList.map(i => (i, Nat.fromInt(i))).foreach(println)

    val one = Nat.fromInt(5)
    val two = Nat.fromInt(2)
    val something = for {
      o <- one
      t <- two
      s <- Nat.subtraction(o, t)
    } yield s


    println(something.get)

    // val i = Nat.fromInt(2)
    // println(i)
    // println(initList)
    // println(initOfList)
  }

}