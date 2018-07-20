package fpis

import scala.io.StdIn.readLine

object Main {

  def main(args: Array[String]): Unit = {
    import fpis.chapter05.Synchronicity._

    val program : Sync[Unit] = Sync.delay(println("Put Something useful to do"))
    .flatMap(_ => Sync.delay{throw new Throwable("Boom!"); ""})
    .flatMap(string => Sync.delay(readLine))
    .recover(_ => "I recovered")
    .flatMap(l => Sync.delay(println(s"I got: $l")))
    

    Sync.unsafeRunSync(program)    
  }

}