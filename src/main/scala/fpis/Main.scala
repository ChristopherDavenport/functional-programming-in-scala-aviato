package fpis

object Main {

  def main(args: Array[String]): Unit = {
    import fpis.chapter06.State._
    import scala.concurrent.ExecutionContext.Implicits.global

    val output = simpleProgram("5")(1)
    println(output)
    

  }

}