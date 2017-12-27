import Calculator.Calculator

object Main {
  def main(args: Array[String]): Unit = {
    println("plz enter an arithmetic expression in one line")
    while (true) {
      try {
        println(Calculator.calculate(scala.io.StdIn.readLine()))
      } catch {
        case e: Exception => println(e.getMessage)
      }
    }
  }
}
