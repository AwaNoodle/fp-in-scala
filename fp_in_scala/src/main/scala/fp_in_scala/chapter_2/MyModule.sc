object MyModule {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def calc(n: Int, cur: Int, pre: Int): Int = {
     if(n == 0) cur
     else calc(n-1, cur+pre, cur)
    }

    calc(n, 0, 1)
  }

  def abs(n: Int): Int = {
    if(n < 0) -n else n
  }

  private def formatAbs(x: Int) = {
    format("The absolute value of %d is %d", x, abs)
  }

  private def formatFibonacci(x: Int) = {
    format("The %d Fibonacci is %d", x, fibonacci)
  }

  private def format(message: String, init: Int, f: Int => Int) = {
    message.format(init, f(init))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFibonacci(7))
  }
}

MyModule.main(Array())
