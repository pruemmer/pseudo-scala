
@main def hello: Unit =
  println("Hello world!")
  println(msg)
  testNaturals

def msg = "I was compiled by Scala 3. :)"

def testNaturals = {

  import pseudoscala._

  println("Testing ℕ")

  val x : ℕ = 42

  println(x + 1)

/*
  def fib(n : ℕ) : ℕ = n match {
    case 0 => 0
    case 1 => 1
    case n => fib(n - 1) + fib(n - 2)
  }
 */

  def fib(n : ℕ) : ℕ =
    if (n == 0) {
      0
    } else if (1 == n) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }

  for (n <- 0 to 10)
    println(s"fib($n) = ${fib(n)}")

}
