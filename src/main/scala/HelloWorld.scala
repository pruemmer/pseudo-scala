
import pseudoscala._

@main def HelloWorld: Unit =
  println("Testing Pseudo-Scala!")
  println
  testNaturals
  println
  testSets

def testNaturals = {

  println("Testing ℕ")

  val x : ℕ = 42

  println(x + 1)

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

def testSets = {

  val s : Set[ℤ] = Set(1, 2, 3)

  println("Testing set: " + s)

  if (s == ∅)
    println("empty")
  else
    println(s"${s.size} elements")

  def powerSet(s : Set[ℤ]) : Set[Set[ℤ]] =
    if (s == ∅) {
      Set(∅)
    } else {
      val el = any(s)
      val ps = powerSet(s \ Set(el))
      ps ∪ (for (x <- ps) yield (x ∪ Set(el)))
    }

  val ps = powerSet(s)

  println("Powerset: " + ps)

  println("Powerset contains " + Set(1, 2) + ": " + (Set[ℤ](1, 2) ∈ ps))

}
