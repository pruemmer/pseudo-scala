
import pseudoscala._

object Euclid extends App {

  def gcd_iterative(a : ℕ, b : ℕ) : ℕ = {
    require((a ≥ 1) ∧ (b ≥ 1))

    var x = a
    var y = b
    while ((x ≠ 0) ∧ (y ≠ 0)) {
      if (x ≥ y)
        x = x - y
      else
        y = y - x
    }

    if (x == 0)
      y
    else
      x
  }

  def gcd_recursive(a : ℕ, b : ℕ) : ℕ =
    if (a == 0)
      b
    else if (b == 0)
      a
    else if (a ≥ b)
      gcd_recursive(a - b, b)
    else
      gcd_recursive(a, b - a)

  println(s"Iterative: the GCD of 12 and 18 is ${gcd_iterative(12, 18)}")
  println(s"Iterative: the GCD of 12 and 19 is ${gcd_iterative(12, 19)}")
  println(s"Recursive: the GCD of 12 and 18 is ${gcd_recursive(12, 18)}")
  println(s"Recursive: the GCD of 12 and 19 is ${gcd_recursive(12, 19)}")

}
