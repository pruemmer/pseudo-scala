
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

  println(s"The GCD of 12 and 18 is ${gcd_iterative(12, 18)}")

}
