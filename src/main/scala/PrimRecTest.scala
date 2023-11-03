
import pseudoscala._
import pseudoscala.primitiverec._

object PrimRecLib {

  def id(x : ℕ) : ℕ = x
  def one() = succ(zero())

  def plusBase(x : ℕ) : ℕ = π_1(x)
  def plusStep(x : ℕ, y : ℕ, z : ℕ) : ℕ = succ(π_3(x, y, z))
  def plus(x : ℕ, y : ℕ) : ℕ = primrec(plusBase, plusStep)(x, y)

  def predBase() : ℕ = zero()
  def predStep(y : ℕ, p : ℕ) = y
  def pred(y : ℕ) : ℕ = primrec(predBase _, predStep)(y)

  def multBase(x : ℕ) : ℕ = zero()
  def multStep(x : ℕ, y : ℕ, p : ℕ) : ℕ = plus(p, x)
  def mult(x : ℕ, y : ℕ) : ℕ = primrec(multBase, multStep)(x, y)

  def minusBase(x : ℕ) : ℕ = x
  def minusStep(x : ℕ, y : ℕ, p : ℕ) : ℕ = pred(p)
  def minus(x : ℕ, y : ℕ) : ℕ = primrec(minusBase, minusStep)(x, y)

  def iteHelpBase(t : ℕ, e : ℕ) : ℕ = t
  def iteHelpStep(t : ℕ, e : ℕ, y : ℕ, p : ℕ) : ℕ = e
  def iteHelp(t : ℕ, e : ℕ, y : ℕ) : ℕ =
    primrec(iteHelpBase, iteHelpStep)(t, e, y)

  /**
   * For 0 <= cond <= 1:
   * if (cond == 0) thenCase else elseCase
   */
  def ite(cond : ℕ, thenCase : ℕ, elseCase : ℕ) : ℕ =
    iteHelp(thenCase, elseCase, cond)

  def signumBase() : ℕ = zero()
  def signumStep(y : ℕ, p : ℕ) = one()
  def signum(y : ℕ) : ℕ = primrec(signumBase _, signumStep)(y)

  def leq(x : ℕ, y : ℕ) : ℕ = signum(minus(x, y))

}

object PrimRecTest extends App {

  import PrimRecLib._

  println(s"plus(5, 2) = ${plus(5, 2)}")

  println(s"pred(10) = ${pred(10)}")

  println(s"mult(5, 6) = ${mult(5, 6)}")

  println(s"minus(5, 3) = ${minus(5, 3)}")
  println(s"minus(5, 7) = ${minus(5, 7)}")

  println(s"leq(1, 1) = ${leq(1, 1)}")
  println(s"leq(2, 1) = ${leq(2, 1)}")
  println(s"leq(1, 2) = ${leq(1, 2)}")

}
