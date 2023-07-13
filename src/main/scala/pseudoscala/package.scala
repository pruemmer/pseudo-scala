

package object pseudoscala {

  type ℕ = BigNat
  val ℕ = BigNat

  type ℤ = BigInt
  val ℤ = BigInt

  implicit def int2BigNat(i : Int) : BigNat =
    BigNat(i)

  implicit def long2BigNat(i : Long) : BigNat =
    BigNat(i)

  implicit def bigInt2BigNat(i : BigInt) : BigNat =
    BigNat(i)

}
