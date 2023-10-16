

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

  def ∅[A] : Set[A] = Set.empty[A]

  class RichSet[A](underlying : Set[A]) {
    def any : A = pseudoscala.any(underlying)

    def ∪(that : Set[A]) =
      underlying ++ that
    def ∩(that : Set[A]) =
      underlying & that
    def \(that : Set[A]) =
      underlying -- that

    def ⊆(that : Set[A]) =
      underlying subsetOf that
    def ⊊(that : Set[A]) =
      (underlying subsetOf that) && !(that subsetOf underlying)
    def ⊇(that : Set[A]) =
      that subsetOf underlying
    def ⊋(that : Set[A]) =
      !(underlying subsetOf that) && (that subsetOf underlying)
  }

  implicit def toRichSet[A](s : Set[A]) : RichSet[A] =
    new RichSet(s)

  class RichElement[A](underlying : A) {
    def ∈(s : Set[A]) = s contains underlying
    def ∉(s : Set[A]) = !(s contains underlying)
  }

  implicit def toRichElement[A](a : A) : RichElement[A] =
    new RichElement(a)

  def any[A](s : Set[A]) : A = {
    require(!s.isEmpty)
    s.iterator.next
  }
}
