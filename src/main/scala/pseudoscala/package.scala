

package object pseudoscala {

  type ℕ = BigNat
  val ℕ = BigNat

  type ℤ = BigInt
  val ℤ = BigInt

  type 𝔹 = Boolean
  val 𝔹 = Boolean


  implicit def int2BigNat(i : Int) : BigNat =
    BigNat(i)

  implicit def long2BigNat(i : Long) : BigNat =
    BigNat(i)

//  implicit def bigInt2BigNat(i : BigInt) : BigNat =
//    BigNat(i)

  implicit def bigNat2BigInt(i : BigNat) : BigInt =
    i.toℤ

  def ∅[A] : Set[A] = Set.empty[A]

  val ε : Seq[Nothing] = Seq()

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

  class RichBoolean(underlying : Boolean) {
    def ∧(that : Boolean) : Boolean = underlying && that
    def ∨(that : Boolean) : Boolean = underlying || that
    def →(that : Boolean) : Boolean = !underlying || that
    def ↔(that : Boolean) : Boolean = underlying == that
    def ⟷(that : Boolean) : Boolean = underlying == that
    def unary_¬ : Boolean = !underlying
  }

  implicit def toRichBoolean(b : Boolean) : RichBoolean =
    new RichBoolean(b)

  class RichBigInt(underlying : BigInt) {
    def ≥(that : BigInt) : Boolean =
      underlying >= that
    def ≥(that : Int) : Boolean =
      underlying >= that

    def ≤(that : BigInt) : Boolean =
      underlying <= that
    def ≤(that : Int) : Boolean =
      underlying <= that

    def toℕ : BigNat =
      BigNat(underlying)
  }

  implicit def toRichBigInt(a : BigInt) : RichBigInt =
    new RichBigInt(a)

  class RichSeq[A](underlying : Seq[A]) {
    def ⌢(that : Seq[A]) : Seq[A] = underlying ++ that
  }

  implicit def toRichSeq[A](s : Seq[A]) : RichSeq[A] =
    new RichSeq(s)

  def any[A](s : Set[A]) : A = {
    require(!s.isEmpty, "could not pick an element from the empty set")
    s.iterator.next
  }
}
