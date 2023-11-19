/**
 * This file is part of Pseudo-Scala.
 * <https://github.com/pruemmer/pseudo-scala>
 *
 * Copyright (C) 2023 Philipp Ruemmer <ph_r@gmx.net>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 * 
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * 
 * * Neither the name of the authors nor the names of their
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


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

  def head[A](l : Seq[A]) : A = l.head
  def tail[A](l : Seq[A]) : Seq[A] = l.tail

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

    def ≠(that : BigInt) : Boolean =
      underlying != that
    def ≠(that : Int) : Boolean =
      underlying != that

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

  def skip : Unit = {}
}
