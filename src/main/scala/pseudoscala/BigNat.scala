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



package pseudoscala

import scala.math.{ScalaNumber, ScalaNumericConversions}


object BigNat {

  def apply(i : Int) : BigNat = {
    require(i >= 0, "natural numbers are non-negative")
    new BigNat(i)
  }

  def apply(i : Long) : BigNat = {
    require(i >= 0, "natural numbers are non-negative")
    new BigNat(i)
  }

  def apply(i : BigInt) : BigNat = {
    require(i >= 0, "natural numbers are non-negative")
    new BigNat(i)
  }
  
  def apply(i : String) : BigNat = {
    require(BigInt(i) >= 0, "natural numbers are non-negative")
    new BigNat(BigInt(i))
  }
  
  def unapply(a : BigNat) : Option[Int] =
    if (a.underlying >= Int.MinValue && a.underlying <= Int.MaxValue)
      Some(a.intValue)
    else
      None

  def wrapNat(num : BigInt) : BigNat =
    new BigNat(num max 0)

  trait BigNatIsIntegral extends Integral[BigNat] {
    def plus(x: BigNat, y: BigNat): BigNat = x + y
    def minus(x: BigNat, y: BigNat): BigNat = x - y
    def times(x: BigNat, y: BigNat): BigNat = x * y
    def quot(x: BigNat, y: BigNat): BigNat = x / y
    def rem(x: BigNat, y: BigNat): BigNat = x % y
    def negate(x: BigNat): BigNat = -x
    def fromInt(x: Int): BigNat = BigNat(x)
    def toInt(x: BigNat): Int = x.toInt
    def toLong(x: BigNat): Long = x.toLong
    def toFloat(x: BigNat): Float = x.toFloat
    def toDouble(x: BigNat): Double = x.toDouble

    def parseString(str : String) : Option[BigNat] = {
      try {
        Some(BigNat(str))
      } catch {
        case _ : NumberFormatException => None
      }
    }
  }

  trait BigNatOrdering extends Ordering[BigNat] {
    def compare(x: BigNat, y: BigNat) = x compare y
  }

  implicit object BigNatIsIntegral extends BigNatIsIntegral with BigNatOrdering

}

class BigNat private (val underlying : BigInt)
      extends ScalaNumber with ScalaNumericConversions with Ordered[BigNat] {

  import BigNat._

  override def hashCode(): Int = underlying.hashCode * 1234567

  override def equals(that: Any): Boolean = that match {
    case that : BigNat => this equals that
    case x             => underlying equals x
  }

  def equals (that: BigNat): Boolean = compare(that) == 0

  def compare(that: BigNat): Int = this.underlying compare that.underlying

  def +(that : BigNat) : BigNat =
    wrapNat(this.underlying + that.underlying)

  def -(that : BigNat) : BigNat =
    wrapNat(this.underlying - that.underlying)

  def unary_- : BigNat =
    wrapNat(-this.underlying)

  def *(that : BigNat) : BigNat =
    wrapNat(this.underlying * that.underlying)

  def /(that : BigNat) : BigNat =
    wrapNat(this.underlying / that.underlying)

  def %(that : BigNat) : BigNat =
    wrapNat(this.underlying % that.underlying)

  def ≥(that : BigNat) : Boolean =
    this >= that
  def ≥(that : Int) : Boolean =
    this >= that

  def ≤(that : BigNat) : Boolean =
    this <= that
  def ≤(that : Int) : Boolean =
    this <= that

  def divides(that : BigNat) : Boolean =
    if (this.underlying == 0)
      that.underlying == 0
    else
      that.underlying % this.underlying == 0

  def intValue : Int = {
    require(underlying <= Int.MaxValue)
    underlying.toInt
  }

  def longValue : Long = {
    require(underlying <= Long.MaxValue)
    underlying.toLong
  }

  def floatValue : Float =
    underlying.toFloat

  def doubleValue : Double =
    underlying.toDouble

  def toBigInt : BigInt =
    underlying

  def toℤ : BigInt =
    underlying

  override def toString =
    underlying.toString

  def isWhole : Boolean = true

}
