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

  /**
   * For 0 <= cond <= 1:
   * if (cond == 0) thenCase else elseCase
   * 
   * Note: this definition is not primitive recursive, but it has the
   * same behaviour as the primitive recursive version.
   */
  def ite(cond : ℕ, thenCase : ℕ, elseCase : ℕ) : ℕ =
    if (cond == 0) thenCase else elseCase

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

object MuRecTest extends App {

  import PrimRecLib._
  import pseudoscala.murec._

  def polyDiff(n : ℕ) : ℕ = minus(27, mult(mult(n, n), n))
  def polyZero() : ℕ = μ(polyDiff)()

  println(s"polyZero() = ${polyZero()}")

}
