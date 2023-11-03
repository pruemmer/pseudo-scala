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
