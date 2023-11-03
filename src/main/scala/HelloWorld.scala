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

@main def HelloWorld: Unit =
  println("Testing Pseudo-Scala!")
  println
  testNaturals
  println
  testSets

def testNaturals = {

  println("Testing ℕ")

  val x : ℕ = 42

  println(x + 1)

  def fib(n : ℕ) : ℕ =
    if (n == 0) {
      0
    } else if (1 == n) {
      1
    } else {
      fib(n - 1) + fib(n - 2)
    }

  for (n <- 0 to 10)
    println(s"fib($n) = ${fib(n)}")

}

def testSets = {

  val s : Set[ℤ] = Set(1, 2, 3)

  println("Testing set: " + s)

  if (s == ∅)
    println("empty")
  else
    println(s"${s.size} elements")

  def powerSet(s : Set[ℤ]) : Set[Set[ℤ]] =
    if (s == ∅) {
      Set(∅)
    } else {
      val el = any(s)
      val ps = powerSet(s \ Set(el))
      ps ∪ (for (x <- ps) yield (x ∪ Set(el)))
    }

  val ps = powerSet(s)

  println("Powerset: " + ps)

  println("Powerset contains " + Set(1, 2) + ": " + (Set[ℤ](1, 2) ∈ ps))

}
