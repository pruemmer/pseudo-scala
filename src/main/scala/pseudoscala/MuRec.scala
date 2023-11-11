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

object murec {

  def μ(f : ℕ => ℕ)() : ℕ = {
    var res : ℕ = 0
    while (f(res) > 0)
      res = res + 1
    res
  }

  def μ(f : (ℕ, ℕ) => ℕ)
       (x1 : ℕ) : ℕ = {
    var res : ℕ = 0
    while (f(x1, res) > 0)
      res = res + 1
    res
  }

  def μ(f : (ℕ, ℕ, ℕ) => ℕ)
       (x1 : ℕ, x2 : ℕ) : ℕ = {
    var res : ℕ = 0
    while (f(x1, x2, res) > 0)
      res = res + 1
    res
  }

  def μ(f : (ℕ, ℕ, ℕ, ℕ) => ℕ)
       (x1 : ℕ, x2 : ℕ, x3 : ℕ) : ℕ = {
    var res : ℕ = 0
    while (f(x1, x2, x3, res) > 0)
      res = res + 1
    res
  }

  def μ(f : (ℕ, ℕ, ℕ, ℕ, ℕ) => ℕ)
       (x1 : ℕ, x2 : ℕ, x3 : ℕ, x4 : ℕ) : ℕ = {
    var res : ℕ = 0
    while (f(x1, x2, x3, x4, res) > 0)
      res = res + 1
    res
  }

}