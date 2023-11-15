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

import pseudoscala.nondet._

/**
 * Boolean SATisfiability problem.
 */
object SAT extends App {

  abstract sealed class For
  case class Prop(name : String)    extends For
  case class Not (f : For)          extends For
  case class And (f : For, g : For) extends For
  case class Or  (f : For, g : For) extends For

  type PartialModel = Map[String, Boolean]

  def propositions(f : For) : Set[String] = f match {
    case Prop(name) => Set(name)
    case Not (f)    => propositions(f)
    case And (f, g) => propositions(f) ++ propositions(g)
    case Or  (f, g) => propositions(f) ++ propositions(g)
  }

  def eval(f : For, m : PartialModel) : Boolean = f match {
    case Prop(name) => m(name)
    case Not (f)    => !eval(f, m)
    case And (f, g) => eval(f, m) && eval(g, m)
    case Or  (f, g) => eval(f, m) || eval(g, m)
  }

  def solve(f : For) : Option[PartialModel] =
    btSearch {
      val props = propositions(f).toList.sorted
      solveRec(f, Map(), props)
    }

  private def solveRec(f : For, m : PartialModel,
                       props : List[String]) : Unit =
    props match {
      case List() => {
        assume(eval(f, m))
        success(m)
      }
      case prop :: otherProps =>
        choose {
          (v : Boolean) =>
          solveRec(f, m + (prop -> v), otherProps)
        }
    }

  val f1 = And(Prop("p"), Or(Not(Prop("p")), Prop("q")))

  println("f1: " + solve(f1))

  val f2 = And(And(Prop("p"),
                   Or(Not(Prop("p")), Prop("q"))),
                   Or(Not(Prop("q")), Not(Prop("q"))))

  println("f2: " + solve(f2))

}
