// Scala-atb
//
// This software is distributed under The MIT License (MIT):
//
// Copyright (c) 2013 Craig Eales
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package atb
package formula {

  object Hierarchy {

    // import BooleanFormula._

    // def increment(baseFormulae: Set[BooleanFormula]):
    //     Set[BooleanFormula] = {

    //   val nots =
    //     for {
    //       x <- baseFormulae
    //     } yield Not(x): BooleanFormula

    //   val ands =
    //     for {
    //       x <- baseFormulae
    //       y <- baseFormulae
    //     } yield And(x,y): BooleanFormula

    //   val ors =
    //     for {
    //       x <- baseFormulae
    //       y <- baseFormulae
    //     } yield Or(x,y): BooleanFormula

    //   nots `union` ands `union` ors
    // }

    // def levelOne(variables: Set[VarType]): Set[BooleanFormula] = {
    //   val vars: Set[BooleanFormula] = variables map Variable
    //   val bools: Set[BooleanFormula] = Set(True,False)
    //     vars union bools
    // }

    // def nextLevel(baseFormulae: Set[BooleanFormula]) =
    //     baseFormulae union increment(baseFormulae)

    // def formulaeOfLevelN(variables: Set[VarType],level: Int):
    //     Set[BooleanFormula] = {

    //   require(level > 0)

    //   if ( level == 1 )
    //     levelOne(variables)
    //   else
    //     nextLevel(formulaeOfLevelN(variables,level-1))
    // }

  }

}
