/*
 * YARR - yet another rss reader
 *
 * Copyright (C) 2013  Semjon Popugaev (senia).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.antabaka.yarr

import org.scalatest.matchers.{Matcher, MatchResult}
import scalaz.Scalaz._
import rss.ValS

trait FailMatching {
  def failN(n: Int) = new Matcher[ValS[_]] {
    def errCount(v: ValS[_]) = v.toEither.left.toOption.map{_.length}.getOrElse(0)
    def apply(v: ValS[_]) = MatchResult(
      errCount(v) == n,
      s"Validation should contain $n errors. Validation: $v.",
      s"Validation contains $n errors. Validation: $v."
    )
  }
}
