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

package org.antabaka.yarr.rss

import scala.xml.Node
import scala.util.Try
import scalaz._
import Scalaz._
import com.github.nscala_time.time.Imports._
import scala.Some


object `package` {
  type ValS[T] = ValidationNel[String, T]
  implicit class Extractor(val n: Node) { // TODO:  extends AnyVal
    def singleNode(name: String): ValS[Node] =
      optNode(name).flatMap{
        _.map{_.success}.getOrElse(s"Node with name '$name' expected, nothing found.".failNel)
      }

    def optNode(name: String): ValS[Option[Node]] = {
      val res = n \ name
      if (res.lengthCompare(1) <= 0)
        res.headOption.success
      else
        s"Single node with name '$name' expected, ${res.length} nodes found: '$res'.".failNel
    }
    def singleTextNode(name: String): ValS[String] = singleNode(name).map{ _.text }
    def optTextNode(name: String): ValS[Option[String]] = optNode(name).map{ _.map{ _.text } }

    def singleIntNode(name: String): ValS[Int] = singleTextNode(name).flatMap{ _.toIntVal }
    def optIntNode(name: String): ValS[Option[Int]] =
      optTextNode(name).flatMap{
        case None => None.success
        case Some(s) => s.toIntVal.map{_.some}
      }
  }

  implicit class ConvertHelper(val s: String) {
    def toIntVal: ValS[Int] =
      Try {s.toInt} match {
        case util.Success(i) => i.success
        case util.Failure(t) => s"Can not convert '$s' to Int. Error: $t".failNel
      }
  }

  implicit class DateConverter(val date: String) extends AnyVal {
    def toDate(dateFmt: String): ValS[DateTime] =
      Try{
        DateTimeFormat.
          forPattern(dateFmt).
          withLocale(java.util.Locale.forLanguageTag("en")).
          parseDateTime(date)
      } match {
        case util.Success(s) => s.success
        case util.Failure(t) =>
          s"""Date parse error. Date: "$date", Pattern: "$dateFmt". Error: $t""".failNel
      }

    def toDateOpt(dateFmt: String): ValS[Option[DateTime]] =
      if (date.isEmpty)
        None.success
      else
        toDate(dateFmt).map{ _.some }
  }
}
