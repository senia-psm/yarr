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

import scalaz._
import Scalaz._
import scala.xml.{NodeSeq, Node}
import com.github.nscala_time.time.Imports._

/** Parsed RSS xml
  *
  * @param  metadata        RSS channel metadata
  * @param  items           RSS channel items
  * @param  pubDate         The publication date for the content in the channel. For example, the <i>New York Times</i> publishes on a daily basis, the publication date flips once every 24 hours. That's when the pubDate of the channel changes. All date-times in RSS conform to the Date and Time Specification of [[http://asg.web.cmu.edu/rfc/rfc822.html RFC 822]], with the exception that the year may be expressed with two characters or four characters (four preferred). Example: ''Sat, 07 Sep 2002 00:00:01 GMT''
  * @param  lastBuildDate   The last time the content of the channel changed.Example: ''Sat, 07 Sep 2002 09:42:31 GMT''
  */
case class RssPacket(metadata: Channel, items: Seq[Item], pubDate: Option[DateTime], lastBuildDate: Option[DateTime])
object RssPacket{

  private def getDate(dateFmt: String)(seq: NodeSeq): ValS[Option[DateTime]] = seq.headOption.map{ _.text.toDateOpt(dateFmt) }.sequence.map{ _.flatten }

  def getPubDate(elem: Node, dateFmt: String): ValS[Option[DateTime]] = getDate(dateFmt)(elem \ "pubDate")

  def getLastBuildDate(elem: Node, dateFmt: String): ValS[Option[DateTime]] = getDate(dateFmt)(elem  \ "lastBuildDate")

  def parce(xml: Node, dateFmt: String): ValS[RssPacket] =
    for {
      chanXml <- xml.singleNode("channel")
      packet <- (
        Channel.parse(chanXml) |@|
          Item.parseItems(chanXml, dateFmt).toList.sequence |@|
          getPubDate(chanXml, dateFmt) |@|
          getLastBuildDate(chanXml, dateFmt)
        ) { RssPacket.apply _ }
    } yield packet

}
