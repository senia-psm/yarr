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

import org.antabaka.yarr.rss.{ValS, RssPacket}
import scalaz._
import Scalaz._
import scala.xml.{Elem, XML}
import scala.util.Try
import java.net.URL
import com.weiglewilczek.slf4s.Logging

object Downloader extends Logging{

  def getRawData(urlAddress: String): ValS[Elem] = {
//    val url = Try{ new URL("""http://habrahabr.ru/rss/hubs/all/""") } // MalformedURLException
    val url = Try{ new URL(urlAddress) } // MalformedURLException
    val stream = url.map{ _.openStream } // IOException
    val xml = stream.map{ XML.load }
    xml match {
      case util.Success(s) => s.success
      case util.Failure(f) => f.toString.failNel
    }
  }

  def getRssPacket(url: String, dateFmt: String): ValS[RssPacket] =
    for {
      xml <- getRawData(url)
      packet <- RssPacket.parce(xml, dateFmt)
    } yield packet
}
