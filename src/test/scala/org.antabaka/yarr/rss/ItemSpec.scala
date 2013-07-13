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

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import scalaz.{Category => _, Source => _, _}, Scalaz._
import org.antabaka.yarr.FailMatching

class ItemSpec extends FunSpec with ShouldMatchers with FailMatching {
  describe("An Enclosure parser") {
    it ("should parse valid example") {
      Enclosure.parse(
        <enclosure url="http://www.scripting.com/mp3s/weatherReportSuite.mp3" length="12216320" type="audio/mpeg" />
      ) should equal(
        Enclosure(url = "http://www.scripting.com/mp3s/weatherReportSuite.mp3", length = "12216320", mimeType = "audio/mpeg").success
      )
    }

    it ("should fail on format error") {
      Enclosure.parse(
          <enclosure />
      ) should failN(3)
    }
  }

  describe("A Guid parser") {
    it ("should parse an example without isPermaLink") {
      Guid.parse(
        <guid>http://some.server.com/weblogItem3207</guid>
      ) should equal(
        Guid(value = "http://some.server.com/weblogItem3207").success
      )
    }

    it ("should parse an example with isPermaLink") {
      Guid.parse(
        <guid isPermaLink="false">http://inessential.com/2002/09/01.php#a2</guid>
      ) should equal(
        Guid(value = "http://inessential.com/2002/09/01.php#a2", isPermaLink = false).success
      )

      Guid.parse(
        <guid isPermaLink="true">http://inessential.com/2002/09/01.php#a2</guid>
      ) should equal(
        Guid(value = "http://inessential.com/2002/09/01.php#a2", isPermaLink = true).success
      )
    }
  }

  describe("A Source parser") {
    it ("should parse valid example") {
      Source.parse(
        <source url="http://www.tomalak.org/links2.xml">Tomalak's Realm</source>
      ) should equal(
        Source(value = "Tomalak's Realm", url = "http://www.tomalak.org/links2.xml").success
      )
    }

    it ("should fail on format error") {
      Source.parse(
        <source>Tomalak's Realm</source>
      ) should failN(1)
    }
  }

  describe("An Item parser") {
    it ("should parce an example with <title> only") {
      Item.parse(
        <item><title>Venice Film Festival Tries to Quit Sinking</title></item>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should equal(
        Item(title = "Venice Film Festival Tries to Quit Sinking".some).success
      )
    }

    it ("should parce an example with <description> only") {
      Item.parse(
        <item><description>Some of the most heated chatter at the Venice Film Festival this week was about the way that the arrival of the stars at the Palazzo del Cinema was being staged.</description></item>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should equal(
        Item(description = "Some of the most heated chatter at the Venice Film Festival this week was about the way that the arrival of the stars at the Palazzo del Cinema was being staged.".some).success
      )
    }

    it ("should fail on format error") {
      Item.parse(
        <item></item>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should failN(1)

      Item.parse(
        <item>
          <title>Venice Film Festival Tries to Quit Sinking</title>
          <title>Venice Film Festival Tries to Quit Sinking</title>
          <description>Some of the most heated chatter at the Venice Film Festival this week was about the way that the arrival of the stars at the Palazzo del Cinema was being staged.</description>
        </item>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should failN(1)
    }

    it ("should parse an example with all elements") {
      Item.parse(
        <item>
          <title>Venice Film Festival Tries to Quit Sinking</title>
          <description>Some of the most heated chatter at the Venice Film Festival this week was about the way that the arrival of the stars at the Palazzo del Cinema was being staged.</description>
          <link>http://nytimes.com/2004/12/07FEST.html</link>
          <author>lawyer@boyer.net (Lawyer Boyer)</author>
          <category domain="http://www.fool.com/cusips">MSFT</category>
          <comments>http://ekzemplo.com/entry/4403/comments</comments>
          <enclosure url="http://www.scripting.com/mp3s/weatherReportSuite.mp3" length="12216320" type="audio/mpeg" />
          <guid isPermaLink="false">http://inessential.com/2002/09/01.php#a2</guid>
          <pubDate>Sun, 19 May 2002 15:21:36 GMT</pubDate>
          <source url="http://www.tomalak.org/links2.xml">Tomalak's Realm</source>
        </item>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should equal(
        Item(
          title = "Venice Film Festival Tries to Quit Sinking".some,
          description = "Some of the most heated chatter at the Venice Film Festival this week was about the way that the arrival of the stars at the Palazzo del Cinema was being staged.".some,
          link = "http://nytimes.com/2004/12/07FEST.html".some,
          author = "lawyer@boyer.net (Lawyer Boyer)".some,
          categories = Seq(Category("MSFT", "http://www.fool.com/cusips".some)),
          comments = "http://ekzemplo.com/entry/4403/comments".some,
          enclosure = Enclosure(url = "http://www.scripting.com/mp3s/weatherReportSuite.mp3", length = "12216320", mimeType = "audio/mpeg").some,
          guid = Guid(value = "http://inessential.com/2002/09/01.php#a2", isPermaLink = false).some,
          pubDate = "Sun, 19 May 2002 15:21:36 GMT".toDate("EEE, dd MMM yyyy HH:mm:ss zzz").toOption.get.some,
          source = Source(value = "Tomalak's Realm", url = "http://www.tomalak.org/links2.xml").some
        ).success
      )
    }
  }
}
