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

class RssPacketSpec extends FunSpec with ShouldMatchers with FailMatching {

  describe("A RssPacket parser") {
    it ("should parse valid example") {
      RssPacket.parce(
        <rss version="2.0">
          <channel>
            <title>Хабрахабр /  / Тематические / Посты</title>
            <link>http://habrahabr.ru/rss/hubs/all/</link>
            <description><![CDATA[ посты из тематических хабов на Хабрахабре]]></description>
            <language>ru</language>
            <managingEditor>editor@habrahabr.ru</managingEditor>
            <generator>habrahabr.ru</generator>
            <pubDate>Sun, 07 Jul 2013 07:38:22 GMT</pubDate>
            <lastBuildDate></lastBuildDate>
            <image>
              <link>http://habrahabr.ru/</link>
              <url>http://habrahabr.ru/i/logo.gif</url>
              <title>Хабрахабр</title>
            </image>

            <copyright>Copyright 2002, Spartanburg Herald-Journal</copyright>
            <webMaster>betty@herald.com (Betty Guernsey)</webMaster>
            <category domain="http://www.fool.com/cusips">MSFT</category>
            <docs>http://www.rssboard.org/rss-specification</docs>
            <cloud domain="rpc.sys.com" port="80" path="/RPC2" registerProcedure="pingMe" protocol="soap"/>
            <ttl>60</ttl>
            <rating>что это вообще?</rating>
            <textInput>
              <title> -- The label of the Submit button in the text input area.</title>
              <description> -- Explains the text input area.</description>
              <name> -- The name of the text object in the text input area.</name>
              <link> -- The URL of the CGI script that processes text input requests.</link>
            </textInput>
            <skipHours>{ (0 to 23).map{ i => <hour>{i}</hour> } }</skipHours>
            <skipDays>{ Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday").map{ d => <day>{d}</day> } }</skipDays>

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
            </item>

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
            </item>

          </channel>
        </rss>,
        "EEE, dd MMM yyyy HH:mm:ss zzz"
      ) should equal(
        RssPacket(
          metadata =
            Channel(
              title = "Хабрахабр /  / Тематические / Посты",
              link = "http://habrahabr.ru/rss/hubs/all/",
              description = " посты из тематических хабов на Хабрахабре",
              language = "ru".some,
              managingEditor = "editor@habrahabr.ru".some,
              generator = "habrahabr.ru".some,
              image = Image(url = "http://habrahabr.ru/i/logo.gif", link = "http://habrahabr.ru/", title = "Хабрахабр").some,
              copyright = "Copyright 2002, Spartanburg Herald-Journal".some,
              webMaster = "betty@herald.com (Betty Guernsey)".some,
              categories = Seq(Category("MSFT", "http://www.fool.com/cusips".some)),
              docs = "http://www.rssboard.org/rss-specification".some,
              cloud = Cloud(domain = "rpc.sys.com", port = 80, path = "/RPC2", registerProcedure = "pingMe", protocol = "soap").some,
              ttl = 60.some,
              rating = "что это вообще?".some,
              textInput = TextInput(
                title = " -- The label of the Submit button in the text input area.",
                description = " -- Explains the text input area.",
                name = " -- The name of the text object in the text input area.",
                link = " -- The URL of the CGI script that processes text input requests."
              ).some,
              skipHours = 0 to 23,
              skipDays = Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday").sorted
            ),
          items =
            Seq(
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
              ),
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
              )
            ),
          pubDate = "Sun, 07 Jul 2013 07:38:22 GMT".toDate("EEE, dd MMM yyyy HH:mm:ss zzz").toOption.get.some,
          lastBuildDate = None
        ).success
      )
    }
  }

}
