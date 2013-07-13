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
import scalaz.{Category => _, _}, Scalaz._
import org.antabaka.yarr.FailMatching


class ChannelSpec extends FunSpec with ShouldMatchers with FailMatching {

  describe("A Category parser") {
    it ("should parse an example with domain") {
      Category.parse(<category domain="http://www.fool.com/cusips">MSFT</category>) should equal(Category("MSFT", "http://www.fool.com/cusips".some).success)
    }

    it ("should parse an example without domain") {
      Category.parse(<category>Grateful Dead</category>) should equal(Category("Grateful Dead").success)
    }
  }

  describe("An Image parser") {
    it ("should parse an example without optional sub-elements") {
      Image.parse(
        <image>
          <link>http://habrahabr.ru/</link>
          <url>http://habrahabr.ru/i/logo.gif</url>
          <title>Хабрахабр</title>
        </image>) should equal(Image(url = "http://habrahabr.ru/i/logo.gif", link = "http://habrahabr.ru/", title = "Хабрахабр").success)
    }

    it ("should parse an example with optional sub-elements") {
      Image.parse(
        <image>
          <link>http://habrahabr.ru/</link>
          <url>http://habrahabr.ru/i/logo.gif</url>
          <title>Хабрахабр</title>
          <width>90</width>
          <height>40</height>
          <description>Image description</description>
        </image>
      ) should equal(
          Image(
            url = "http://habrahabr.ru/i/logo.gif",
            link = "http://habrahabr.ru/",
            title = "Хабрахабр",
            width = 90,
            height = 40,
            description = "Image description".some
          ).success
        )
    }

    it ("should fail on format errors") {
      Image.parse(
        <image>
          <link>http://habrahabr.ru/</link>
          <url>http://habrahabr.ru/i/logo.gif</url>
          <title>Хабрахабр</title>
          <width>-90</width>
          <height>500x</height>
          <description>Image description</description>
          <description>Image description</description>
        </image>) should failN(3)
    }
  }

  describe("A Cloud parser") {
    it ("should parse valid example") {
      Cloud.parse(
          <cloud domain="rpc.sys.com" port="80" path="/RPC2" registerProcedure="myCloud.rssPleaseNotify" protocol="xml-rpc" />
      ) should equal(
        Cloud(
          domain = "rpc.sys.com",
          port = 80,
          path = "/RPC2",
          registerProcedure = "myCloud.rssPleaseNotify",
          protocol = "xml-rpc"
        ).success
      )
    }

    it ("should fail on format error") {
      Cloud.parse(<cloud port="80x" />) should failN(5)
    }
  }

  describe("A TextInput parser") {
    it ("should parse a valid example") {
      TextInput.parse(
        <textInput>
          <title> -- The label of the Submit button in the text input area.</title>
          <description> -- Explains the text input area.</description>
          <name> -- The name of the text object in the text input area.</name>
          <link> -- The URL of the CGI script that processes text input requests.</link>
        </textInput>
      ) should equal(
        TextInput(
          title = " -- The label of the Submit button in the text input area.",
          description = " -- Explains the text input area.",
          name = " -- The name of the text object in the text input area.",
          link = " -- The URL of the CGI script that processes text input requests."
        ).success
      )
    }

    it ("should fail on format error") {
      TextInput.parse(
        <textInput>
          <link> -- The URL of the CGI script that processes text input requests.</link>
          <link> -- The URL of the CGI script that processes text input requests.</link>
        </textInput>
      ) should failN(4)
    }
  }

  describe("A Channel parser") {
    it ("should parse an example without optional sub-elements") {
      Channel.parse(
        <channel>
          <title>Хабрахабр /  / Тематические / Посты</title>
          <link>http://habrahabr.ru/rss/hubs/all/</link>
          <description><![CDATA[ посты из тематических хабов на Хабрахабре]]></description>
        </channel>
      ) should equal(
        Channel(
          title = "Хабрахабр /  / Тематические / Посты",
          link = "http://habrahabr.ru/rss/hubs/all/",
          description = " посты из тематических хабов на Хабрахабре"
        ).success
      )
    }

    it ("should parse an example with optional sub-elements") {
      Channel.parse(
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
        </channel>
      ) should equal(
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
        ).success
      )
    }

    it ("should fail on format error") {
      Channel.parse(
        <channel>
          <title>Хабрахабр /  / Тематические / Посты</title>
          <description><![CDATA[ посты из тематических хабов на Хабрахабре]]></description>
          <language>ru</language>
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
          <category domain="http://www.fool.com/cusips">MSFT</category>
          <docs>http://www.rssboard.org/rss-specification</docs>
          <cloud domain="rpc.sys.com" port="80" path="/RPC2" registerProcedure="pingMe" protocol="soap"/>
          <ttl>60x</ttl>
          <rating>что это вообще?</rating>
          <textInput>
            <title> -- The label of the Submit button in the text input area.</title>
            <description> -- Explains the text input area.</description>
            <name> -- The name of the text object in the text input area.</name>
            <link> -- The URL of the CGI script that processes text input requests.</link>
          </textInput>
          <skipHours>{ (0 to 24).map{ i => <hour>{i}</hour> } }</skipHours>
          <skipDays>{ Seq("x", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday").map{ d => <day>{d}</day> } }</skipDays>
        </channel>
      ) should failN(5)
    }
  }
}
