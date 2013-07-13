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
import xml.{Elem, Node}

/** Information about the [[http://www.rssboard.org/rss-specification#whatIsRss RSS]] channel (metadata).
  *
  * @param    title           The name of the channel. It's how people refer to your service. If you have an HTML website that contains the same information as your RSS file, the title of your channel should be the same as the title of your website. Example: ''GoUpstate.com News Headlines''
  * @param    link            The URL to the HTML website corresponding to the channel. Example: ''http://www.goupstate.com/''
  * @param    description     Phrase or sentence describing the channel. Example: ''The latest news from GoUpstate.com, a Spartanburg Herald-Journal Web site.''
  *
  * @param    language        The language the channel is written in. This allows aggregators to group all Italian language sites, for example, on a single page. A list of allowable values for this element, as provided by Netscape, is [[http://www.rssboard.org/rss-language-codes here]]. You may also use [[http://www.w3.org/TR/REC-html40/struct/dirlang.html#langcodes values defined]] by the W3C. Example: ''en-us''
  * @param    copyright       Copyright notice for content in the channel. Example: ''Copyright 2002, Spartanburg Herald-Journal''
  * @param    managingEditor  Email address for person responsible for editorial content. Example: ''geo@herald.com (George Matesky)''
  * @param    webMaster       Email address for person responsible for technical issues relating to channel. Example: ''betty@herald.com (Betty Guernsey)''
  * @param    categories      Specify categories that the channel belongs to. Follows the same rules as the [[org.antabaka.yarr.rss.Item# c a t e g o r i e s]].
  * @param    generator       A string indicating the program used to generate the channel. Example: ''MightyInHouse Content System v2.3''
  * @param    docs            A URL that points to the [[http://www.rssboard.org/rss-specification documentation]] for the format used in the RSS file. It's probably a pointer to this page. It's for people who might stumble across an RSS file on a Web server 25 years from now and wonder what it is. Example: ''http://www.rssboard.org/rss-specification''
  * @param    cloud           Allows processes to register with a cloud to be notified of updates to the channel, implementing a lightweight publish-subscribe protocol for RSS feeds.
  * @param    ttl             ttl stands for time to live. It's a number of minutes that indicates how long a channel can be cached before refreshing from the source.
  * @param    image           Specifies a GIF, JPEG or PNG image that can be displayed with the channel.
  * @param    rating          The [[http://www.w3.org/PICS/ PICS]] rating for the channel.
  * @param    textInput       Specifies a text input box that can be displayed with the channel.
  * @param    skipHours       A hint for aggregators telling them which hours they can skip. It contains up to 24 <hour> sub-elements whose value is a number between 0 and 23, representing a time in GMT, when aggregators, if they support the feature, may not read the channel on hours listed in the <skipHours> element. The hour beginning at midnight is hour zero.
  * @param    skipDays        A hint for aggregators telling them which days they can skip. This element contains up to seven <day> sub-elements whose value is Monday, Tuesday, Wednesday, Thursday, Friday, Saturday or Sunday. Aggregators may not read the channel during days listed in the <skipDays> element.
  */
case class Channel(title: String,
                   link: String,
                   description: String,

                   language: Option[String] = None,
                   copyright: Option[String] = None,
                   managingEditor: Option[String] = None,
                   webMaster: Option[String] = None,
                   categories: Seq[Category] = Nil,
                   generator: Option[String] = None,
                   docs: Option[String] = None,
                   cloud: Option[Cloud] = None,
                   ttl: Option[Int] = None,
                   image: Option[Image] = None,
                   rating: Option[String] = None,
                   textInput: Option[TextInput] = None,
                   skipHours: Seq[Int] = Nil,
                   skipDays: Seq[String] = Nil)

object Channel {
  def parse(n: Node): ValS[Channel] = {
    val res = (
        n.singleTextNode("title") |@|
        n.singleTextNode("link") |@|
        n.singleTextNode("description")
      ) { (t, l, d) => Channel(t, l, d) }

    extractors.foldLeft(res){ (c, e) => e(c, n) }
  }


  private[this] val extractors: Seq[(ValS[Channel], Node) => ValS[Channel]] = Seq(
    (c, n) => (c |@| n.optTextNode("language"))                                           { (c, e) => c.copy(language = e) },
    (c, n) => (c |@| n.optTextNode("copyright"))                                          { (c, e) => c.copy(copyright = e) },
    (c, n) => (c |@| n.optTextNode("managingEditor"))                                     { (c, e) => c.copy(managingEditor = e) },
    (c, n) => (c |@| n.optTextNode("webMaster"))                                          { (c, e) => c.copy(webMaster = e) },
    (c, n) => (c |@| (n \ "category").toList.map{Category.parse}.sequence)                { (c, e) => c.copy(categories = e ) },
    (c, n) => (c |@| n.optTextNode("generator"))                                          { (c, e) => c.copy(generator = e) },
    (c, n) => (c |@| n.optTextNode("docs"))                                               { (c, e) => c.copy(docs = e) },
    (c, n) => (c |@| n.optNode("cloud").flatMap{ _.map(Cloud.parse).sequenceU })          { (c, e) => c.copy(cloud = e) },
    (c, n) => (c |@| n.optIntNode("ttl"))                                                 { (c, e) => c.copy(ttl = e) },
    (c, n) => (c |@| n.optNode("image").flatMap{ _.map(Image.parse).sequenceU })          { (c, e) => c.copy(image = e) },
    (c, n) => (c |@| n.optTextNode("rating"))                                             { (c, e) => c.copy(rating = e) },
    (c, n) => (c |@| n.optNode("textInput").flatMap{ _.map(TextInput.parse).sequenceU })  { (c, e) => c.copy(textInput = e) },
    (c, n) => (c |@| getSkipHours(n))                                                     { (c, e) => c.copy(skipHours = e) },
    (c, n) => (c |@| getSkipDays(n))                                                      { (c, e) => c.copy(skipDays = e) }
  )

  private[this] def getSkipHours(n: Node): ValS[Seq[Int]] =
    for{
      node <- n.optNode("skipHours")
      hours <- node.toList.flatMap { _ \ "hour" }.map{ _.text.toIntVal }.sequence
      validSizeHours <- if (hours.length > 24) s"Too many <hour> elements: $hours.".failNel else hours.success
      validHours <- if (validSizeHours.exists{ h => h > 23 || h < 0 }) s"skipHours contains wrong hours: $validSizeHours.".failNel else validSizeHours.success
    } yield validHours


  private[this] val validDays = Set("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  private[this] def getSkipDays(n: Node): ValS[Seq[String]] =
    for{
      node <- n.optNode("skipDays")
      days = node.toList.flatMap { _ \ "day" }.map{ _.text }
      knownDays <- if (days.exists{ !validDays(_) }) s"skipDays contains wrong days: $days.".failNel else days.success
    } yield knownDays.sorted.distinct
}

/** An optional sub-element of [[org.antabaka.yarr.rss.Channel]]
  *
  * @param      url             the URL of a GIF, JPEG or PNG image that represents the channel.
  * @param      title           describes the image, it's used in the ALT attribute of the HTML <img> tag when the channel is rendered in HTML.
  * @param      link            the URL of the site, when the channel is rendered, the image is a link to the site. (Note, in practice the image title and link should have the same value as the channel's [[org.antabaka.yarr.rss.Channel# t i t l e]] and [[org.antabaka.yarr.rss.Channel# l i n k]].
  *
  * @param      width           indicating the width of the image in pixels. Maximum value for width is 144, default value is 88.
  * @param      height          indicating the height of the image in pixels. Maximum value for height is 400, default value is 31.
  * @param      description     contains text that is included in the TITLE attribute of the link formed around the image in the HTML rendering.
  */
case class Image(url: String,
                 title: String,
                 link: String,

                 width: Int = 88,
                 height: Int = 31,
                 description: Option[String] = None) {
  require(width <= 144, s"Maximum value for width is 144, $width found")
  require(height <= 400, s"Maximum value for height is 400, $height found")
  require(width >= 0, s"width ($width) >= 0")
  require(height >= 0, s"height ($height) >= 0")
}
object Image {
  def parse(n: Node): ValS[Image] =
    (
      n.singleTextNode("url") |@|
      n.singleTextNode("title") |@|
      n.singleTextNode("link") |@|
      n.optIntNode("width").map{ _.getOrElse(88)}.flatMap{ i => if ( i < 0 || i > 144 ) s"Maximum value for width is 144, minimum - 0, found - $i".failNel else i.success } |@|
      n.optIntNode("height").map{ _.getOrElse(31)}.flatMap{ i => if ( i < 0 || i > 400 ) s"Maximum value for height is 400, minimum - 0, found - $i".failNel else i.success } |@|
      n.optTextNode("description")
    ) { Image.apply _ }
}

/**
 * @param      value   forward-slash-separated string that identifies a hierarchic location in the indicated taxonomy. Processors may establish conventions for the interpretation of categories.
 * @param      domain  categorization taxonomy
 */
case class Category(value: String, domain: Option[String] = None)
object Category{
  def parse(n: Node): ValS[Category] = n.optTextNode("@domain").map{ Category(n.text, _) }
}


/** Specifies a text input box that can be displayed with the channel
  *
  * The purpose of the <textInput> element is something of a mystery. You can use it to specify a search engine box. Or to allow a reader to provide feedback. Most aggregators ignore it.
  *
  * @param      title           The label of the Submit button in the text input area.
  * @param      description     Explains the text input area.
  * @param      name            The name of the text object in the text input area.
  * @param      link            The URL of the CGI script that processes text input requests.
  */
case class TextInput(title: String, description: String, name: String, link: String)
object TextInput {
  def parse(n: Node): ValS[TextInput] =
    (
      n.singleTextNode("title") |@|
      n.singleTextNode("description") |@|
      n.singleTextNode("name") |@|
      n.singleTextNode("link")
    ) { TextInput.apply _ }
}

/** Web service that supports the rssCloud interface which can be implemented in HTTP-POST, XML-RPC or SOAP 1.1.
  *
  * Its purpose is to allow processes to register with a cloud to be notified of updates to the channel, implementing a lightweight publish-subscribe protocol for RSS feeds.
  *
  * Example:
  * ''<cloud domain="rpc.sys.com" port="80" path="/RPC2" registerProcedure="myCloud.rssPleaseNotify" protocol="xml-rpc" />''
  *
  * A full explanation of this element and the rssCloud interface is [[http://www.rssboard.org/rsscloud-interface here]].
  */
case class Cloud(domain: String,
                 port: Int,
                 path: String,
                 registerProcedure: String,
                 protocol: String)
object Cloud {
  def parse(n: Node): ValS[Cloud] =
    (
      n.singleTextNode("@domain") |@|
      n.singleIntNode("@port") |@|
      n.singleTextNode("@path") |@|
      n.singleTextNode("@registerProcedure") |@|
      n.singleTextNode("@protocol")
    ) { Cloud.apply _ }
}

//DateTimeFormat.forPattern("EEE, dd MMM yyyy HH:mm:ss zzz").withLocale(java.util.Locale.forLanguageTag("en")).parseDateTime("Mon, 13 May 2013 17:36:43 GMT")
                
