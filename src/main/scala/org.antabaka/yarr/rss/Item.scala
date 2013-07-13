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

import org.joda.time.DateTime
import scalaz.{Category => _, _}
import Scalaz._
import scala.xml.{Elem, Node}
import com.weiglewilczek.slf4s.Logging

/** RSS channel item.
  *
  * An item may represent a "story" -- much like a story in a newspaper or magazine; if so its description is a synopsis of the story, and the link points to the full story.
  * An item may also be complete in itself, if so, the description contains the text (entity-encoded HTML is allowed; see examples), and the link and title may be omitted.
  * All elements of an item are optional, however at least one of title or description must be present.
  *
  * @param      title           The title of the item.
  * @param      link            The URL of the item.
  * @param      description     The item synopsis.
  * @param      author          Email address of the author of the item.
  * @param      categories      Includes the item in one or more categories.
  * @param      comments        URL of a page for comments relating to the item.
  * @param      enclosure       Describes a media object that is attached to the item.
  * @param      guid            A string that uniquely identifies the item.
  * @param      pubDate         Indicates when the item was published.
  * @param      source          The RSS channel that the item came from.
  */
case class Item(title: Option[String] = None,
                link: Option[String] = None,
                description: Option[String] = None,
                author: Option[String] = None,
                categories: Seq[Category] = Nil,
                comments: Option[String] = None,
                enclosure: Option[Enclosure] = None,
                guid: Option[Guid] = None,
                pubDate: Option[DateTime] = None,
                source: Option[Source] = None)

object Item extends Logging {
  def parseItems(elem: Node, dateFmt: String): Seq[ValS[Item]] = elem \ "item" map { parse(_, dateFmt) }

  def parse(node: Node, dateFmt: String): ValS[Item] =
    extractors(dateFmt).
      foldLeft(Item().successNel: ValS[Item]){
        (i, e) => e(i, node)
      }.
      flatMap{ item =>
        if (item.title.isEmpty && item.description.isEmpty)
          s"""Item parse error. At least one of title or description must be present. Node: "$node".""".failNel
        else
          item.success
      }

  private[this] def extractors(dateFmt: String): Seq[(ValS[Item], Node) => ValS[Item]] = Seq(
    (i, n) => (i |@| n.optTextNode("title"))                                                  { (i, e) => i.copy(title = e) },
    (i, n) => (i |@| n.optTextNode("link"))                                                   { (i, e) => i.copy(link = e) },
    (i, n) => (i |@| n.optTextNode("description"))                                            { (i, e) => i.copy(description = e) },
    (i, n) => (i |@| n.optTextNode("author"))                                                 { (i, e) => i.copy(author = e) },
    (i, n) => (i |@| (n \ "category").toList.map{Category.parse}.sequence)                    { (i, e) => i.copy(categories = e) },
    (i, n) => (i |@| n.optTextNode("comments"))                                               { (i, e) => i.copy(comments = e) },
    (i, n) => (i |@| n.optNode("enclosure").flatMap{ _.map{Enclosure.parse}.sequenceU })      { (i, e) => i.copy(enclosure = e) },
    (i, n) => (i |@| n.optNode("guid").flatMap{ _.map{Guid.parse}.sequenceU })                { (i, e) => i.copy(guid = e) },
    (i, n) => (i |@| n.optTextNode("pubDate").flatMap{ _.map{_.toDate(dateFmt)}.sequenceU })  { (i, e) => i.copy(pubDate = e) },
    (i, n) => (i |@| n.optNode("source").flatMap{ _.map{Source.parse}.sequenceU })            { (i, e) => i.copy(source = e) }
  )
}

/** The purpose of this element is to propagate credit for links, to publicize the sources of news items.
  *
  * It can be used in the Post command of an aggregator.
  * It should be generated automatically when forwarding an item from an aggregator to a weblog authoring tool.
  *
  * @param      value   name of the RSS channel that the item came from, derived from its <title>
  * @param      url     links to the XMLization of the source
  */
case class Source(value: String, url: String)
object Source {
  def parse(node: Node): ValS[Source] = node.singleTextNode("@url").map{ Source(node.text, _) }
}

/** Media object
  *
  * @param      url             location
  * @param      length          size
  * @param      mimeType        standard MIME type
  *
  */
case class Enclosure(url: String, length: String, mimeType: String)
object Enclosure extends Logging {
  def parse(n: Node): ValS[Enclosure] =
    (
      n.singleTextNode("@url") |@|
      n.singleTextNode("@length") |@|
      n.singleTextNode("@type")
    ) { Enclosure.apply _ }
}

/** Globally unique identifier
  *
  * It's a string that uniquely identifies the item.
  * When present, an aggregator may choose to use this string to determine if an item is new.
  *
  * @param      isPermaLink     If the guid element has an attribute named isPermaLink with a value of true, the reader may assume that it is a permalink to the item, that is, a url that can be opened in a Web browser, that points to the full item described by the <item> element.
  *
  */
case class Guid(value: String, isPermaLink: Boolean = true)
object Guid {
  def parse(n: Node): ValS[Guid] =
    for {
      isPermaLink <- n.optTextNode("@isPermaLink")
    } yield Guid(n.text, isPermaLink.map{_.toLowerCase != "false"}.getOrElse(true))
}
