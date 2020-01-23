package reader

import scala.xml.{NodeSeq, XML}

case class Guid(isPermaLink: Boolean, guid: String)

case class ItemSource(url: String, source: String)

case class Channelltem(title: String,
                       link: String,
                       description: String,
                       pubDate: String,
                       guid: Guid,
                       source: Option[ItemSource])

case class ChannelImage(url: String,
                        title: String,
                        link: String,
                        width: Option[Int],
                        height: Option[Int],
)

case class Channel(title: String,
                   link: String,
                   description: String,
                   language: String,
                   lastBuildDate: String,
                   ttl: Option[Int],
                   generator: String,
                   copyright: String,
                   webMaster: String,
                   image: Option[ChannelImage],
                   items: Seq[Channelltem])

object RssFeedReader {

  def read(name: String): Channel = {

    val rss = XML.loadFile(name)
    val channel = rss \\ "channel"

    val items = for (item <- channel \ "item") yield {
      Channelltem(
        (item \ "title").text.trim,
        (item \ "link").text.trim,
        (item \ "description").text.trim,
        (item \ "pubDate").text,
        Guid(
          (item \ "guid" \ "@isPermaLink").text.toBoolean,
          (item \ "guid").text,
        ),
        Option(
          ItemSource((item \ "source" \ "@url").text, (item \ "source").text))
      )
    }

    Channel(
      (channel \ "title").text,
      (channel \ "link").text,
      (channel \ "description").text,
      (channel \ "language").text,
      (channel \ "lastBuildDate").text,
      parseOptionalElementAsInt(channel \ "ttl"),
      (channel \ "generator").text,
      (channel \ "copyright").text,
      (channel \ "webMaster").text,
      Option(
        ChannelImage(
          (channel \ "image" \ "url").text,
          (channel \ "image" \ "title").text,
          (channel \ "image" \ "link").text,
          parseOptionalElementAsInt(channel \ "image" \ "width"),
          parseOptionalElementAsInt(channel \ "image" \ "height")
        )),
      items
    )

  }

  def parseOptionalElementAsInt(el: NodeSeq): Option[Int] = {
    val value = try {
      Some(el.text.toInt)
    } catch {
      case _: NumberFormatException => None;
    }

    Option(
      value
        .getOrElse(0)
        .asInstanceOf[Int])
  }
}
