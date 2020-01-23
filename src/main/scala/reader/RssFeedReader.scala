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
                   ttl: Int,
                   generator: String,
                   copyright: String,
                   webMaster: String,
                   image: ChannelImage,
                   items: Seq[Channelltem])

object RssFeedReader {

  def read(name: String): Channel = {

    val rss = XML.loadFile(name)
    val channel = rss \\ "channel"

    val items = for (item <- channel \ "item") yield {
      Channelltem(
        (item \ "title").text,
        (item \ "link").text,
        (item \ "description").text,
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
      (channel \ "ttl").text.toInt,
      (channel \ "generator").text,
      (channel \ "copyright").text,
      (channel \ "webMaster").text,
      ChannelImage(
        (channel \ "image" \ "url").text,
        (channel \ "image" \ "title").text,
        (channel \ "image" \ "link").text,
        Option(
          parseOptionalElement("width", channel)
            .getOrElse(0)
            .asInstanceOf[Int]),
        Option(
          parseOptionalElement("height", channel)
            .getOrElse(0)
            .asInstanceOf[Int])
      ),
      items
    )

  }

  def parseOptionalElement(el: String, channel: NodeSeq): Option[Any] = {
    try {
      Some((channel \ "image" \ el).text.toInt)
    } catch {
      case _: NumberFormatException => None;
    }
  }
}
