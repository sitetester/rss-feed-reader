import org.scalatest.flatspec.AnyFlatSpec
import reader.{Channel, RssFeedReader}

class RssFeedReaderSpec extends AnyFlatSpec {

  it should "load bbc_news)" in {
    val channel = RssFeedReader.read("src/test/resources/bbc_news.rss")

    // https://stackoverflow.com/questions/33164822/is-it-necessary-to-use-cdata-in-rss-feed-format
    // channel
    checkChannel(channel)

    // image
    checkImage(channel)

    // items
    assert(channel.items.size == 44)

    // items.head
    checkFirstItem(channel)

    // items.last
    checkLastItem(channel)
  }

  def checkChannel(channel: Channel): Unit = {

    assert(channel.title == "BBC News - Home")
    assert(channel.link == "https://www.bbc.co.uk/news/")
    assert(channel.description == "BBC News - Home")
    assert(channel.generator == "RSS for Node")
    assert(channel.lastBuildDate == "Thu, 23 Jan 2020 11:12:57 GMT")
    assert(
      channel.copyright == "Copyright: (C) British Broadcasting Corporation, see http://news.bbc.co.uk/2/hi/help/rss/4498287.stm for terms and conditions of reuse.")
    assert(channel.language == "en-gb")
    assert(channel.ttl == 15)
  }

  def checkImage(channel: Channel): Unit = {

    assert(
      channel.image.url == "https://news.bbcimg.co.uk/nol/shared/img/bbc_news_120x60.gif")
    assert(channel.image.title == "BBC News - Home")
    assert(channel.image.link == "https://www.bbc.co.uk/news/")
    assert(channel.image.width.get == 999)
  }

  def checkFirstItem(channel: Channel): Unit = {

    assert(
      channel.items.head.title ==
        "China coronavirus: Wuhan and Huanggang on lockdown")
    assert(
      channel.items.head.link ==
        "https://www.bbc.co.uk/news/world-asia-china-51217455")
    assert(
      channel.items.head.description ==
        "There are more than 500 confirmed cases of the virus, which has also spread abroad.")
    assert(channel.items.head.guid.isPermaLink)
    assert(
      channel.items.head.guid.guid ==
        "https://www.bbc.co.uk/news/world-asia-china-51217455")

    assert(channel.items.head.pubDate == "Thu, 23 Jan 2020 11:05:34 GMT")
  }

  def checkLastItem(channel: Channel): Unit = {

    assert(
      channel.items.last.title ==
        "How a boy from Vietnam became a slave on a UK cannabis farm")
    assert(
      channel.items.last.link ==
        "https://www.bbc.co.uk/news/stories-51176958")
    assert(
      channel.items.last.description ==
        "Ba was a street child in Ho Chi Minh city. He ended up growing cannabis as a slave in the UK - until he escaped.")
    assert(channel.items.last.guid.isPermaLink)
    assert(
      channel.items.last.guid.guid ==
        "https://www.bbc.co.uk/news/stories-51176958")

    assert(channel.items.last.pubDate == "Tue, 21 Jan 2020 00:33:45 GMT")
  }
}
