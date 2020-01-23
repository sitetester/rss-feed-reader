import org.scalatest.flatspec.AnyFlatSpec
import reader.{Channel, RssFeedReader}

class RssFeedReaderSpec extends AnyFlatSpec {

  it should "match bbc_news input)" in {
    val channel = RssFeedReader.read("src/test/resources/bbc_news.rss")

    // https://stackoverflow.com/questions/33164822/is-it-necessary-to-use-cdata-in-rss-feed-format
    // channel
    assert(channel.title == "BBC News - Home")
    assert(channel.link == "https://www.bbc.co.uk/news/")
    assert(channel.description == "BBC News - Home")
    assert(channel.generator == "RSS for Node")
    assert(channel.lastBuildDate == "Thu, 23 Jan 2020 11:12:57 GMT")
    assert(
      channel.copyright == "Copyright: (C) British Broadcasting Corporation, see http://news.bbc.co.uk/2/hi/help/rss/4498287.stm for terms and conditions of reuse.")
    assert(channel.language == "en-gb")
    assert(channel.ttl.getOrElse(0) == 15)

    // image
    assert(
      channel.image.get.url == "https://news.bbcimg.co.uk/nol/shared/img/bbc_news_120x60.gif")
    assert(channel.image.get.title == "BBC News - Home")
    assert(channel.image.get.link == "https://www.bbc.co.uk/news/")
    assert(channel.image.get.width.get == 999)

    // items
    assert(channel.items.size == 44)

    // items.head
    val firstItem = channel.items.head
    assert(
      firstItem.title ==
        "China coronavirus: Wuhan and Huanggang on lockdown")
    assert(
      firstItem.link ==
        "https://www.bbc.co.uk/news/world-asia-china-51217455")
    assert(
      firstItem.description ==
        "There are more than 500 confirmed cases of the virus, which has also spread abroad.")
    assert(firstItem.guid.isPermaLink)
    assert(
      firstItem.guid.guid ==
        "https://www.bbc.co.uk/news/world-asia-china-51217455")

    assert(firstItem.pubDate == "Thu, 23 Jan 2020 11:05:34 GMT")

    // items.last
    val lastItem = channel.items.last
    assert(
      lastItem.title ==
        "How a boy from Vietnam became a slave on a UK cannabis farm")
    assert(
      lastItem.link ==
        "https://www.bbc.co.uk/news/stories-51176958")
    assert(
      lastItem.description ==
        "Ba was a street child in Ho Chi Minh city. He ended up growing cannabis as a slave in the UK - until he escaped.")
    assert(lastItem.guid.isPermaLink)
    assert(
      lastItem.guid.guid ==
        "https://www.bbc.co.uk/news/stories-51176958")

    assert(lastItem.pubDate == "Tue, 21 Jan 2020 00:33:45 GMT")
  }

  it should "load google_news)" in {
    val channel = RssFeedReader.read("src/test/resources/google_news.rss")

    assert(channel.title == "Top stories - Google News")
    assert(channel.link == "https://news.google.com/?hl=en-US")
    assert(channel.description == "Google News")
    assert(channel.generator == "NFE/5.0")
    assert(channel.lastBuildDate == "Thu, 23 Jan 2020 19:00:47 GMT")
    assert(channel.copyright == "2020 Google Inc.")
    assert(channel.language == "en-US")
    assert(channel.ttl.getOrElse(0) == 0)

    // image
    assert(channel.image.get.url == "")

    // items
    assert(channel.items.size == 38)

    // items.head
    val firstItem = channel.items.head
    assert(
      firstItem.title ==
        "Graham 'congratulated' Schiff on a 'well done day' in impeachment trial, senator says - CNN")
    assert(
      firstItem.link ==
        "https://news.google.com/__i/rss/rd/articles/CBMiX2h0dHBzOi8vd3d3LmNubi5jb20vMjAyMC8wMS8yMy9wb2xpdGljcy9ncmFoYW0tc2NoaWZmLXNlbmF0ZS1pbXBlYWNobWVudC10cmlhbC1jbm50di9pbmRleC5odG1s0gFjaHR0cHM6Ly9hbXAuY25uLmNvbS9jbm4vMjAyMC8wMS8yMy9wb2xpdGljcy9ncmFoYW0tc2NoaWZmLXNlbmF0ZS1pbXBlYWNobWVudC10cmlhbC1jbm50di9pbmRleC5odG1s?oc=5")
    assert(firstItem.description.contains("Trump Senate impeachment"))

    assert(!firstItem.guid.isPermaLink)
    assert(
      firstItem.guid.guid ==
        "52780542825966")

    assert(firstItem.pubDate == "Thu, 23 Jan 2020 18:51:00 GMT")

    // items.last
    val lastItem = channel.items.last
    assert(
      lastItem.title ==
        "North Carolina flu death total passes 40 for 2019-20 season - WXII12 Winston-Salem")
    assert(lastItem.link.contains(
      "CBMiX2h0dHBzOi8vd3d3Lnd4aWkxMi5jb20vYXJ0aWNsZS9ub3J0aC1jYXJvbGluYS1mbHUtZGVhdGgtdG90YWwtcGFzc2VzLTQwLTIwMTktMjAtc2Vhc29uLzMwNjQzNjc00gFjaHR0cHM6Ly93d3cud3hpaTEyLmNvbS9hbXAvYXJ0aWNsZS9ub3J0aC1jYXJvbGluYS1mbHUtZGVhdGgtdG90YWwtcGFzc2VzLTQwLTIwMTktMjAtc2Vhc29uLzMwNjQzNjc0"))
    assert(
      lastItem.description
        .contains("34-year-old dies from flu in Dallas"))
    assert(!lastItem.guid.isPermaLink)
    assert(
      lastItem.guid.guid ==
        "52780567624154")

    assert(lastItem.pubDate == "Thu, 23 Jan 2020 16:26:00 GMT")
    assert(lastItem.source.get.url == "https://www.wxii12.com")
    assert(lastItem.source.get.source == "WXII12 Winston-Salem")

  }

  def checkGoogleNewsChannel(channel: Channel): Unit = {}
}
