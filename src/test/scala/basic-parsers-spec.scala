package info.ditrapani.asm

class BasicParsersSpec extends Spec {
  describe("symbol") {
    val good_symbols = List(
      "one",
      "R15",
      "RA",
      "video-cells",
      "START_LOOP"
    )

    for (symbol <- good_symbols) {
      it(s"successfully parses ${symbol}") {
        import fastparse.all._
        val result = P(Start ~/ BasicParsers.symbol ~/ End).parse(symbol)
        @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
        val value = result.asInstanceOf[Parsed.Success[String]].value
        value shouldBe symbol
      }
    }

    val bad_symbols = List(
      "_one",
      "6-shot",
      "R~"
    )

    for (symbol <- bad_symbols) {
      it(s"fails to parse ${symbol}") {
        import fastparse.all._
        val result = P(Start ~/ BasicParsers.symbol ~/ End).parse(symbol)
        result shouldBe a[Parsed.Failure]
      }
    }
  }
}
