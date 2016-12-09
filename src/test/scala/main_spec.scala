package info.ditrapani.asm

class MainSpec extends Spec {
  describe("main") {
    describe("-t") {
      it("transform a good ascii tile file into a binary one") {
        Main.main(Array("-t", "src/test/resources/built-in.tiles"))
      }
    }
  }
}
