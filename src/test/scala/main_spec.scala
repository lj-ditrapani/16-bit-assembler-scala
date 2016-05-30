package info.ditrapani.tilecreator

class MainSpec extends Spec {
  describe("Adder") {
    describe("add") {
      it("makes new adder with summed value") {
        Adder(7).add(3).base should === (10)
      }
    }
  }
}
