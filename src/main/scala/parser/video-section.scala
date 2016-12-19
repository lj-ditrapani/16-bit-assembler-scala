package info.ditrapani.asm.parser.video

object VideoSection {
  import fastparse.all._
  import info.ditrapani.asm.parser.BasicParsers._

  val video_section = P(
    ".video-rom\n" ~/ noise ~/ ".end-video-rom" ~/ tail_noise
  )
}
