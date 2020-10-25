package com.evolutiongaming.bootcamp.json

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import io.circe
import io.circe.parser._
import io.circe.generic.JsonCodec
import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scalaj.http.Http

/**
 * HOMEWORK:
 *
 * Some classes and generated JSON codecs are provided for NBA API.
 * Unfortunately, they don't work as expected out of the box.
 * The task is to fix (rewrite) some of the codecs to make tests pass.
 * You are not supposed to change anything in _class_ HomeworkSpec,
 * instead of it you are supposed to change whatever you want inside _companion object_ for HomeworkSpec.
 *
 * It would be nice to avoid using Encoder/Decoder.forProductN where you specify all field names
 */
class HomeworkSpec extends AnyWordSpec with Matchers with EitherValues {
  import HomeworkSpec._

  "NBA JSON API client" should {
    "get info about today games" in {
      val date = LocalDate.now()
      val scoreboardOrError = fetchScoreboard(date)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      val allGameIds = scoreboard.games.map(_.gameId)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      succeed
    }

    "fetch games for 14 Feb 2020" in {
      val date = LocalDate.of(2020, 2, 14)
      val scoreboardOrError = fetchScoreboard(date)
      println(scoreboardOrError)
      val scoreboard = scoreboardOrError.getOrElse(fail(scoreboardOrError.toString))
      println(scoreboard)
      val allGameIds = scoreboard.games.map(_.gameId)
      println("allGameIds: " + allGameIds)
      val gameInfosOrError = allGameIds.map(fetchGameInfo(date, _)).sequence
      println("gameInfosOrError: " + gameInfosOrError)
      val gameInfos = gameInfosOrError.getOrElse(fail(gameInfosOrError.toString))
      println(gameInfos)
      gameInfos.size must be(1)
    }
  }

}

object HomeworkSpec {
  import cats.syntax.either._
  import io.circe.generic.extras._
  import io.circe.generic.extras.semiauto._
  import io.circe.{Decoder, Encoder}
  import io.circe.generic.extras.semiauto.{deriveConfiguredDecoder, deriveConfiguredEncoder}
  import java.time.{LocalDate, ZonedDateTime}

  implicit val config: Configuration = Configuration.default.copy(
    transformMemberNames = {
      case "fullTimeoutRemaining" => "full_timeout_remaining"
      case other => other
    }
  )

  implicit val encodeLocalDate: Encoder[LocalDate] = Encoder.encodeString.contramap[LocalDate](_.toString)
  implicit val decodeLocalDate: Decoder[LocalDate] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(LocalDate.parse(str, DateTimeFormatter.BASIC_ISO_DATE)).leftMap(err => "LocalDate: " + err.getMessage)
  }

  @ConfiguredJsonCodec final case class TeamTotals(assists: String, fullTimeoutRemaining: String, plusMinus: String)
  @JsonCodec final case class TeamBoxScore(totals: TeamTotals)
  @JsonCodec final case class GameStats(hTeam: TeamBoxScore, vTeam: TeamBoxScore)
  @JsonCodec final case class PrevMatchup(gameDate: LocalDate, gameId: String)
  @JsonCodec final case class BoxScore(
    basicGameData: Game,
    previousMatchup: PrevMatchup,
    stats: Option[GameStats],
  )
  @JsonCodec final case class JustScore(score: String)
  @JsonCodec final case class TeamStats(
    linescore: List[JustScore],
    loss: String,
    score: String,
    teamId: String,
    triCode: String
  )
  @JsonCodec final case class GameDuration(hours: String, minutes: String)
  @JsonCodec final case class Arena(
    city: String,
    country: String,
    isDomestic: Boolean,
    name: String,
    stateAbbr: String
  )
  @JsonCodec final case class Game(
    arena: Arena,
    attendance: String,
    endTimeUTC: Option[ZonedDateTime],
    gameDuration: GameDuration,
    gameId: String,
    gameUrlCode: String,
    hTeam: TeamStats,
    isBuzzerBeater: Boolean,
    startTimeUTC: ZonedDateTime,
    vTeam: TeamStats,
  )
  @JsonCodec final case class Scoreboard(games: List[Game], numGames: Int)

  private def fetchScoreboard(date: LocalDate): Either[circe.Error, Scoreboard] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/scoreboard.json").asString.body
    decode[Scoreboard](body)
  }

  private def fetchGameInfo(date: LocalDate, gameId: String): Either[circe.Error, BoxScore] = {
    val dateString = date.format(DateTimeFormatter.BASIC_ISO_DATE)
    val body = Http(s"https://data.nba.net/10s/prod/v1/$dateString/${gameId}_boxscore.json").asString.body
    decode[BoxScore](body)
  }
}
