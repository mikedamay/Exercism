using System;
using System.IO;
using System.Linq;

public static class Tournament
{
    private enum Result
    {
        win,
        loss,
        draw
    }

    private enum Fixture // football convention is that the home team is mentioned first
    {
        Home,
        Away
    }

    private const string heading = "Team                           | MP |  W |  D |  L |  P";
    private const string template = "{0,-31}|{1,3} |{2,3} |{3,3} |{4,3} |{5,3}";

    private struct TeamAndScores
    {
        public string TeamName { get; }
        public int Wins => result == Result.win ? 1 : 0;
        public int Draws => result == Result.draw ? 1 : 0;
        public int Losses => result == Result.loss ? 1 : 0;
        public int Points => result == Result.win ? 3 : result == Result.draw ? 1 : 0;

        public TeamAndScores(string teamName, Result result)
        {
            TeamName = teamName;
            this.result = result;
        }

        private readonly Result result;
    }

    public static void Tally(Stream inStream, Stream outStream)
    {
        const byte HOME_TEAM = 0;
        const byte AWAY_TEAM = 1;
        const byte RESULT_PART = 2;
        var results = new StreamReader(inStream).ReadToEnd();

        Result ExtractResult(string[] resultTextParts)
            => Enum.Parse<Result>(resultTextParts[RESULT_PART]);

        TeamAndScores MakeTeamAndScores(string teamName, Fixture venue, Result homeResult) =>
            new TeamAndScores(
                teamName,
                homeResult == Result.draw
                    ? homeResult
                    : homeResult == Result.win && venue == Fixture.Home
                      || homeResult == Result.loss && venue == Fixture.Away
                        ? Result.win
                        : Result.loss
            );

        var leagueTable = results
            .Split('\n')
            .Select(s => s.Split(';'))
            .Where(s => s.Length == RESULT_PART + 1)
            .Select(parts =>
                new
                {
                    team1 = new {TeamName = parts[HOME_TEAM], Venue = Fixture.Home, HomeResult = ExtractResult(parts)},
                    team2 = new {TeamName = parts[AWAY_TEAM], Venue = Fixture.Away, HomeResult = ExtractResult(parts)},
                })
            .SelectMany(_ => new int[] {HOME_TEAM, AWAY_TEAM},
                (match, homeOrAway) => homeOrAway == HOME_TEAM ? match.team1 : match.team2)
            .Select(team => MakeTeamAndScores(team.TeamName, team.Venue, team.HomeResult))
            .OrderBy(team => team.TeamName)
            .GroupBy(team => team.TeamName)
            .Select(teamResults => teamResults
                .Select(teamResult => new
                {
                    teamResult.TeamName, Played = 1, teamResult.Wins, teamResult.Draws, teamResult.Losses,
                    teamResult.Points
                })
                .Aggregate(new {TeamName = string.Empty, Played = 0, Wins = 0, Draws = 0, Losses = 0, Points = 0}
                    , (curr, acc) => new {acc.TeamName, Played = acc.Played + curr.Played, Wins = acc.Wins + curr.Wins, Draws = acc.Draws + curr.Draws, Losses = acc.Losses + curr.Losses, Points = acc.Points + curr.Points}))
            .OrderByDescending(teamTotalScore => teamTotalScore.Points)
            .ThenBy(teamTotalScore => teamTotalScore.TeamName)
            .Select(teamTotalScore => string.Format(template, teamTotalScore.TeamName,
                teamTotalScore.Played, teamTotalScore.Wins, teamTotalScore.Draws,
                teamTotalScore.Losses, teamTotalScore.Points))
            .ToList();
        
        var writer = new StreamWriter(outStream);
        writer.Write(heading + (leagueTable.Count() > 0 ? "\n" : string.Empty));
        writer.Write(string.Join('\n', leagueTable));
        writer.Flush();
    }
}