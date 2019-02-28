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

    private enum Fixture    // football convention is that the home team is mentioned first
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

        Result EvalResult(string[] resultTextParts) 
            => Enum.Parse<Result>(resultTextParts[RESULT_PART]);
        
        TeamAndScores MakeTeamAndScores(string teamName, Fixture venue, Result homeResult) =>
            new TeamAndScores(
                teamName,
                homeResult == Result.draw ? homeResult
                    : homeResult == Result.win && venue == Fixture.Home
                      || homeResult == Result.loss && venue == Fixture.Away
                    ? Result.win : Result.loss 
                );

        var leagueTable = results
            .Split('\n')
            .Select(s => s.Split(';'))
            .Where(s => s.Length == 3)
            .Select(parts =>
                new
                {
                    team1 = new {TeamName = parts[HOME_TEAM], Venue = Fixture.Home, HomeResult = EvalResult(parts)},
                    team2 = new {TeamName = parts[AWAY_TEAM], Venue = Fixture.Away, HomeResult = EvalResult(parts)},
                }).SelectMany(r => new int[] {HOME_TEAM, AWAY_TEAM}, (r, ii) => ii == HOME_TEAM? r.team1 : r.team2)
            .Select(p => MakeTeamAndScores(p.TeamName, p.Venue, p.HomeResult))
            .OrderBy(p => p.TeamName)
            .GroupBy(p => p.TeamName)
            .Select(g => g
                .Select(r => new {r.TeamName, Played = 1, r.Wins, r.Draws, r.Losses, r.Points})
                .Aggregate(new {TeamName = string.Empty, Played = 0, Wins = 0, Draws = 0, Losses = 0, Points = 0}
                 , (curr, acc) => new {acc.TeamName, Played = acc.Played + curr.Played, Wins = acc.Wins + curr.Wins, Draws = acc.Draws + curr.Draws,Losses = acc.Losses + curr.Losses, Points = acc.Points + curr.Points}))
            .OrderByDescending(r => r.Points).ThenBy(r => r.TeamName)
            .Select(r => string.Format(template, r.TeamName, r.Played,r.Wins, r.Draws, r.Losses, r.Points))
            .ToList();
        var writer = new StreamWriter(outStream);
        writer.Write(heading + (leagueTable.Count() >  0 ? "\n" : string.Empty));
        writer.Write(string.Join('\n',leagueTable));
        writer.Flush();
    }
}
