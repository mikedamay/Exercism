using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport
{
    internal class DesignDocParser
    {
        private const string HEADING_TEXT = "headingtext";
        private const string CONCEPT = "concept";
        private const string LEARNING_OBJECTIVE = "learningobjective";

        private readonly Regex headingRegex = new Regex(@$"
            ^\s*\#+\s                    # typically ##
            (?<{HEADING_TEXT}>.*)       # e.g one of the following: Concepts, Prerequisites
            $",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);

        private readonly Regex learningObjectiveRegex = new Regex(@$"^
            -\s                
            `(?<{CONCEPT}>.*)`                # e.g. `string-formatting`
            \s*:\s*                           # :
            (?<{LEARNING_OBJECTIVE}>.*)       # e.g. our string formatting is great
            $",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);

        // we are extracting the learning objectives as associated with each concept
        // not the ones actually called "Learning Objectives".  It is what it is.
        public IEnumerable<(bool success, string error, string concept, string objective)> ParseDesignDoc(
            string designDocPath, string designDocText)
        {
            string docId = GetIdFromPath(designDocPath);
            string[] lines = designDocText.Split("\n");
            var conceptsAndObjectives = lines
                .SkipWhile(line => !MatchesHeading(line, "Concepts"))
                .Skip(1)
                .TakeWhile(line => !MatchesHeading(line))
                .Where(line => line.Length > 1 && line[0] == '-' && char.IsWhiteSpace(line[1]))
                .SelectMany(line => LineToConceptAndObjective(docId, line))
                .DefaultIfEmpty((false, $"{docId}: no learning objectives found", string.Empty, String.Empty));
            return conceptsAndObjectives;
        }

        // designDocPath: typically "./languages/<language>/exercises/concept/<exercise-name>/.meta/design.md"
        // returns: <exercise-naem
        private string GetIdFromPath(string designDocPath)
        {
            var path = Path.GetDirectoryName(designDocPath);
            var parts = path?.Split("/") ?? new string[0];
            return parts.Length > 2 ? parts[^2] : designDocPath;
        }

        // line: e.g. "- `basics`: basic-stuff; other-stuff"
        // => (true, "", basics, basic-stuff)
        // => (true, "", basics, other-stuff)
        private List<(bool success, string error, string concept, string objective)>
            LineToConceptAndObjective(string docId, string line)
        {
            var match = learningObjectiveRegex.Match(line);
            if (match.Success && match.Groups.ContainsKey(CONCEPT) && match.Groups.ContainsKey(LEARNING_OBJECTIVE))
            {
                var results = new List<(bool success, string error, string concept, string objective)>();
                string conceptName = match.Groups[CONCEPT].Value;
                foreach (var learningObjective in match.Groups[LEARNING_OBJECTIVE].Value.Split(';'))
                {
                    results.Add((true, string.Empty, conceptName.Trim(), learningObjective.Trim()));
                }

                return results;
            }
            else
            {
                return new List<(bool success, string error, string concept, string objective)>{(false, $"{docId}: invalid format: {line}", string.Empty, string.Empty)};
            }
        }

        // line: typically "## Concepts" or "## Prerequisites"
        // headingText: specific text to match - "*" == any text
        private bool MatchesHeading(string line, string headingText = "*")
        {
            var match = headingRegex.Match(line.Trim());
            return match.Success switch
            {
                false => false,
                true when headingText == "*" => true,
                true when match.Groups[HEADING_TEXT].Value == headingText => true,
                _ => false
            };
        }
    }
}