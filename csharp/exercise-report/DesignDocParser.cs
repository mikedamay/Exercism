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
                .Select(line => LineToConceptAndObjective(docId, line))
                .DefaultIfEmpty((false, $"{docId}: no learning objectives found", string.Empty, String.Empty));
            return conceptsAndObjectives;
        }

        // designDocPath: typically "./languages/<language>/exercises/concept/<exercise-name>/.meta/design.md"
        // returns: <exercise-naem
        private string GetIdFromPath(string designDocPath)
        {
            var path = Path.GetDirectoryName(designDocPath);
            var parts = path.Split("/");
            return parts.Length > 2 ? parts[^2] : designDocPath;
        }

        // line: e.g. "- `basics`: basic stuff"
        private (bool success, string error, string concept, string objective)
            LineToConceptAndObjective(string docId, string line)
        {
            var match = learningObjectiveRegex.Match(line);
            if (match.Success && match.Groups.ContainsKey(CONCEPT) && match.Groups.ContainsKey(LEARNING_OBJECTIVE))
            {
                return (true, string.Empty, match.Groups[CONCEPT].Value, match.Groups[LEARNING_OBJECTIVE].Value);
            }
            else
            {
                return (false, $"{docId}: invalid format: {line}", string.Empty, string.Empty);
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