using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text.RegularExpressions;

namespace ExerciseReport
{
    internal class LearningObjectives
    {
        public IBuilder Builder { get; }
        
        
        private Dictionary<string, List<string>> concepts = new Dictionary<string, List<string>>();

        public interface IBuilder
        {
            void Add(string conceptName, string learningObjective);
        }

        private class BuilderImpl : IBuilder
        {
            private LearningObjectives _this;

            public BuilderImpl(LearningObjectives _this)
            {
                this._this = _this;
            }

            public void Add(string conceptName, string learningObjective)
            {
                if (!_this.concepts.ContainsKey(conceptName))
                {
                    _this.concepts[conceptName] = new List<string>();
                }
                _this.concepts[conceptName].Add(learningObjective);
            }
        }

        public LearningObjectives()
        {
            Builder = new BuilderImpl(this);
        }
        public IEnumerable<string>? GetList(string conceptName)
        {
            if (!concepts.ContainsKey(conceptName))
            {
                return null;
            }

            return new ReadOnlyCollection<string>(concepts[conceptName]);
        }
    }
    
    internal class DesignDocParser
    {
        private const string HEADING_TEXT = "headingtext";
        private const string CONCEPT = "concept";
        private const string LEARNING_OBJECTIVE = "learningobjective";

        private Regex headingRegex = new Regex(@$"
            ^\s*\#+\s                    # typically ##
            (?<{HEADING_TEXT}>.*)       # typically Concepts or Prerequisites
            $",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);
        private Regex learningObjectiveRegex = new Regex(@$"^-\s`(?<{CONCEPT}>.*)`\s*:\s*(?<{LEARNING_OBJECTIVE}>.*)$",
            RegexOptions.IgnoreCase | RegexOptions.IgnorePatternWhitespace);

        // we are extracting the learning objectives as associated with each concept
        // not the ones actually called "Learning Objectives".  It is what it is.
        public LearningObjectives ParseDesignDoc(string designDocText)
        {
            var errors = new List<string>();
            var learningObjectives = new LearningObjectives();
            string[] lines = designDocText.Split("\n");
            var learnings = lines
                .SkipWhile(line => !MatchesHeading(line, "Concepts"))
                .TakeWhile(line => !MatchesHeading(line, "*Concepts"))
                .Where(line => line.Length > 1 && line[0] == '-' && char.IsWhiteSpace(line[1]))
                .Select(line => SplitLine(line)).ToList();
            foreach (var learning in learnings)
            {
                switch (learning)
                {
                    case (true, _, string concept, string objective):
                        learningObjectives.Builder.Add(concept, objective);
                        break;
                    case (false, string error, _, _):
                        errors.Add(error);
                        break;
                }
            }

            return learningObjectives;
        }

        // line: e.g. "- `basics`: basic stuff"
        private (bool success, string error, string concept, string objective) SplitLine(string line)
        {
            var match = learningObjectiveRegex.Match(line);
            if (match.Groups.ContainsKey(CONCEPT) && match.Groups.ContainsKey(LEARNING_OBJECTIVE))
            {
                return (true, string.Empty, match.Groups[CONCEPT].Value, match.Groups[LEARNING_OBJECTIVE].Value);
            }
            else
            {
                return (false, $"invalid format: {line}", string.Empty, string.Empty);
            }
        }

        // line: typically "## Concepts" or "## Prerequisites"
        private bool MatchesHeading(string line, string headingText)
        {
            var match = headingRegex.Match(line.Trim());
            return match.Success switch
            {
                false => false,
                true when headingText.StartsWith("*") && match.Groups[HEADING_TEXT].Value != headingText.Substring(1) => true,
                true when match.Groups[HEADING_TEXT].Value == headingText => true,
                _ => false
            };
        }
    }
}