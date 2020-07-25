using System;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace ExerciseReport
{
    internal class TrackNeutralConceptsImporter
    {
        private const string TrackNeutralConceptsDoc = "ExerciseReport.Tests.track_neutral_concepts.md";

        public IDictionary<String, string> ImportTrackNeutralConcepts()
        {
            var trackNeutralConceptsDoc = this.GetType().Assembly.GetManifestResourceStream(TrackNeutralConceptsDoc);
            if (trackNeutralConceptsDoc == null)
            {
                throw new InvalidOperationException(
                    $"ImportTrackNeutralConcepts: Failed to find embedded resource {TrackNeutralConceptsDoc}");
            }

            string trackNeutralConcepts = string.Empty;
            try
            {
                using var doc = trackNeutralConceptsDoc;
                using var reader = new StreamReader(doc);
                trackNeutralConcepts = reader.ReadToEnd();
            }
            catch (Exception ex)
            {
                throw new Exception("ImportOriginalConceptsDoc: weird resource read failure", ex);
            }

            return ImportTrackNeutralConcepts(trackNeutralConcepts);
        }

        public IDictionary<string, string> ImportTrackNeutralConcepts(string trackNeutralConcepts)
        {
            string[] lines = trackNeutralConcepts.Split("\n");
            var trackNeutralConceptsMap = new Dictionary<string, string>();
            Regex regex = new Regex(@"\[(?<key>.+)\]:(?<url>.*)");
            foreach (var line in lines)
            {
                var match = regex.Match(line);
                var key = match.Groups["key"].Value;
                var url = match.Groups["url"].Value;
                trackNeutralConceptsMap[key] = url;
            }

            return trackNeutralConceptsMap;
        }
    }
}