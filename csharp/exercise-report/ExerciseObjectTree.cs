using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    public enum DocType
    {
        None = 'N',
        Design = 'D',
        Issue = 'I',
        Invalid = 'L'
    }

    public enum Level
    {
        Introductory = 'A',
        Essential = 'B',
        Advanced = 'C',
        None = 'N',
        Invalid = 'L'
    }

    public class Exercise
    {
        [JsonPropertyName("slug")]
        public string Slug { get; set; } = string.Empty;
        [JsonPropertyName("level")]
        public Level Level { get; set; } = Level.Invalid;
        [JsonPropertyName("track-neutral-story")]
        public string TrackNeutralStory { get; set; } = string.Empty;
        [JsonPropertyName("document-type")]
        public DocType DocumentType { get; set; } = DocType.Invalid;
        [JsonPropertyName("document-link")]
        public string DocumentLink { get; set; } = string.Empty;
        [JsonPropertyName("concepts")]
        public IList<Concept> Concepts { get; set; } = new List<Concept>();
    }

    public class Concept
    {
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;
        [JsonPropertyName("track-neutral-concept")]
        public string TrackNeutralConcept { get; set; } = string.Empty;
        [JsonPropertyName("learning-objectives")]
        public IList<string> LearningObjectives { get; set; } = new List<string>();
        [JsonPropertyName("original-concepts")]
        public IList<OriginalConcept> OriginalConcepts { get; set; } = new List<OriginalConcept>();
    }

    public class OriginalConcept
    {
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;
        [JsonPropertyName("line-number")]
        public int LineNumber { get; set; } = 0;
    }
    
    public class ExerciseObjectTree
    {
        [JsonPropertyName("exercises")]
        public IList<Exercise> Exercises { get; set; } = new List<Exercise>(); 
    }
}