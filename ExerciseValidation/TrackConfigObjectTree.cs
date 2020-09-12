using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace ExerciseValidation
{
    public class OnlineEditor
    {
        [JsonPropertyName("indent_style")]
        public string IndentStyle { get; set; } = String.Empty;
        [JsonPropertyName("indent_size")]
        public int IndentSize { get; set; } = 0;
    }

    public class Exercise
    {
        [JsonPropertyName("slug")]
        public string Slug { get; set; } = string.Empty;
    }
    
    public class Exercises
    {
        [JsonPropertyName("concept")]
        public IList<Exercise> Concept { get; set; } = new List<Exercise>();
        [JsonPropertyName("practice")]
        public IList<Exercise> Practice { get; set; } = new List<Exercise>();
    }
    
    public class TrackConfigObjectTree
    {
        [JsonPropertyName("language")]
        public string Language { get; set; } = string.Empty;
        [JsonPropertyName("slug")]
        public string Slug { get; set; } = string.Empty;
        [JsonPropertyName("active")]
        public bool Active { get; set; } = false;
        [JsonPropertyName("blurb")]
        public string Blurb { get; set; } = string.Empty;
        [JsonPropertyName("version")]
        public decimal Version { get; set; } = decimal.Zero;
        [JsonPropertyName("online_editor")] 
        public OnlineEditor OnlineEditor { get; set; } = null;
    }
}