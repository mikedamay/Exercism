using System.Text.Json;
using System.Text.Json.Serialization;

namespace ExerciseReport
{
    internal class ExerciseJsonParser
    {
        public string ToString(ExerciseObjectTree exerciseObjectTree)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true,
                WriteIndented = true,
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Serialize(exerciseObjectTree, options);
        }

        public ExerciseObjectTree FromString(string sampleJson)
        {
            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };
            options.Converters.Add(new JsonStringEnumConverter(JsonNamingPolicy.CamelCase));
            return JsonSerializer.Deserialize<ExerciseObjectTree>(sampleJson, options);
        }
    }
}